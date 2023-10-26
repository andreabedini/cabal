{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Client.IndexUtils
-- Copyright   :  (c) Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  duncan@community.haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Extra utils related to the package indexes.
module Distribution.Client.IndexUtils
  ( getIndexFileAge
  , getInstalledPackages
  , Configure.getInstalledPackagesMonitorFiles
  , getSourcePackages
  , getSourcePackagesMonitorFiles
  , TotalIndexState
  , getSourcePackagesAtIndexState
  , ActiveRepos
  , filterSkippedActiveRepos
  , Index (..)
  , RepoIndexState (..)
  , updateRepoIndexCache
  , updatePackageIndexCacheFile
  , writeIndexTimestamp
  , currentIndexTimestamp
  , refTypeFromTypeCode
  , typeCodeFromRefType
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Archive.Tar.Index as Tar
import Distribution.Client.IndexUtils.ActiveRepos
import Distribution.Client.IndexUtils.IndexState
import Distribution.Client.IndexUtils.Timestamp
import qualified Distribution.Client.Tar as Tar
import Distribution.Client.Types
import Distribution.Parsec (simpleParsecBS)
import Distribution.Verbosity

import Distribution.Client.Setup
  ( RepoContext (..)
  )
import Distribution.Package
  ( Package (..)
  , PackageId
  , PackageIdentifier (..)
  , mkPackageName
  , packageName
  , packageVersion
  )
import Distribution.PackageDescription
  ( GenericPackageDescription (..)
  , PackageDescription (..)
  , emptyPackageDescription
  )
import Distribution.Simple.Compiler
  ( Compiler
  , PackageDBStack
  )
import qualified Distribution.Simple.Configure as Configure
  ( getInstalledPackages
  , getInstalledPackagesMonitorFiles
  )
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import Distribution.Simple.Program
  ( ProgramDb
  )
import Distribution.Simple.Utils
  ( createDirectoryIfMissingVerbose
  , die'
  , dieWithException
  , fromUTF8LBS
  , info
  , warn
  )
import Distribution.Types.Dependency
import Distribution.Types.PackageName (PackageName)
import Distribution.Version
  ( Version
  , VersionRange
  , intersectVersionRanges
  , mkVersion
  )

import Distribution.PackageDescription.Parsec
  ( parseGenericPackageDescription
  , parseGenericPackageDescriptionMaybe
  )
import qualified Distribution.PackageDescription.Parsec as PackageDesc.Parse
import qualified Distribution.Simple.PackageDescription as PackageDesc.Parse

import Distribution.Solver.Types.PackageIndex (PackageIndex)
import qualified Distribution.Solver.Types.PackageIndex as PackageIndex
import Distribution.Solver.Types.SourcePackage

import qualified Codec.Compression.GZip as GZip
import Control.Exception
import qualified Data.ByteString.Char8 as BSS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Either
  ( rights
  )
import Data.List (stripPrefix)
import qualified Data.Map as Map
import qualified Data.Set as Se
import Distribution.Client.GZipUtils (maybeDecompress)
import Distribution.Client.Utils
  ( byteStringToFilePath
  , tryFindAddSourcePackageDesc
  )
import Distribution.Compat.Directory (listDirectory)
import Distribution.Compat.Time (getFileAge, getModTime)
import Distribution.Utils.Generic (fstOf3)
import Distribution.Utils.Structured (Structured (..), nominalStructure, structuredDecodeFileOrFail, structuredEncodeFile)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath
  ( normalise
  , splitDirectories
  , takeDirectory
  , takeExtension
  , takeFileName
  , (<.>)
  , (</>)
  )
import qualified System.FilePath.Posix as FilePath.Posix
import System.IO
import System.IO.Error (isDoesNotExistError)
import System.IO.Unsafe (unsafeInterleaveIO)

import Distribution.Client.Errors
import Distribution.Client.Repository (Located (..), Repo (..), Repository (..), indexBaseName)
import Distribution.Client.Repository.Cache
import Distribution.Client.Repository.IndexCache (PackageOrDep (..), writeIndexCache)
import qualified Hackage.Security.Client as Sec
import qualified Hackage.Security.Util.Some as Sec

-- | Reduced-verbosity version of 'Configure.getInstalledPackages'
getInstalledPackages
  :: Verbosity
  -> Compiler
  -> PackageDBStack
  -> ProgramDb
  -> IO InstalledPackageIndex
getInstalledPackages verbosity comp packageDbs progdb =
  Configure.getInstalledPackages verbosity' comp packageDbs progdb
  where
    verbosity' = lessVerbose verbosity

------------------------------------------------------------------------
-- Reading the source package index
--

-- Note: 'data IndexState' is defined in
-- "Distribution.Client.IndexUtils.Timestamp" to avoid import cycles

-- | 'IndexStateInfo' contains meta-information about the resulting
-- filtered 'Cache' 'after applying 'filterCache' according to a
-- requested 'IndexState'.
data IndexStateInfo = IndexStateInfo
  { isiMaxTime :: !Timestamp
  -- ^ 'Timestamp' of maximum/latest 'Timestamp' in the current
  -- filtered view of the cache.
  --
  -- The following property holds
  --
  -- > filterCache (IndexState (isiMaxTime isi)) cache == (cache, isi)
  , isiHeadTime :: !Timestamp
  -- ^ 'Timestamp' equivalent to 'IndexStateHead', i.e. the latest
  -- known 'Timestamp'; 'isiHeadTime' is always greater or equal to
  -- 'isiMaxTime'.
  }

emptyStateInfo :: IndexStateInfo
emptyStateInfo = IndexStateInfo nullTimestamp nullTimestamp

-- | Filters a 'Cache' according to an 'IndexState'
-- specification. Also returns 'IndexStateInfo' describing the
-- resulting index cache.
--
-- Note: 'filterCache' is idempotent in the 'Cache' value
filterCache :: RepoIndexState -> Cache -> (Cache, IndexStateInfo)
filterCache IndexStateHead cache = (cache, IndexStateInfo{..})
  where
    isiMaxTime = cacheHeadTs cache
    isiHeadTime = cacheHeadTs cache
filterCache (IndexStateTime ts0) cache0 = (cache, IndexStateInfo{..})
  where
    cache = Cache{cacheEntries = ents, cacheHeadTs = isiMaxTime}
    isiHeadTime = cacheHeadTs cache0
    isiMaxTime = maximumTimestamp (map cacheEntryTimestamp ents)
    ents = filter ((<= ts0) . cacheEntryTimestamp) (cacheEntries cache0)

-- | Read a repository index from disk, from the local files specified by
-- a list of 'Repo's.
--
-- All the 'SourcePackage's are marked as having come from the appropriate
-- 'Repo'.
--
-- This is a higher level wrapper used internally in cabal-install.
getSourcePackages :: Verbosity -> RepoContext -> IO SourcePackageDb
getSourcePackages verbosity repoCtxt =
  fstOf3 <$> getSourcePackagesAtIndexState verbosity repoCtxt Nothing Nothing

-- | Variant of 'getSourcePackages' which allows getting the source
-- packages at a particular 'IndexState'.
--
-- Current choices are either the latest (aka HEAD), or the index as
-- it was at a particular time.
--
-- Returns also the total index where repositories'
-- RepoIndexState's are not HEAD. This is used in v2-freeze.
getSourcePackagesAtIndexState
  :: Verbosity
  -> RepoContext
  -> Maybe TotalIndexState
  -> Maybe ActiveRepos
  -> IO (SourcePackageDb, TotalIndexState, ActiveRepos)
getSourcePackagesAtIndexState verbosity repoCtxt _ _
  | null (repoContextRepos repoCtxt) = do
      -- In the test suite, we routinely don't have any remote package
      -- servers, so don't bleat about it
      warn (verboseUnmarkOutput verbosity) $
        "No remote package servers have been specified. Usually "
          ++ "you would have one specified in the config file."
      return
        ( SourcePackageDb
            { packageIndex = mempty
            , packagePreferences = mempty
            }
        , headTotalIndexState
        , ActiveRepos []
        )
getSourcePackagesAtIndexState verbosity repoCtxt mb_idxState mb_activeRepos = do
  let describeState IndexStateHead = "most recent state"
      describeState (IndexStateTime time) = "historical state as of " ++ prettyShow time

  pkgss <- for (repoContextRepos repoCtxt) $ \(r :: Repo) -> do
    let rname :: RepoName
        rname = repositoryName r

    info verbosity ("Reading available packages of " ++ unRepoName rname ++ "...")

    idxState <- case mb_idxState of
      Just totalIdxState -> do
        let idxState = lookupIndexState rname totalIdxState
        info verbosity $
          "Using "
            ++ describeState idxState
            ++ " as explicitly requested (via command line / project configuration)"
        return idxState
      Nothing -> do
        mb_idxState' <- readIndexTimestamp verbosity (RepoIndex repoCtxt r)
        case mb_idxState' of
          Nothing -> do
            info verbosity "Using most recent state (could not read timestamp file)"
            return IndexStateHead
          Just idxState -> do
            info verbosity $
              "Using "
                ++ describeState idxState
                ++ " specified from most recent cabal update"
            return idxState

    unless (idxState == IndexStateHead) $
      case r of
        RepoLocalNoIndex{} -> warn verbosity "index-state ignored for file+noindex repositories"
        RepoLegacy{} -> warn verbosity ("index-state ignored for old-format (remote repository '" ++ unRepoName rname ++ "')")
        RepoSecure{} -> pure ()

    let idxState' = case r of
          RepoSecure{} -> idxState
          _ -> IndexStateHead

    (pis, deps, isi) <- readRepoIndex verbosity repoCtxt r idxState'

    case idxState' of
      IndexStateHead -> do
        info verbosity ("index-state(" ++ unRepoName rname ++ ") = " ++ prettyShow (isiHeadTime isi))
        return ()
      IndexStateTime ts0 -> do
        when (isiMaxTime isi /= ts0) $
          if ts0 > isiMaxTime isi
            then
              warn verbosity $
                "Requested index-state "
                  ++ prettyShow ts0
                  ++ " is newer than '"
                  ++ unRepoName rname
                  ++ "'!"
                  ++ " Falling back to older state ("
                  ++ prettyShow (isiMaxTime isi)
                  ++ ")."
            else
              info verbosity $
                "Requested index-state "
                  ++ prettyShow ts0
                  ++ " does not exist in '"
                  ++ unRepoName rname
                  ++ "'!"
                  ++ " Falling back to older state ("
                  ++ prettyShow (isiMaxTime isi)
                  ++ ")."
        info
          verbosity
          ( "index-state("
              ++ unRepoName rname
              ++ ") = "
              ++ prettyShow (isiMaxTime isi)
              ++ " (HEAD = "
              ++ prettyShow (isiHeadTime isi)
              ++ ")"
          )

    pure
      RepoData
        { rdRepoName = rname
        , rdTimeStamp = isiMaxTime isi
        , rdIndex = pis
        , rdPreferences = deps
        }

  let activeRepos :: ActiveRepos
      activeRepos = fromMaybe defaultActiveRepos mb_activeRepos

  pkgss' <- case organizeByRepos activeRepos rdRepoName pkgss of
    Right x -> return x
    Left err -> warn verbosity err >> return (map (\x -> (x, CombineStrategyMerge)) pkgss)

  let activeRepos' :: ActiveRepos
      activeRepos' =
        ActiveRepos
          [ ActiveRepo (rdRepoName rd) strategy
          | (rd, strategy) <- pkgss'
          ]

  let totalIndexState :: TotalIndexState
      totalIndexState =
        makeTotalIndexState IndexStateHead $
          Map.fromList
            [ (n, IndexStateTime ts)
            | (RepoData n ts _idx _prefs, _strategy) <- pkgss'
            , -- e.g. file+noindex have nullTimestamp as their timestamp
            ts /= nullTimestamp
            ]

  let addIndex
        :: PackageIndex UnresolvedSourcePackage
        -> (RepoData, CombineStrategy)
        -> PackageIndex UnresolvedSourcePackage
      addIndex acc (RepoData _ _ _ _, CombineStrategySkip) = acc
      addIndex acc (RepoData _ _ idx _, CombineStrategyMerge) = PackageIndex.merge acc idx
      addIndex acc (RepoData _ _ idx _, CombineStrategyOverride) = PackageIndex.override acc idx

  let pkgs :: PackageIndex UnresolvedSourcePackage
      pkgs = foldl' addIndex mempty pkgss'

  -- Note: preferences combined without using CombineStrategy
  let prefs :: Map PackageName VersionRange
      prefs =
        Map.fromListWith
          intersectVersionRanges
          [ (name, range)
          | (RepoData _n _ts _idx prefs', _strategy) <- pkgss'
          , Dependency name range _ <- prefs'
          ]

  _ <- evaluate pkgs
  _ <- evaluate prefs
  _ <- evaluate totalIndexState
  return
    ( SourcePackageDb
        { packageIndex = pkgs
        , packagePreferences = prefs
        }
    , totalIndexState
    , activeRepos'
    )

-- auxiliary data used in getSourcePackagesAtIndexState
data RepoData = RepoData
  { rdRepoName :: RepoName
  , rdTimeStamp :: Timestamp
  , rdIndex :: PackageIndex UnresolvedSourcePackage
  , rdPreferences :: [Dependency]
  }

-- | Read a repository index from disk, from the local file specified by
-- the 'Repo'.
--
-- All the 'SourcePackage's are marked as having come from the given 'Repo'.
--
-- This is a higher level wrapper used internally in cabal-install.
readRepoIndex
  :: Verbosity
  -> RepoContext
  -> Repo
  -> RepoIndexState
  -> IO (PackageIndex UnresolvedSourcePackage, [Dependency], IndexStateInfo)
readRepoIndex verbosity repoCtxt repo idxState =
  handleNotFound $ do
    when (_isRepoLegacy repo) $ warnIfIndexIsOld =<< getIndexFileAge repo
    -- note that if this step fails due to a bad repo cache, the the procedure can still succeed by reading from the existing cache, which is updated regardless.
    updateRepoIndexCache verbosity (RepoIndex repoCtxt repo)
      `catchIO` (\e -> warn verbosity $ "unable to update the repo index cache -- " ++ displayException e)
    readPackageIndexCacheFile
      verbosity
      mkAvailablePackage
      (RepoIndex repoCtxt repo)
      idxState
  where
    mkAvailablePackage pkgEntry =
      SourcePackage
        { srcpkgPackageId = pkgid
        , srcpkgDescription = pkgdesc
        , srcpkgSource = case pkgEntry of
            NormalPackage _ _ _ _ -> RepoTarballPackage repo pkgid Nothing
            BuildTreeRef _ _ _ path _ -> LocalUnpackedPackage path
        , srcpkgDescrOverride = case pkgEntry of
            NormalPackage _ _ pkgtxt _ -> Just pkgtxt
            _ -> Nothing
        }
      where
        pkgdesc = packageDesc pkgEntry
        pkgid = packageId pkgEntry

    handleNotFound action = catchIO action $ \e ->
      if isDoesNotExistError e
        then do
          case repo of
            RepoLegacy (Located _ repoLegacy) -> warn verbosity $ errMissingPackageList repoLegacy
            RepoSecure (Located _ repoSecure) -> warn verbosity $ errMissingPackageList repoSecure
            RepoLocalNoIndex (Located _ repoLocal) ->
              warn verbosity $
                "Error during construction of local+noindex "
                  ++ unRepoName (localRepoName repoLocal)
                  ++ " repository index: "
                  ++ show e
          return (mempty, mempty, emptyStateInfo)
        else ioError e

    isOldThreshold = 15 -- days
    warnIfIndexIsOld dt = do
      when (dt >= isOldThreshold) $ case repo of
        RepoLegacy (Located _ repoLegacy) -> warn verbosity $ errOutdatedPackageList repoLegacy dt
        RepoSecure (Located _ repoSecure) -> warn verbosity $ errOutdatedPackageList repoSecure dt
        RepoLocalNoIndex{} -> return ()

    errMissingPackageList r =
      "The package list for '"
        ++ unRepoName (repositoryName r)
        ++ "' does not exist. Run 'cabal update' to download it."
    errOutdatedPackageList r dt =
      "The package list for '"
        ++ unRepoName (repositoryName r)
        ++ "' is "
        ++ shows (floor dt :: Int) " days old.\nRun "
        ++ "'cabal update' to get the latest list of available packages."

-- | Return the age of the index file in days (as a Double).
getIndexFileAge :: Located r -> IO Double
getIndexFileAge repo = getFileAge $ indexBaseName repo <.> "tar"

-- | A set of files (or directories) that can be monitored to detect when
-- there might have been a change in the source packages.
getSourcePackagesMonitorFiles :: [Located r] -> [FilePath]
getSourcePackagesMonitorFiles repos =
  concat
    [ [ indexBaseName repo <.> "cache"
      , indexBaseName repo <.> "timestamp"
      ]
    | repo <- repos
    ]

-- | It is not necessary to call this, as the cache will be updated when the
-- index is read normally. However you can do the work earlier if you like.
updateRepoIndexCache :: Verbosity -> Index r -> IO ()
updateRepoIndexCache verbosity index =
  whenCacheOutOfDate index $ updatePackageIndexCacheFile verbosity index

whenCacheOutOfDate :: Index r -> IO () -> IO ()
whenCacheOutOfDate index action = do
  exists <- doesFileExist $ cacheFile index
  if not exists
    then action
    else do
      indexTime <- getModTime $ indexFile index
      cacheTime <- getModTime $ cacheFile index
      when (indexTime > cacheTime) action

------------------------------------------------------------------------
-- Reading and updating the index cache
--

-- | Which index do we mean?
data Index r
  = -- | The main index for the specified repository
    RepoIndex RepoContext (Located r)

indexFile :: Index r -> FilePath
indexFile (RepoIndex _ctxt repo) = indexBaseName repo <.> "tar"

cacheFile :: Index r -> FilePath
cacheFile (RepoIndex _ctxt repo) = indexBaseName repo <.> "cache"

timestampFile :: Index r -> FilePath
timestampFile (RepoIndex _ctxt repo) = indexBaseName repo <.> "timestamp"

updatePackageIndexCacheFile :: Verbosity -> Index r -> IO ()
updatePackageIndexCacheFile verbosity index = do
  info verbosity ("Updating index cache file " ++ cacheFile index ++ " ...")
  withIndexEntries verbosity index callback callbackNoIndex
  where
    callback entries = do
      let !maxTs = maximumTimestamp (map cacheEntryTimestamp entries)
          cache =
            Cache
              { cacheHeadTs = maxTs
              , cacheEntries = entries
              }
      writeIndexCache index cache
      info
        verbosity
        ( "Index cache updated to index-state "
            ++ prettyShow (cacheHeadTs cache)
        )

    callbackNoIndex entries = do
      writeNoIndexCache verbosity index $ NoIndexCache entries
      info verbosity "Index cache updated"

withIndexEntriesNoIndex :: Verbosity -> Index r -> ([NoIndexCacheEntry] -> IO b) -> IO b
withIndexEntriesNoIndex verbosity (RepoIndex _repoCtxt (RepoLocalNoIndex (Located _cacheDir (LocalRepo name localDir _)))) callback = do
  dirContents <- listDirectory localDir
  let contentSet = Set.fromList dirContents

  entries <- handle handler $ fmap catMaybes $ for dirContents $ \file -> do
    case isTarGz file of
      Nothing
        | isPreferredVersions file -> do
            contents <- BS.readFile (localDir </> file)
            let versionPreferencesParsed = parsePreferredVersionsWarnings contents
            let (warnings, versionPreferences) = partitionEithers versionPreferencesParsed
            unless (null warnings) $ do
              warn verbosity $
                "withIndexEntries: failed to parse some entries of \"preferred-versions\" found at: "
                  ++ (localDir </> file)
              for_ warnings $ \err -> do
                warn verbosity $ "* \"" ++ preferredVersionsOriginalDependency err
                warn verbosity $ "Parser Error: " ++ preferredVersionsParsecError err
            return $ Just $ NoIndexCachePreference versionPreferences
        | otherwise -> do
            unless (takeFileName file == "noindex.cache" || ".cabal" `isSuffixOf` file) $
              info verbosity $
                "Skipping " ++ file
            return Nothing
      Just pkgid | cabalPath `Set.member` contentSet -> do
        contents <- BSS.readFile (localDir </> cabalPath)
        for (parseGenericPackageDescriptionMaybe contents) $ \gpd ->
          return (CacheGPD gpd contents)
        where
          cabalPath = prettyShow pkgid ++ ".cabal"
      Just pkgId -> do
        -- check for the right named .cabal file in the compressed tarball
        tarGz <- BS.readFile (localDir </> file)
        let tar = GZip.decompress tarGz
            entries = Tar.read tar

        case Tar.foldEntries (readCabalEntry pkgId) Nothing (const Nothing) entries of
          Just ce -> return (Just ce)
          Nothing -> dieWithException verbosity $ CannotReadCabalFile file

  let (prefs, gpds) =
        partitionEithers $
          map
            ( \case
                NoIndexCachePreference deps -> Left deps
                CacheGPD gpd _ -> Right gpd
            )
            entries

  info verbosity $ "Entries in file+noindex repository " ++ unRepoName name
  for_ gpds $ \gpd ->
    info verbosity $ "- " ++ prettyShow (package $ Distribution.PackageDescription.packageDescription gpd)
  unless (null prefs) $ do
    info verbosity $ "Preferred versions in file+noindex repository " ++ unRepoName name
    for_ (concat prefs) $ \pref ->
      info verbosity ("* " ++ prettyShow pref)

  callback entries
  where
    handler :: IOException -> IO a
    handler e = dieWithException verbosity $ ErrorUpdatingIndex (unRepoName name) e

    isTarGz :: FilePath -> Maybe PackageIdentifier
    isTarGz fp = do
      pfx <- stripSuffix ".tar.gz" fp
      simpleParsec pfx

    stripSuffix sfx str = fmap reverse (stripPrefix (reverse sfx) (reverse str))

    -- look for <pkgid>/<pkgname>.cabal inside the tarball
    readCabalEntry :: PackageIdentifier -> Tar.Entry -> Maybe NoIndexCacheEntry -> Maybe NoIndexCacheEntry
    readCabalEntry pkgId entry Nothing
      | filename == Tar.entryPath entry
      , Tar.NormalFile contents _ <- Tar.entryContent entry =
          let bs = BS.toStrict contents
           in ((`CacheGPD` bs) <$> parseGenericPackageDescriptionMaybe bs)
      where
        filename = prettyShow pkgId FilePath.Posix.</> prettyShow (packageName pkgId) ++ ".cabal"
    readCabalEntry _ _ x = x

readPackageIndexCacheFile
  :: Package pkg
  => Verbosity
  -> (PackageEntry -> pkg)
  -> Index r
  -> RepoIndexState
  -> IO (PackageIndex pkg, [Dependency], IndexStateInfo)
readPackageIndexCacheFile verbosity mkPkg index idxState
  | localNoIndex index = do
      cache0 <- readNoIndexCache verbosity index
      (pkgs, prefs) <- packageNoIndexFromCache verbosity mkPkg cache0
      pure (pkgs, prefs, emptyStateInfo)
  | otherwise = do
      cache0 <- readIndexCache verbosity index
      indexHnd <- openFile (indexFile index) ReadMode
      let (cache, isi) = filterCache idxState cache0
      (pkgs, deps) <- packageIndexFromCache verbosity mkPkg indexHnd cache
      pure (pkgs, deps, isi)

packageIndexFromCache
  :: Package pkg
  => Verbosity
  -> (PackageEntry -> pkg)
  -> Handle
  -> Cache
  -> IO (PackageIndex pkg, [Dependency])
packageIndexFromCache verbosity mkPkg hnd cache = do
  (pkgs, prefs) <- packageListFromCache verbosity mkPkg hnd cache
  pkgIndex <- evaluate $ PackageIndex.fromList pkgs
  return (pkgIndex, prefs)

packageNoIndexFromCache
  :: forall pkg
   . Package pkg
  => Verbosity
  -> (PackageEntry -> pkg)
  -> NoIndexCache
  -> IO (PackageIndex pkg, [Dependency])
packageNoIndexFromCache _verbosity mkPkg cache = do
  let (pkgs, prefs) = packageListFromNoIndexCache
  pkgIndex <- evaluate $ PackageIndex.fromList pkgs
  pure (pkgIndex, prefs)
  where
    packageListFromNoIndexCache :: ([pkg], [Dependency])
    packageListFromNoIndexCache = foldr go mempty (noIndexCacheEntries cache)

    go :: NoIndexCacheEntry -> ([pkg], [Dependency]) -> ([pkg], [Dependency])
    go (CacheGPD gpd bs) (pkgs, prefs) =
      let pkgId = package $ Distribution.PackageDescription.packageDescription gpd
       in (mkPkg (NormalPackage pkgId gpd (BS.fromStrict bs) 0) : pkgs, prefs)
    go (NoIndexCachePreference deps) (pkgs, prefs) =
      (pkgs, deps ++ prefs)

-- | Read package list
--
-- The result package releases and preference entries are guaranteed
-- to be unique.
--
-- Note: 01-index.tar is an append-only index and therefore contains
-- all .cabal edits and preference-updates. The masking happens
-- here, i.e. the semantics that later entries in a tar file mask
-- earlier ones is resolved in this function.
packageListFromCache
  :: Verbosity
  -> (PackageEntry -> pkg)
  -> Handle
  -> Cache
  -> IO ([pkg], [Dependency])
packageListFromCache verbosity mkPkg hnd Cache{..} = accum mempty [] mempty cacheEntries
  where
    accum !srcpkgs btrs !prefs [] = return (Map.elems srcpkgs ++ btrs, Map.elems prefs)
    accum srcpkgs btrs prefs (CachePackageId pkgid blockno _ : entries) = do
      -- Given the cache entry, make a package index entry.
      -- The magic here is that we use lazy IO to read the .cabal file
      -- from the index tarball if it turns out that we need it.
      -- Most of the time we only need the package id.
      ~(pkg, pkgtxt) <- unsafeInterleaveIO $ do
        pkgtxt <- getEntryContent blockno
        pkg <- readPackageDescription pkgid pkgtxt
        return (pkg, pkgtxt)

      let srcpkg = mkPkg (NormalPackage pkgid pkg pkgtxt blockno)
      accum (Map.insert pkgid srcpkg srcpkgs) btrs prefs entries
    accum srcpkgs btrs prefs (CacheBuildTreeRef refType blockno : entries) = do
      -- We have to read the .cabal file eagerly here because we can't cache the
      -- package id for build tree references - the user might edit the .cabal
      -- file after the reference was added to the index.
      path <- fmap byteStringToFilePath . getEntryContent $ blockno
      pkg <- do
        let err = "Error reading package index from cache."
        file <- tryFindAddSourcePackageDesc verbosity path err
        PackageDesc.Parse.readGenericPackageDescription normal file
      let srcpkg = mkPkg (BuildTreeRef refType (packageId pkg) pkg path blockno)
      accum srcpkgs (srcpkg : btrs) prefs entries
    accum srcpkgs btrs prefs (CachePreference pref@(Dependency pn _ _) _ _ : entries) =
      accum srcpkgs btrs (Map.insert pn pref prefs) entries

    getEntryContent :: BlockNo -> IO ByteString
    getEntryContent blockno = do
      entry <- Tar.hReadEntry hnd blockno
      case Tar.entryContent entry of
        Tar.NormalFile content _size -> return content
        Tar.OtherEntryType typecode content _size
          | Tar.isBuildTreeRefTypeCode typecode ->
              return content
        _ -> interror "unexpected tar entry type"

    readPackageDescription :: PackageIdentifier -> ByteString -> IO GenericPackageDescription
    readPackageDescription pkgid content =
      case snd $ PackageDesc.Parse.runParseResult $ parseGenericPackageDescription $ BS.toStrict content of
        Right gpd -> return gpd
        Left (Just specVer, _) | specVer >= mkVersion [2, 2] -> return (dummyPackageDescription specVer)
        Left _ -> interror "failed to parse .cabal file"
      where
        dummyPackageDescription :: Version -> GenericPackageDescription
        dummyPackageDescription specVer =
          GenericPackageDescription
            { packageDescription =
                emptyPackageDescription
                  { package = pkgid
                  , synopsis = dummySynopsis
                  }
            , gpdScannedVersion = Just specVer -- tells index scanner to skip this file.
            , genPackageFlags = []
            , condLibrary = Nothing
            , condSubLibraries = []
            , condForeignLibs = []
            , condExecutables = []
            , condTestSuites = []
            , condBenchmarks = []
            }

        dummySynopsis = "<could not be parsed due to unsupported CABAL spec-version>"

    interror :: String -> IO a
    interror msg =
      dieWithException verbosity $ InternalError msg

------------------------------------------------------------------------
-- Index cache data structure --

readNoIndexCache :: Verbosity -> Index r -> IO NoIndexCache
readNoIndexCache verbosity index = do
  cacheOrFail <- readNoIndexCache' index
  case cacheOrFail of
    Left msg -> do
      warn verbosity $
        concat
          [ "Parsing the index cache failed ("
          , msg
          , "). "
          , "Trying to regenerate the index cache..."
          ]

      updatePackageIndexCacheFile verbosity index

      either (die' verbosity) return =<< readNoIndexCache' index

    -- we don't hash cons local repository cache, they are hopefully small
    Right res -> return res

readNoIndexCache' :: Index r -> IO (Either String NoIndexCache)
readNoIndexCache' index = structuredDecodeFileOrFail (cacheFile index)

writeNoIndexCache :: Verbosity -> Index r -> NoIndexCache -> IO ()
writeNoIndexCache verbosity index cache = do
  let path = cacheFile index
  createDirectoryIfMissingVerbose verbosity True (takeDirectory path)
  structuredEncodeFile path cache

-- | Write the 'IndexState' to the filesystem
writeIndexTimestamp :: Index r -> RepoIndexState -> IO ()
writeIndexTimestamp index st =
  writeFile (timestampFile index) (prettyShow st)

-- | Read out the "current" index timestamp, i.e., what
-- timestamp you would use to revert to this version
currentIndexTimestamp :: Verbosity -> RepoContext -> Repo -> IO Timestamp
currentIndexTimestamp verbosity repoCtxt r = do
  mb_is <- readIndexTimestamp verbosity (RepoIndex repoCtxt r)
  case mb_is of
    Just (IndexStateTime ts) -> return ts
    _ -> do
      (_, _, isi) <- readRepoIndex verbosity repoCtxt r IndexStateHead
      return (isiHeadTime isi)

-- | Read the 'IndexState' from the filesystem
readIndexTimestamp :: Verbosity -> Index r -> IO (Maybe RepoIndexState)
readIndexTimestamp verbosity index =
  fmap simpleParsec (readFile (timestampFile index))
    `catchIO` \e ->
      if isDoesNotExistError e
        then return Nothing
        else do
          warn verbosity $ "Warning: could not read current index timestamp: " ++ displayException e
          return Nothing

-- | Cache format for 'file+noindex' repositories
newtype NoIndexCache = NoIndexCache
  { noIndexCacheEntries :: [NoIndexCacheEntry]
  }
  deriving (Show, Generic)

instance NFData NoIndexCache where
  rnf = rnf . noIndexCacheEntries

data NoIndexCacheEntry
  = CacheGPD GenericPackageDescription !BSS.ByteString
  | NoIndexCachePreference [Dependency]
  deriving (Eq, Show, Generic)

instance NFData NoIndexCacheEntry where
  rnf (CacheGPD gpd bs) = rnf gpd `seq` rnf bs
  rnf (NoIndexCachePreference dep) = rnf dep

----------------------------------------------------------------------------
-- new binary 01-index.cache format

instance Binary NoIndexCache

instance Structured NoIndexCache

-- | We need to save only .cabal file contents
instance Binary NoIndexCacheEntry where
  put (CacheGPD _ bs) = do
    put (0 :: Word8)
    put bs
  put (NoIndexCachePreference dep) = do
    put (1 :: Word8)
    put dep

  get = do
    t :: Word8 <- get
    case t of
      0 -> do
        bs <- get
        case parseGenericPackageDescriptionMaybe bs of
          Just gpd -> return (CacheGPD gpd bs)
          Nothing -> fail "Failed to parse GPD"
      1 -> do
        NoIndexCachePreference <$> get
      _ -> fail "Failed to parse NoIndexCacheEntry"

instance Structured NoIndexCacheEntry where
  structure = nominalStructure
