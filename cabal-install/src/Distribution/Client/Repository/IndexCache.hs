{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.Client.Repository.IndexCache where

import Distribution.Client.Compat.Prelude
import Prelude ()

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Archive.Tar.Index as Tar

import qualified Data.ByteString.Char8 as BSS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS

import qualified Data.Map as Map

import Distribution.Client.IndexUtils.Timestamp (Timestamp, epochTimeToTimestamp, maximumTimestamp, nullTimestamp)
import qualified Distribution.Client.Tar as Tar
import Distribution.Package (Package (..), packageName, packageVersion)
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)
import Distribution.Parsec (simpleParsecBS)
import qualified Distribution.Simple.PackageDescription as PackageDesc.Parse
import Distribution.Version (mkVersion)
import System.FilePath (normalise, splitDirectories, takeExtension)

import Distribution.Simple.Utils (die', info, warn)

import Distribution.Client.GZipUtils (maybeDecompress)
import Distribution.Client.Repository.Cache (BlockNo, Cache (..), IndexCacheEntry (..), PackageEntry (..), cacheEntryTimestamp, refTypeFromTypeCode, typeCodeFromRefType)
import Distribution.Client.Repository.PreferredVersions (isPreferredVersions, parsePreferredVersions)
import Distribution.Client.Utils (byteStringToFilePath, tryFindAddSourcePackageDesc)
import Distribution.Utils.Structured (structuredDecodeFileOrFail, structuredEncodeFile)
import Distribution.Verbosity (normal)
import qualified Hackage.Security.Client as Sec
import qualified Hackage.Security.Util.Some as Sec
import System.Directory (doesDirectoryExist)
import System.IO (IOMode (..), withFile)
import System.IO.Unsafe (unsafeInterleaveIO)

-- TODO: temporary
data Is01Index = Is01Index | IsNot01Index

-- | Read the 'Index' cache from the filesystem
--
-- If a corrupted index cache is detected this function regenerates
-- the index cache and then reattempt to read the index once (and
-- 'die's if it fails again).
readIndexCache :: Is01Index -> Verbosity -> _RepoContext -> FilePath -> r -> IO Cache
readIndexCache is01Index verbosity repoCtxt cacheFile repo = do
  cacheOrFail <- readIndexCache' is01Index cacheFile
  case cacheOrFail of
    Left msg -> do
      warn verbosity $
        concat
          [ "Parsing the index cache failed ("
          , msg
          , "). "
          , "Trying to regenerate the index cache..."
          ]

      updatePackageIndexCacheFile is01Index verbosity repoCtxt repo

      either (die' verbosity) (return . hashConsCache) =<< readIndexCache' is01Index cacheFile
    Right res -> return (hashConsCache res)

-- | Read the cache from the filesystem without attempting to
-- regenerate on parsing failures.
readIndexCache' :: Is01Index -> FilePath -> IO (Either String Cache)
readIndexCache' Is01Index cacheFile = structuredDecodeFileOrFail cacheFile
readIndexCache' IsNot01Index cacheFile = Right . read00IndexCache <$> BSS.readFile cacheFile

-- | Write the cache to the filesystem
writeIndexCache :: Is01Index -> FilePath -> Cache -> IO ()
writeIndexCache Is01Index cacheFile cache = structuredEncodeFile cacheFile cache
writeIndexCache IsNot01Index cacheFile cache = writeFile cacheFile (show00IndexCache cache)

updatePackageIndexCacheFile :: Is01Index -> Verbosity -> FilePath -> r -> IO ()
updatePackageIndexCacheFile is01Index verbosity cacheFile index =
  case is01Index of
    Is01Index -> withIndexEntriesSecure verbosity index callback
    IsNot01Index -> withIndexEntriesLegacy verbosity cacheFile callback
  where
    callback entries = do
      let !maxTs = maximumTimestamp (map cacheEntryTimestamp entries)
          cache =
            Cache
              { cacheHeadTs = maxTs
              , cacheEntries = entries
              }
      writeIndexCache is01Index cacheFile cache
      info
        verbosity
        ( "Index cache updated to index-state "
            ++ prettyShow (cacheHeadTs cache)
        )

-- Read the index (for the purpose of building a cache)
--
-- The callback is provided with list of cache entries, which is guaranteed to
-- be lazily constructed. This list must ONLY be used in the scope of the
-- callback; when the callback is terminated the file handle to the index will
-- be closed and further attempts to read from the list will result in (pure)
-- I/O exceptions.
--
-- In the construction of the index for a secure repo we take advantage of the
-- index built by the @hackage-security@ library to avoid reading the @.tar@
-- file as much as possible (we need to read it only to extract preferred
-- versions). This helps performance, but is also required for correctness:
-- the new @01-index.tar.gz@ may have multiple versions of preferred-versions
-- files, and 'parsePackageIndex' does not correctly deal with that (see #2956);
-- by reading the already-built cache from the security library we will be sure
-- to only read the latest versions of all files.
--
-- TODO: It would be nicer if we actually incrementally updated @cabal@'s
-- cache, rather than reconstruct it from zero on each update. However, this
-- would require a change in the cache format.

withIndexEntriesSecure
  :: repoContext
  -> r
  -> ([IndexCacheEntry] -> IO a)
  -> IO a
withIndexEntriesSecure repoCtxt repo callback =
  repoContextWithSecureRepo repoCtxt repo $ \repoSecure ->
    Sec.withIndex repoSecure $ \Sec.IndexCallbacks{..} -> do
      -- Incrementally (lazily) read all the entries in the tar file in order,
      -- including all revisions, not just the last revision of each file
      indexEntries <- lazyUnfold indexLookupEntry (Sec.directoryFirst indexDirectory)
      callback
        [ cacheEntry
        | (dirEntry, indexEntry) <- indexEntries
        , cacheEntry <- toCacheEntries dirEntry indexEntry
        ]
  where
    toCacheEntries
      :: Sec.DirectoryEntry
      -> Sec.Some Sec.IndexEntry
      -> [IndexCacheEntry]
    toCacheEntries dirEntry (Sec.Some sie) =
      case Sec.indexEntryPathParsed sie of
        Nothing -> [] -- skip unrecognized file
        Just (Sec.IndexPkgMetadata _pkgId) -> [] -- skip metadata
        Just (Sec.IndexPkgCabal pkgId) ->
          force
            [CachePackageId pkgId blockNo timestamp]
        Just (Sec.IndexPkgPrefs _pkgName) ->
          force
            [ CachePreference dep blockNo timestamp
            | dep <- parsePreferredVersions (Sec.indexEntryContent sie)
            ]
      where
        blockNo = Sec.directoryEntryBlockNo dirEntry
        timestamp =
          fromMaybe (error "withIndexEntries: invalid timestamp") $
            epochTimeToTimestamp $
              Sec.indexEntryTime sie

withIndexEntriesLegacy :: Verbosity -> FilePath -> ([IndexCacheEntry] -> IO b) -> IO b
withIndexEntriesLegacy verbosity indexFile callback = do
  -- non-secure repositories
  withFile indexFile ReadMode $ \h -> do
    bs <- maybeDecompress `fmap` BS.hGetContents h
    pkgsOrPrefs <- lazySequence $ parsePackageIndex verbosity bs
    callback $ map toCache (catMaybes pkgsOrPrefs)
  where
    toCache :: PackageOrDep -> IndexCacheEntry
    toCache (Pkg (NormalPackage pkgid _ _ blockNo)) = CachePackageId pkgid blockNo nullTimestamp
    toCache (Pkg (BuildTreeRef refType _ _ _ blockNo)) = CacheBuildTreeRef refType blockNo
    toCache (Dep d) = CachePreference d 0 nullTimestamp

-- | Variation on 'sequence' which evaluates the actions lazily
--
-- Pattern matching on the result list will execute just the first action;
-- more generally pattern matching on the first @n@ '(:)' nodes will execute
-- the first @n@ actions.
lazySequence :: [IO a] -> IO [a]
lazySequence = unsafeInterleaveIO . go
  where
    go [] = return []
    go (x : xs) = do
      x' <- x
      xs' <- lazySequence xs
      return (x' : xs')

-- | A lazy unfolder for lookup operations which return the current
-- value and (possibly) the next key
lazyUnfold :: (k -> IO (v, Maybe k)) -> k -> IO [(k, v)]
lazyUnfold step = goLazy . Just
  where
    goLazy s = unsafeInterleaveIO (go s)

    go Nothing = return []
    go (Just k) = do
      (v, mk') <- step k
      vs' <- goLazy mk'
      return ((k, v) : vs')

--
-- legacy 00-index.cache format
--

packageKey, blocknoKey, buildTreeRefKey, preferredVersionKey :: String
packageKey = "pkg:"
blocknoKey = "b#"
buildTreeRefKey = "build-tree-ref:"
preferredVersionKey = "pref-ver:"

read00IndexCache :: BSS.ByteString -> Cache
read00IndexCache bs =
  Cache
    { cacheHeadTs = nullTimestamp
    , cacheEntries = mapMaybe read00IndexCacheEntry $ BSS.lines bs
    }

read00IndexCacheEntry :: BSS.ByteString -> Maybe IndexCacheEntry
read00IndexCacheEntry = \line ->
  case BSS.words line of
    [key, pkgnamestr, pkgverstr, sep, blocknostr]
      | key == BSS.pack packageKey && sep == BSS.pack blocknoKey ->
          case ( parseName pkgnamestr
               , parseVer pkgverstr []
               , parseBlockNo blocknostr
               ) of
            (Just pkgname, Just pkgver, Just blockno) ->
              Just
                ( CachePackageId
                    (PackageIdentifier pkgname pkgver)
                    blockno
                    nullTimestamp
                )
            _ -> Nothing
    [key, typecodestr, blocknostr] | key == BSS.pack buildTreeRefKey ->
      case (parseRefType typecodestr, parseBlockNo blocknostr) of
        (Just refType, Just blockno) ->
          Just (CacheBuildTreeRef refType blockno)
        _ -> Nothing
    (key : remainder) | key == BSS.pack preferredVersionKey -> do
      pref <- simpleParsecBS (BSS.unwords remainder)
      return $ CachePreference pref 0 nullTimestamp
    _ -> Nothing
  where
    parseName str
      | BSS.all (\c -> isAlphaNum c || c == '-') str =
          Just (mkPackageName (BSS.unpack str))
      | otherwise = Nothing

    parseVer str vs =
      case BSS.readInt str of
        Nothing -> Nothing
        Just (v, str') -> case BSS.uncons str' of
          Just ('.', str'') -> parseVer str'' (v : vs)
          Just _ -> Nothing
          Nothing -> Just (mkVersion (reverse (v : vs)))

    parseBlockNo str =
      case BSS.readInt str of
        Just (blockno, remainder)
          | BSS.null remainder -> Just (fromIntegral blockno)
        _ -> Nothing

    parseRefType str =
      case BSS.uncons str of
        Just (typeCode, remainder)
          | BSS.null remainder && Tar.isBuildTreeRefTypeCode typeCode ->
              Just (refTypeFromTypeCode typeCode)
        _ -> Nothing

show00IndexCache :: Cache -> String
show00IndexCache Cache{..} = unlines $ map show00IndexCacheEntry cacheEntries

show00IndexCacheEntry :: IndexCacheEntry -> String
show00IndexCacheEntry entry = unwords $ case entry of
  CachePackageId pkgid b _ ->
    [ packageKey
    , prettyShow (packageName pkgid)
    , prettyShow (packageVersion pkgid)
    , blocknoKey
    , show b
    ]
  CacheBuildTreeRef tr b ->
    [ buildTreeRefKey
    , [typeCodeFromRefType tr]
    , show b
    ]
  CachePreference dep _ _ ->
    [ preferredVersionKey
    , prettyShow dep
    ]

--
-- Reading the index file
--

-- | Parse an uncompressed \"00-index.tar\" repository index file represented
-- as a 'ByteString'.
data PackageOrDep = Pkg PackageEntry | Dep Dependency

-- | Read @00-index.tar.gz@ and extract @.cabal@ and @preferred-versions@ files
--
-- We read the index using 'Tar.read', which gives us a lazily constructed
-- 'TarEntries'. We translate it to a list of entries using  'tarEntriesList',
-- which preserves the lazy nature of 'TarEntries', and finally 'concatMap' a
-- function over this to translate it to a list of IO actions returning
-- 'PackageOrDep's. We can use 'lazySequence' to turn this into a list of
-- 'PackageOrDep's, still maintaining the lazy nature of the original tar read.
parsePackageIndex :: Verbosity -> ByteString -> [IO (Maybe PackageOrDep)]
parsePackageIndex verbosity = concatMap (uncurry extract) . tarEntriesList . Tar.read
  where
    extract :: BlockNo -> Tar.Entry -> [IO (Maybe PackageOrDep)]
    extract blockNo entry = tryExtractPkg ++ tryExtractPrefs
      where
        tryExtractPkg = do
          mkPkgEntry <- maybeToList $ extractPkg verbosity entry blockNo
          return $ fmap (fmap Pkg) mkPkgEntry

        tryExtractPrefs = do
          prefs' <- maybeToList $ extractPrefs entry
          fmap (return . Just . Dep) prefs'

-- | Turn the 'Entries' data structure from the @tar@ package into a list,
-- and pair each entry with its block number.
--
-- NOTE: This preserves the lazy nature of 'Entries': the tar file is only read
-- as far as the list is evaluated.
tarEntriesList :: Show e => Tar.Entries e -> [(BlockNo, Tar.Entry)]
tarEntriesList = go 0
  where
    go !_ Tar.Done = []
    go !_ (Tar.Fail e) = error ("tarEntriesList: " ++ show e)
    go !n (Tar.Next e es') = (n, e) : go (Tar.nextEntryOffset e n) es'

extractPkg :: Verbosity -> Tar.Entry -> BlockNo -> Maybe (IO (Maybe PackageEntry))
extractPkg verbosity entry blockNo = case Tar.entryContent entry of
  Tar.NormalFile content _
    | takeExtension fileName == ".cabal" ->
        case splitDirectories (normalise fileName) of
          [pkgname, vers, _] -> case simpleParsec vers of
            Just ver -> Just . return $ Just (NormalPackage pkgid descr content blockNo)
              where
                pkgid = PackageIdentifier (mkPackageName pkgname) ver
                parsed = parseGenericPackageDescriptionMaybe (BS.toStrict content)
                descr = case parsed of
                  Just d -> d
                  Nothing ->
                    error $
                      "Couldn't read cabal file "
                        ++ show fileName
            _ -> Nothing
          _ -> Nothing
  Tar.OtherEntryType typeCode content _
    | Tar.isBuildTreeRefTypeCode typeCode ->
        Just $ do
          let path = byteStringToFilePath content
          dirExists <- doesDirectoryExist path
          if not dirExists
            then return Nothing
            else do
              cabalFile <- tryFindAddSourcePackageDesc verbosity path "Error reading package index."
              descr <- PackageDesc.Parse.readGenericPackageDescription normal cabalFile
              return . Just $
                BuildTreeRef
                  (refTypeFromTypeCode typeCode)
                  (packageId descr)
                  descr
                  path
                  blockNo
  _ -> Nothing
  where
    fileName = Tar.entryPath entry

extractPrefs :: Tar.Entry -> Maybe [Dependency]
extractPrefs entry = case Tar.entryContent entry of
  Tar.NormalFile content _
    | isPreferredVersions entrypath ->
        Just prefs
    where
      entrypath = Tar.entryPath entry
      prefs = parsePreferredVersions content
  _ -> Nothing

-- | Optimise sharing of equal values inside 'Cache'
--
-- c.f. https://en.wikipedia.org/wiki/Hash_consing
hashConsCache :: Cache -> Cache
hashConsCache cache0 =
  cache0{cacheEntries = go mempty mempty (cacheEntries cache0)}
  where
    -- TODO/NOTE:
    --
    -- If/when we redo the binary serialisation via e.g. CBOR and we
    -- are able to use incremental decoding, we may want to move the
    -- hash-consing into the incremental deserialisation, or
    -- alternatively even do something like
    -- http://cbor.schmorp.de/value-sharing
    --
    go _ _ [] = []
    -- for now we only optimise only CachePackageIds since those
    -- represent the vast majority
    go !pns !pvs (CachePackageId pid bno ts : rest) =
      CachePackageId pid' bno ts : go pns' pvs' rest
      where
        !pid' = PackageIdentifier pn' pv'
        (!pn', !pns') = mapIntern pn pns
        (!pv', !pvs') = mapIntern pv pvs
        PackageIdentifier pn pv = pid
    go pns pvs (x : xs) = x : go pns pvs xs

    mapIntern :: Ord k => k -> Map.Map k k -> (k, Map.Map k k)
    mapIntern k m = maybe (k, Map.insert k k m) (\k' -> (k', m)) (Map.lookup k m)
