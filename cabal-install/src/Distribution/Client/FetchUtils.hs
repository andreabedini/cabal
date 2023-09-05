{-# LANGUAGE NamedFieldPuns #-}
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Distribution.Client.FetchUtils
-- Copyright   :  (c) David Himmelstrup 2005
--                    Duncan Coutts 2011
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Functions for fetching packages
module Distribution.Client.FetchUtils
  ( -- * fetching packages
    fetchPackage
  , isFetched
  , checkFetched

    -- ** specifically for repo packages
  , checkRepoTarballFetched
  , fetchRepoTarball
  , verifyFetchedTarball

    -- ** fetching packages asynchronously
  , asyncFetchPackages
  , waitAsyncFetchPackage
  , AsyncFetchMap

    -- * fetching other things
  , downloadIndex
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.HttpUtils
  ( DownloadResult (..)
  , HttpTransport (..)
  , downloadURI
  , isOldHackageURI
  , remoteRepoCheckHttps
  , transportCheckHttps
  )
import Distribution.Client.Types

import Distribution.Client.GlobalFlags
  ( RepoContext (..)
  )
import Distribution.Client.Utils
  ( ProgressPhase (..)
  , progressMessage
  )
import Distribution.Package
  ( PackageId
  , packageName
  , packageVersion
  )
import Distribution.Simple.Utils
  ( debug
  , dieWithException
  , info
  , notice
  , warn
  )
import Distribution.Verbosity
  ( verboseUnmarkOutput
  )

import Control.Concurrent.Async
import Control.Concurrent.MVar
import qualified Control.Exception.Safe as Safe
import qualified Data.Map as Map
import Network.URI
  ( URI (uriPath)
  )
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , getFileSize
  , getTemporaryDirectory
  )
import System.FilePath
  ( (<.>)
  , (</>)
  )
import qualified System.FilePath.Posix as FilePath.Posix
  ( combine
  , joinPath
  )
import System.IO
  ( hClose
  , openTempFile
  )

import Distribution.Client.Errors
import Distribution.Client.HashValue (HashValue, hashFromTUF, readFileHashValue)
import qualified Hackage.Security.Client as Sec
import qualified Hackage.Security.Util.Checked as Sec
import qualified Hackage.Security.Util.Path as Sec

-- ------------------------------------------------------------

-- * Actually fetch things

-- ------------------------------------------------------------

-- | Returns @True@ if the package has already been fetched
-- or does not need fetching.
isFetched :: UnresolvedPkgLoc -> IO Bool
isFetched loc = case loc of
  LocalUnpackedPackage _dir -> return True
  LocalTarballPackage _file -> return True
  RemoteTarballPackage _uri local -> return (isJust local)
  RepoTarballPackage repo pkgid _ -> doesFileExist (packageFile repo pkgid)
  RemoteSourceRepoPackage _ local -> return (isJust local)

-- | Checks if the package has already been fetched (or does not need
-- fetching) and if so returns evidence in the form of a 'PackageLocation'
-- with a resolved local file location.
checkFetched
  :: UnresolvedPkgLoc
  -> IO (Maybe ResolvedPkgLoc)
checkFetched loc =
  -- FIXME: remove IO once we know this works
  -- NOTE: this is traverse, isn't it?
  return
    $ case loc of
      LocalUnpackedPackage dir ->
        Just $ LocalUnpackedPackage dir
      LocalTarballPackage file ->
        Just $ LocalTarballPackage file
      RemoteTarballPackage uri (Just file) ->
        Just $ RemoteTarballPackage uri file
      RepoTarballPackage repo pkgid (Just file) ->
        Just $ RepoTarballPackage repo pkgid file
      RemoteSourceRepoPackage repo (Just file) ->
        Just $ RemoteSourceRepoPackage repo file
      RemoteTarballPackage _uri Nothing -> Nothing
      RemoteSourceRepoPackage _repo Nothing -> Nothing
      RepoTarballPackage _repo _pkgid Nothing -> Nothing

-- | Like 'checkFetched' but for the specific case of a 'RepoTarballPackage'.
checkRepoTarballFetched :: Repo -> PackageId -> IO (Maybe FilePath)
checkRepoTarballFetched repo pkgid = do
  let file = packageFile repo pkgid
  exists <- doesFileExist file
  if exists
    then return (Just file)
    else return Nothing

verifyFetchedTarball :: Verbosity -> RepoContext -> Repo -> PackageId -> IO Bool
verifyFetchedTarball verbosity repoCtxt repo pkgid =
  let file = packageFile repo pkgid
      handleError :: IO Bool -> IO Bool
      handleError act = do
        res <- Safe.try act
        case res of
          Left e -> warn verbosity ("Error verifying fetched tarball " ++ file ++ ", will redownload: " ++ show (e :: SomeException)) >> pure False
          Right b -> pure b
   in handleError $ do
        exists <- doesFileExist file
        if not exists
          then return True -- if the file does not exist, it vacuously passes validation, since it will be downloaded as necessary with what we will then check is a valid hash.
          else case repo of
            -- a secure repo has hashes we can compare against to confirm this is the correct file.
            RepoSecure{} ->
              repoContextWithSecureRepo repoCtxt repo $ \repoSecure ->
                Sec.withIndex repoSecure $ \callbacks ->
                  let warnAndFail s = warn verbosity ("Fetched tarball " ++ file ++ " does not match server, will redownload: " ++ s) >> return False
                   in -- the do block in parens is due to dealing with the checked exceptions mechanism.
                      ( do
                          fileInfo <- Sec.indexLookupFileInfo callbacks pkgid
                          sz <- Sec.FileLength . fromInteger <$> getFileSize file
                          if sz /= Sec.fileInfoLength (Sec.trusted fileInfo)
                            then warnAndFail "file length mismatch"
                            else do
                              res <- Sec.compareTrustedFileInfo (Sec.trusted fileInfo) <$> Sec.computeFileInfo (Sec.Path file :: Sec.Path Sec.Absolute)
                              if res
                                then pure True
                                else warnAndFail "file hash mismatch"
                      )
                        `Sec.catchChecked` (\(e :: Sec.InvalidPackageException) -> warnAndFail (show e))
                        `Sec.catchChecked` (\(e :: Sec.VerificationError) -> warnAndFail (show e))
            _ -> pure True

-- | Fetch a package if we don't have it already.
fetchPackage
  :: Verbosity
  -> RepoContext
  -> UnresolvedPkgLoc
  -> IO ResolvedPkgLoc
fetchPackage verbosity repoCtxt loc = case loc of
  LocalUnpackedPackage dir ->
    return (LocalUnpackedPackage dir)
  LocalTarballPackage file ->
    return (LocalTarballPackage file)
  RemoteTarballPackage uri (Just file) ->
    return (RemoteTarballPackage uri file)
  RepoTarballPackage repo pkgid (Just file) ->
    return (RepoTarballPackage repo pkgid file)
  RemoteSourceRepoPackage repo (Just dir) ->
    return (RemoteSourceRepoPackage repo dir)
  RemoteTarballPackage uri Nothing -> do
    path <- downloadTarballPackage uri
    return (RemoteTarballPackage uri path)
  RepoTarballPackage repo pkgid Nothing -> do
    local <- fetchRepoTarball verbosity repoCtxt repo pkgid
    return (RepoTarballPackage repo pkgid local)
  RemoteSourceRepoPackage _repo Nothing ->
    dieWithException verbosity FetchPackageErr
  where
    downloadTarballPackage :: URI -> IO (HashValue, FilePath)
    downloadTarballPackage uri = do
      transport <- repoContextGetTransport repoCtxt
      transportCheckHttps verbosity transport uri
      notice verbosity ("Downloading " ++ show uri)
      tmpdir <- getTemporaryDirectory
      (path, hnd) <- openTempFile tmpdir "cabal-.tar.gz"
      hClose hnd
      _ <- downloadURI transport verbosity uri path
      hash <- readFileHashValue path
      return (hash, path)

-- | Fetch a package from a repository.
--
-- Returns the path and the hash of the downloaded package tarball.
--
-- In case the file already exists:
--
-- - For secure repositories, we verify the correct hash and redownload
--   only if the verification fails.
--
-- - For local and legacy repositories: we never redownload.
fetchRepoTarball :: Verbosity -> RepoContext -> Repo -> PackageId -> IO (HashValue, FilePath)
fetchRepoTarball verbosity' repoCtxt repo pkgid = do
  case repo of
    RepoLocalNoIndex{} -> do
      -- NOTE: for a local repository there's nothing to fetch, the
      -- package must be there
      --
      -- NOTE: we don't have a place to store this hash so we have to recompute it
      hash <- readFileHashValue path
      return (hash, path)
    RepoRemote{repoRemote} -> do
      fetched <- doesFileExist path
      if fetched
        then info verbosity $ prettyShow pkgid ++ " has already been downloaded."
        else do
          progressMessage verbosity ProgressDownloading (prettyShow pkgid)
          transport <- repoContextGetTransport repoCtxt
          remoteRepoCheckHttps verbosity transport repoRemote
          let uri = packageURI repoRemote pkgid
          createDirectoryIfMissing True dir
          _ <- downloadURI transport verbosity uri path
          progressMessage verbosity ProgressDownloaded (prettyShow pkgid)
      -- NOTE: we don't have a place to store this hash so we have to recompute it
      hash <- readFileHashValue path
      return (hash, path)
    RepoSecure{} -> repoContextWithSecureRepo repoCtxt repo $ \secureRepo ->
      Sec.withIndex secureRepo $ \repoIndex -> do
        anyVerificationFailure <-
          Sec.handleChecked (\(e :: Sec.InvalidPackageException) -> return $ Just (show e)) $
          Sec.handleChecked (\(e :: Sec.VerificationError) -> return $ Just (show e)) $
          do
            fileInfo <- Sec.indexLookupFileInfo repoIndex pkgid
            sz <- Sec.FileLength . fromInteger <$> getFileSize path
            if sz /= Sec.fileInfoLength (Sec.trusted fileInfo)
              then return $ Just "file length mismatch"
              else do
                hashMatches <- Sec.compareTrustedFileInfo (Sec.trusted fileInfo) <$> Sec.computeFileInfo (Sec.Path path :: Sec.Path Sec.Absolute)
                return
                  $ if hashMatches then Nothing else (Just "file hash mismatch")
        for_ anyVerificationFailure $ \failure -> do
          warn verbosity $ "Fetched tarball " ++ path ++ " does not match server (" ++ failure ++ "), will redownload."
          createDirectoryIfMissing True dir
          progressMessage verbosity ProgressDownloading (prettyShow pkgid)
          Sec.uncheckClientErrors $ do
            info verbosity ("Writing " ++ path)
            Sec.downloadPackage' secureRepo pkgid path
          progressMessage verbosity ProgressDownloaded (prettyShow pkgid)
        -- this is the same hash that would be in fileInfo but fileInfo is
        -- not guaranteed to have one. We leave hackage-security the
        -- responsibility to error out if fileInfo is missing the hash.
        --
        -- NOTE: indexLookupHash can throw InvalidPackageException and VerificationError
        -- I don't know how to handle those exceptions here so we rethrow
        hash <-
          Sec.uncheckClientErrors $
          Sec.trusted <$> Sec.indexLookupHash repoIndex pkgid
        return (hashFromTUF hash, path)
  where
    -- whether we download or not is non-deterministic
    verbosity = verboseUnmarkOutput verbosity'

    path = packageFile repo pkgid
    dir = packageDir repo pkgid

-- | Downloads an index file to [config-dir/packages/serv-id] without
-- hackage-security. You probably don't want to call this directly;
-- use 'updateRepo' instead.
downloadIndex :: HttpTransport -> Verbosity -> RemoteRepo -> FilePath -> IO DownloadResult
downloadIndex transport verbosity remoteRepo cacheDir = do
  remoteRepoCheckHttps verbosity transport remoteRepo
  let uri =
        (remoteRepoURI remoteRepo)
          { uriPath =
              uriPath (remoteRepoURI remoteRepo)
                `FilePath.Posix.combine` "00-index.tar.gz"
          }
      path = cacheDir </> "00-index" <.> "tar.gz"
  createDirectoryIfMissing True cacheDir
  downloadURI transport verbosity uri path

-- ------------------------------------------------------------

-- * Async fetch wrapper utilities

-- ------------------------------------------------------------

type AsyncFetchMap =
  Map
    UnresolvedPkgLoc
    (MVar (Either SomeException ResolvedPkgLoc))

-- | Fork off an async action to download the given packages (by location).
--
-- The downloads are initiated in order, so you can arrange for packages that
-- will likely be needed sooner to be earlier in the list.
--
-- The body action is passed a map from those packages (identified by their
-- location) to a completion var for that package. So the body action should
-- lookup the location and use 'waitAsyncFetchPackage' to get the result.
--
-- Synchronous exceptions raised by the download actions are delivered
-- via 'waitAsyncFetchPackage'.
asyncFetchPackages
  :: Verbosity
  -> RepoContext
  -> [PackageLocation ()]
  -> (AsyncFetchMap -> IO a)
  -> IO a
asyncFetchPackages verbosity repoCtxt pkglocs body = do
  -- TODO: [nice to have] use parallel downloads?

  asyncDownloadVars <-
    sequenceA
      [ do
        v <- newEmptyMVar
        return (pkgloc, v)
      | pkgloc <- pkglocs
      ]

  let fetchPackages :: IO ()
      fetchPackages =
        for_ asyncDownloadVars $ \(pkgloc, var) -> do
          -- Suppress marking here, because 'withAsync' means
          -- that we get nondeterministic interleaving.
          -- It is essential that we don't catch async exceptions here,
          -- specifically 'AsyncCancelled' thrown at us from 'concurrently'.
          result <-
            Safe.try
              $ fetchPackage (verboseUnmarkOutput verbosity) repoCtxt (fmap (const Nothing) pkgloc)
          putMVar var result

  (_, res) <-
    concurrently
      fetchPackages
      (body $ Map.fromList asyncDownloadVars)
  pure res

-- | Expect to find a download in progress in the given 'AsyncFetchMap'
-- and wait on it to finish.
--
-- If the download failed with an exception then this will be thrown.
--
-- Note: This function is supposed to be idempotent, as our install plans
-- can now use the same tarball for many builds, e.g. different
-- components and/or qualified goals, and these all go through the
-- download phase so we end up using 'waitAsyncFetchPackage' twice on
-- the same package. C.f. #4461.
waitAsyncFetchPackage
  :: Verbosity
  -> AsyncFetchMap
  -> UnresolvedPkgLoc
  -> IO ResolvedPkgLoc
waitAsyncFetchPackage verbosity downloadMap srcloc =
  case Map.lookup srcloc downloadMap of
    Just hnd -> do
      debug verbosity $ "Waiting for download of " ++ show srcloc
      either throwIO return =<< readMVar hnd
    Nothing -> fail "waitAsyncFetchPackage: package not being downloaded"

-- ------------------------------------------------------------

-- * Path utilities

-- ------------------------------------------------------------

-- | Generate the full path to the locally cached copy of
-- the tarball for a given @PackageIdentifier@.
packageFile :: Repo -> PackageId -> FilePath
packageFile repo pkgid =
  packageDir repo pkgid
    </> prettyShow pkgid
    <.> "tar.gz"

-- | Generate the full path to the directory where the local cached copy of
-- the tarball for a given @PackageIdentifier@ is stored.
packageDir :: Repo -> PackageId -> FilePath
packageDir (RepoLocalNoIndex (LocalRepo _ dir _) _) _pkgid = dir
packageDir repo pkgid =
  repoLocalDir repo
    </> prettyShow (packageName pkgid)
    </> prettyShow (packageVersion pkgid)

-- | Generate the URI of the tarball for a given package.
packageURI :: RemoteRepo -> PackageId -> URI
packageURI repo pkgid
  | isOldHackageURI (remoteRepoURI repo) =
      (remoteRepoURI repo)
        { uriPath =
            FilePath.Posix.joinPath
              [ uriPath (remoteRepoURI repo)
              , prettyShow (packageName pkgid)
              , prettyShow (packageVersion pkgid)
              , prettyShow pkgid <.> "tar.gz"
              ]
        }
packageURI repo pkgid =
  (remoteRepoURI repo)
    { uriPath =
        FilePath.Posix.joinPath
          [ uriPath (remoteRepoURI repo)
          , "package"
          , prettyShow pkgid <.> "tar.gz"
          ]
    }
