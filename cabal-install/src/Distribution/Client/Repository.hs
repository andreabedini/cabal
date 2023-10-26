{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Distribution.Client.Repository
  ( -- * Repository
    Repo (..)
  , Repository (..)

    -- * Remote repositories
  , asRepo
  , maybeRepoRemote
  , Repo'Remote (..)
  , RepositoryIsRemote (..)

    -- * Located
  , Located (..)
  , repoLocalDir
  , indexBaseName

    -- * Repo context
  , RepoContext (..)
  , withRepoContext'

    -- * HTTP utilities
  , remoteRepoCheckHttps
  , remoteRepoTryUpgradeToHttps
  , isOldHackageURI
  , URI (..)
  , URIAuth (..)

    -- * TODO: sort out these re-exports
  , module Distribution.Client.Types.Repo
  , module Distribution.Client.Types.RepoName
  , packageURI
  )
where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Control.Concurrent
  ( MVar
  , modifyMVar
  , newMVar
  )
import Data.Kind (Type)
import qualified Data.Map as Map

import Network.URI
import System.FilePath ((</>))
import qualified System.FilePath.Posix as FilePath.Posix

import Distribution.Client.HttpUtils
  ( HttpTransport (..)
  , configureTransport
  , requiresHttpsErrorMessage
  , supportedTransports
  )
import qualified Distribution.Client.Security.DNS as Sec.DNS
import qualified Distribution.Client.Security.HTTP as Sec.HTTP
import Distribution.Client.Types.Repo
import Distribution.Client.Types.RepoName
import Distribution.Simple.Utils (die', info, warn)

import Distribution.Compat.Lens (Lens', over, view)
import Distribution.Package (packageName, packageVersion)
import Distribution.Types.PackageId (PackageId)

import Distribution.Client.Repository.Cache (Cache)

import qualified Hackage.Security.Client as Sec
import qualified Hackage.Security.Client.Repository.Cache as Sec.Cache
import qualified Hackage.Security.Client.Repository.Local as Sec.Local
import qualified Hackage.Security.Client.Repository.Remote as Sec.Remote
import qualified Hackage.Security.Util.Path as Sec
import qualified Hackage.Security.Util.Pretty as Sec

-- NOTE: We are going to keep RemoteRepo as representative of the user input syntax
-- We will turn it into Repo after parsing.

-- | Different kinds of repositories
--
-- NOTE: It is important that this type remains serializable.
-- TODO: Rename to Repository? and rename the typeclass?
data Repo where
  -- | Local repository, without index.
  -- TODO: Move LocalRepo under Repository.Local
  -- I need to separate it from parsing
  RepoLocalNoIndex :: Located LocalRepo -> Repo
  -- | Standard (unsecured) remote repositories
  RepoLegacy :: Located LegacyRepo -> Repo
  -- | Secure repositories
  RepoSecure :: Located SecureRepo -> Repo
  deriving (Show, Eq, Ord, Generic)

instance Binary Repo
instance Structured Repo

data Repo'Remote where
  Repo'Remote'LegacyRepo :: Located LegacyRepo -> Repo'Remote
  Repo'Remote'SecureRepo :: Located SecureRepo -> Repo'Remote

asRepo :: Repo'Remote -> Repo
asRepo (Repo'Remote'LegacyRepo locRepo) = RepoLegacy locRepo
asRepo (Repo'Remote'SecureRepo locRepo) = RepoSecure locRepo

maybeRepoRemote :: Repo -> Maybe Repo'Remote
maybeRepoRemote (RepoLocalNoIndex _) = Nothing
maybeRepoRemote (RepoLegacy repo) = Just (Repo'Remote'LegacyRepo repo)
maybeRepoRemote (RepoSecure repo) = Just (Repo'Remote'SecureRepo repo)

-- TODO: for compat
repoLocalDir :: Repo -> FilePath
repoLocalDir (RepoLocalNoIndex (Located dir _)) = dir
repoLocalDir (RepoLegacy (Located dir _)) = dir
repoLocalDir (RepoSecure (Located dir _)) = dir

-- | Get filename base (i.e. without file extension) for index-related files
--
-- /Secure/ cabal repositories use a new extended & incremental
-- @01-index.tar@. In order to avoid issues resulting from clobbering
-- new/old-style index data, we save them locally to different names.
--
-- Example: Use @indexBaseName repo <.> "tar.gz"@ to compute the 'FilePath' of the
-- @00-index.tar.gz@/@01-index.tar.gz@ file.
indexBaseName :: forall repo. RepositoryWithIndex repo => Located repo -> FilePath
indexBaseName (Located dir _repo) = dir </> indexBaseName' (Proxy :: Proxy repo)

data Located r = Located
  { repositoryCacheDir :: FilePath
  , repository :: r
  }
  deriving (Show, Eq, Ord, Generic)

instance Binary r => Binary (Located r)
instance Structured r => Structured (Located r)

--
-- class
--

class Repository r where
  type CacheType r

  repositoryName :: r -> RepoName

  readIndexCache :: Verbosity -> RepoContext -> Located r -> IO (CacheType r)

class Repository r => RepositoryWithIndex r where
  -- TODO: I am sure this will be unnecessary when we capture the right interface
  indexBaseName' :: Proxy r -> FilePath

class Repository r => RepositoryIsRemote r where
  _remoteRepositoryURI :: Lens' r URI

  remoteRepositoryURI :: r -> URI
  remoteRepositoryURI = view _remoteRepositoryURI

  remoteRepositoryShouldTryHttps :: r -> Bool

--
-- context
--

-- | Access to repositories
data RepoContext = RepoContext
  { repoContextRepos :: [Repo]
  -- ^ All user-specified repositories
  , repoContextGetTransport :: IO HttpTransport
  -- ^ Get the HTTP transport
  --
  -- The transport will be initialized on the first call to this function.
  --
  -- NOTE: It is important that we don't eagerly initialize the transport.
  -- Initializing the transport is not free, and especially in contexts where
  -- we don't know a priori whether or not we need the transport incurring
  -- the overhead of transport initialization on _every_ invocation
  -- (eg @cabal build@) is undesirable.
  , repoContextWithSecureRepo
      :: forall a
       . Located SecureRepo
      -> (forall down. Sec.Repository down -> IO a)
      -> IO a
  -- ^ Get the (initialized) secure repo
  , repoContextIgnoreExpiry :: Bool
  -- ^ Should we ignore expiry times (when checking security)?
  }

withRepoContext'
  :: Verbosity
  -> [RemoteRepo]
  -> [LocalRepo]
  -> FilePath
  -> Maybe String
  -> Maybe Bool
  -> [FilePath]
  -> (RepoContext -> IO a)
  -> IO a
withRepoContext'
  verbosity
  remoteRepos
  localNoIndexRepos
  sharedCacheDir
  httpTransport
  ignoreExpiry
  extraPaths = \callback -> do
    for_ localNoIndexRepos $ \local ->
      unless (FilePath.Posix.isAbsolute (localRepoPath local)) $
        warn verbosity $
          "file+noindex "
            ++ unRepoName (localRepoName local)
            ++ " repository path is not absolute; this is fragile, and not recommended"

    transportRef <- newMVar Nothing
    let httpLib =
          Sec.HTTP.transportAdapter
            verbosity
            (getTransport transportRef)
    initSecureRepos verbosity httpLib secureRemoteRepos $ \secureRepos' ->
      callback
        RepoContext
          { repoContextRepos = map asRepo allRemoteRepos ++ map RepoLocalNoIndex allLocalNoIndexRepos
          , repoContextGetTransport = getTransport transportRef
          , repoContextWithSecureRepo = withSecureRepo secureRepos'
          , repoContextIgnoreExpiry = fromMaybe False ignoreExpiry
          }
    where
      secureRemoteRepos :: [Located SecureRepo]
      secureRemoteRepos = [repo | RepoSecure repo <- map asRepo allRemoteRepos]

      parseRemoteRepo :: RemoteRepo -> Repo'Remote
      parseRemoteRepo RemoteRepo{..}
        | Just True <- remoteRepoSecure =
            Repo'Remote'SecureRepo $
              Located cacheDir $
                SecureRepo
                  { secureRepoName = remoteRepoName
                  , secureRepoURI = remoteRepoURI
                  , secureRepoRootKeys = remoteRepoRootKeys
                  , secureRepoKeyThreshold = remoteRepoKeyThreshold
                  , secureRepoShouldTryHttps = remoteRepoShouldTryHttps
                  }
        | otherwise =
            Repo'Remote'LegacyRepo $
              Located cacheDir $
                LegacyRepo
                  { legacyRepoName = remoteRepoName
                  , legacyRepoURI = remoteRepoURI
                  , legacyRepoShouldTryHttps = remoteRepoShouldTryHttps
                  }
        where
          cacheDir = sharedCacheDir </> unRepoName remoteRepoName

      allRemoteRepos :: [Repo'Remote]
      allRemoteRepos = map parseRemoteRepo remoteRepos

      allLocalNoIndexRepos :: [Located LocalRepo]
      allLocalNoIndexRepos =
        [ Located cacheDir local
        | local@LocalRepo{..} <- localNoIndexRepos
        , let cacheDir
                | localRepoSharedCache = sharedCacheDir </> localRepoCacheKey local
                | otherwise = localRepoPath
        ]

      getTransport :: MVar (Maybe HttpTransport) -> IO HttpTransport
      getTransport transportRef =
        modifyMVar transportRef $ \mTransport -> do
          transport <- case mTransport of
            Just tr -> return tr
            Nothing -> configureTransport verbosity extraPaths httpTransport
          return (Just transport, transport)

      withSecureRepo
        :: Map (Located SecureRepo) SomeSecRepository
        -> Located SecureRepo
        -> (forall down. Sec.Repository down -> IO a)
        -> IO a
      withSecureRepo secureRepos repo callback =
        case Map.lookup repo secureRepos of
          Just (SomeSecRepository secureRepo) -> callback secureRepo
          Nothing -> throwIO $ userError "repoContextWithSecureRepo: unknown repo"

data SomeSecRepository = forall down. SomeSecRepository (Sec.Repository down)

-- | Initialize the provided secure repositories
--
-- Assumed invariant: `remoteRepoSecure` should be set for all these repos.
-- TODO: check that types guarantee this
initSecureRepos
  :: forall a
   . Verbosity
  -> Sec.HTTP.HttpLib
  -> [Located SecureRepo]
  -> (Map (Located SecureRepo) SomeSecRepository -> IO a)
  -> IO a
initSecureRepos verbosity httpLib repos callback = go Map.empty repos
  where
    go :: Map (Located SecureRepo) SomeSecRepository -> [Located SecureRepo] -> IO a
    go !acc [] = callback acc
    go !acc (r : rs) = do
      initSecureRepo verbosity httpLib r $ \r' ->
        go (Map.insert r (SomeSecRepository r') acc) rs

-- | Initialize the given secure repo
--
-- The security library has its own concept of a "local" repository, distinct
-- from @cabal-install@'s; these are secure repositories, but live in the local
-- file system. We use the convention that these repositories are identified by
-- URLs of the form @file:/path/to/local/repo@.
initSecureRepo
  :: Verbosity
  -> Sec.HTTP.HttpLib
  -> Located SecureRepo
  -- ^ Secure repo ('remoteRepoSecure' assumed)
  -> (forall down. Sec.Repository down -> IO a)
  -- ^ Callback
  -> IO a
initSecureRepo verbosity httpLib (Located cacheDir SecureRepo{..}) = \callback -> do
  cachePath <- Sec.makeAbsolute $ Sec.fromFilePath cacheDir

  let cache :: Sec.Cache.Cache
      cache =
        Sec.Cache.Cache
          { cacheRoot = cachePath
          , cacheLayout =
              Sec.cabalCacheLayout
                { Sec.cacheLayoutIndexTar = cacheFn "01-index.tar"
                , Sec.cacheLayoutIndexIdx = cacheFn "01-index.tar.idx"
                , Sec.cacheLayoutIndexTarGz = cacheFn "01-index.tar.gz"
                }
          }

  requiresBootstrap <- withRepo [] cache Sec.requiresBootstrap

  mirrors <-
    if requiresBootstrap
      then do
        info verbosity $
          "Trying to locate mirrors via DNS for "
            ++ "initial bootstrap of secure "
            ++ "repository '"
            ++ show secureRepoURI
            ++ "' ..."

        Sec.DNS.queryBootstrapMirrors verbosity secureRepoURI
      else pure []

  withRepo mirrors cache $ \r -> do
    when requiresBootstrap $
      Sec.uncheckClientErrors $
        Sec.bootstrap
          r
          (map Sec.KeyId secureRepoRootKeys)
          (Sec.KeyThreshold (fromIntegral secureRepoKeyThreshold))
    callback $ r
  where
    -- Initialize local or remote repo depending on the URI
    withRepo :: [URI] -> Sec.Cache.Cache -> (forall down. Sec.Repository down -> IO a) -> IO a
    withRepo _ cache callback | uriScheme secureRepoURI == "file:" = do
      dir <- Sec.makeAbsolute $ Sec.fromFilePath (uriPath secureRepoURI)
      Sec.Local.withRepository
        dir
        cache
        Sec.hackageRepoLayout
        Sec.hackageIndexLayout
        logTUF
        callback
    withRepo mirrors cache callback =
      Sec.Remote.withRepository
        httpLib
        (secureRepoURI : mirrors)
        Sec.Remote.defaultRepoOpts
        cache
        Sec.hackageRepoLayout
        Sec.hackageIndexLayout
        logTUF
        callback

    cacheFn :: FilePath -> Sec.CachePath
    cacheFn = Sec.rootPath . Sec.fragment

    -- We display any TUF progress only in verbose mode, including any transient
    -- verification errors. If verification fails, then the final exception that
    -- is thrown will of course be shown.
    logTUF :: Sec.LogMessage -> IO ()
    logTUF = info verbosity . Sec.pretty

--
-- secure
--

-- NOTE: we are going to use RemoteRepo as representative of the repository syntax
-- but turn it into SomeRepo when we need to work with it

-- | A secure repository
data SecureRepo = SecureRepo
  -- NOTE: mostly copied from RemoteRepo
  { secureRepoName :: RepoName
  , secureRepoURI :: URI
  , -- 'Nothing' here represents "whatever the default is"; this is important
    -- to allow for a smooth transition from opt-in to opt-out security
    -- (once we switch to opt-out, all access to the central Hackage
    -- repository should be secure by default)
    secureRepoRootKeys :: [String]
  -- ^ Root key IDs (for bootstrapping)
  , secureRepoKeyThreshold :: Int
  -- ^ Threshold for verification during bootstrapping
  , secureRepoShouldTryHttps :: Bool
  -- ^ Normally a repo just specifies an HTTP or HTTPS URI, but as a
  -- special case we may know a repo supports both and want to try HTTPS
  -- if we can, but still allow falling back to HTTP.
  --
  -- This field is not currently stored in the config file, but is filled
  -- in automagically for known repos.
  }
  deriving (Show, Eq, Ord, Generic)

instance Binary SecureRepo
instance Structured SecureRepo

instance Repository SecureRepo where
  type CacheType SecureRepo = Cache

  repositoryName = secureRepoName

instance RepositoryWithIndex SecureRepo where
  indexBaseName' _proxy = "01-index"

instance RepositoryIsRemote SecureRepo where
  _remoteRepositoryURI f s =
    fmap (\x -> s{secureRepoURI = x}) (f (secureRepoURI s))
  remoteRepositoryShouldTryHttps = secureRepoShouldTryHttps

--
-- legacy
--

-- | A legacy repository
data LegacyRepo = LegacyRepo
  { legacyRepoName :: RepoName
  , legacyRepoURI :: URI
  , legacyRepoShouldTryHttps :: Bool
  -- ^ Normally a repo just specifies an HTTP or HTTPS URI, but as a
  -- special case we may know a repo supports both and want to try HTTPS
  -- if we can, but still allow falling back to HTTP.
  --
  -- This field is not currently stored in the config file, but is filled
  -- in automagically for known repos.
  }
  deriving (Show, Eq, Ord, Generic)

instance Binary LegacyRepo
instance Structured LegacyRepo

instance Repository LegacyRepo where
  repositoryName = legacyRepoName

instance RepositoryWithIndex LegacyRepo where
  indexBaseName' _proxy = "00-index"

instance RepositoryIsRemote LegacyRepo where
  _remoteRepositoryURI f s =
    fmap (\x -> s{legacyRepoURI = x}) (f (legacyRepoURI s))
  remoteRepositoryShouldTryHttps = legacyRepoShouldTryHttps

--
-- remote repo utilities
--

remoteRepoCheckHttps :: RepositoryIsRemote r => Verbosity -> HttpTransport -> r -> IO ()
remoteRepoCheckHttps verbosity transport repo
  | uriScheme (remoteRepositoryURI repo) == "https:"
  , not (transportSupportsHttps transport) =
      die' verbosity $
        "The remote repository '"
          ++ unRepoName (repositoryName repo)
          ++ "' specifies a URL that "
          ++ requiresHttpsErrorMessage
  | otherwise = return ()

remoteRepoTryUpgradeToHttps :: RepositoryIsRemote r => Verbosity -> HttpTransport -> r -> IO r
remoteRepoTryUpgradeToHttps verbosity transport repo
  | remoteRepositoryShouldTryHttps repo
  , uriScheme (remoteRepositoryURI repo) == "http:"
  , not (transportSupportsHttps transport)
  , not (transportManuallySelected transport) =
      die' verbosity $
        "The builtin HTTP implementation does not support HTTPS, but using "
          ++ "HTTPS for authenticated uploads is recommended. "
          ++ "The transport implementations with HTTPS support are "
          ++ intercalate ", " [name | (name, _, True, _) <- supportedTransports]
          ++ "but they require the corresponding external program to be "
          ++ "available. You can either make one available or use plain HTTP by "
          ++ "using the global flag --http-transport=plain-http (or putting the "
          ++ "equivalent in the config file). With plain HTTP, your password "
          ++ "is sent using HTTP digest authentication so it cannot be easily "
          ++ "intercepted, but it is not as secure as using HTTPS."
  | remoteRepositoryShouldTryHttps repo
  , uriScheme (remoteRepositoryURI repo) == "http:"
  , transportSupportsHttps transport =
      return $ over _remoteRepositoryURI (\uri -> uri{uriScheme = "https:"}) repo
  | otherwise =
      return repo

-- | Generate the URI of the tarball for a given package.
-- TODO: this should be a method of the RepositoryIsRemote class
packageURI :: RepositoryIsRemote r => r -> PackageId -> URI
packageURI repo pkgid
  | isOldHackageURI (remoteRepositoryURI repo) =
      (remoteRepositoryURI repo)
        { uriPath =
            FilePath.Posix.joinPath
              [ uriPath (remoteRepositoryURI repo)
              , prettyShow (packageName pkgid)
              , prettyShow (packageVersion pkgid)
              , prettyShow pkgid FilePath.Posix.<.> "tar.gz"
              ]
        }
packageURI repo pkgid =
  (remoteRepositoryURI repo)
    { uriPath =
        FilePath.Posix.joinPath
          [ uriPath (remoteRepositoryURI repo)
          , "package"
          , prettyShow pkgid FilePath.Posix.<.> "tar.gz"
          ]
    }

-- | Utility function for legacy support.
isOldHackageURI :: URI -> Bool
isOldHackageURI uri =
  case uriAuthority uri of
    Just (URIAuth{uriRegName = "hackage.haskell.org"}) ->
      FilePath.Posix.splitDirectories (uriPath uri)
        == ["/", "packages", "archive"]
    _ -> False
