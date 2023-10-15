{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Distribution.Client.Repository
  ( -- * Repository
    Repo (..)
  , Repository (..)
  , RepositoryIsRemote (..)
  , RemoteRepository (..)
  , remoteRepositoryURI

    -- * Located
  , Located (..)
  , repoLocalDir

    -- * Repo context
  , RepoContext (..)
  , withRepoContext'

    -- * HTTP utilities
  , remoteRepoCheckHttps
  , remoteRepoTryUpgradeToHttps
  , isOldHackageURI
  , URI (..)
  , URIAuth (..)
  , module Distribution.Client.Types.Repo
  , module Distribution.Client.Types.RepoName
  , module Data.Some
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
import Data.GADT.Show
import qualified Data.Map as Map
import Data.Some
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

import Data.GADT.Compare
import Data.Type.Equality (TestEquality (testEquality), type (:~:) (..))
import Distribution.Compat.Lens (Lens', over, view)
import Distribution.Package (packageName, packageVersion)
import Distribution.Types.PackageId (PackageId)
import Distribution.Utils.Structured (Structured (..))
import qualified Hackage.Security.Client as Sec
import qualified Hackage.Security.Client.Repository.Cache as Sec
import qualified Hackage.Security.Client.Repository.Local as Sec.Local
import qualified Hackage.Security.Client.Repository.Remote as Sec.Remote
import qualified Hackage.Security.Util.Path as Sec
import qualified Hackage.Security.Util.Pretty as Sec

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

-- | Different kinds of repositories
--
-- NOTE: It is important that this type remains serializable.
-- TODO: Rename to Repository? and rename the typeclass?
data Repo r where
  -- | Local repository, without index.
  RepoLocalNoIndex :: Located LocalRepo -> Repo LocalRepo
  -- | Standard (unsecured) remote repositories
  RepoLegacy :: Located LegacyRepo -> Repo LegacyRepo
  -- | Secure repositories
  RepoSecure :: Located SecureRepo -> Repo SecureRepo

instance GShow Repo where
  gshowsPrec = defaultGshowsPrec

instance GEq Repo where
  geq (RepoLocalNoIndex lhs) (RepoLocalNoIndex rhs) | lhs == rhs = Just Refl
  geq (RepoLegacy lhs) (RepoLegacy rhs) | lhs == rhs = Just Refl
  geq (RepoSecure lhs) (RepoSecure rhs) | lhs == rhs = Just Refl
  geq _ _ = Nothing

instance GCompare Repo where
  gcompare (RepoLocalNoIndex x) (RepoLocalNoIndex y) | x < y = GLT
  gcompare (RepoLocalNoIndex x) (RepoLocalNoIndex y) | x == y = GEQ
  gcompare (RepoLocalNoIndex _) (RepoLocalNoIndex _) = GGT
  gcompare (RepoLocalNoIndex x) (RepoLegacy y) = GLT
  gcompare (RepoLocalNoIndex x) (RepoSecure y) = GLT
  gcompare (RepoLegacy x) (RepoLocalNoIndex y) = GGT
  gcompare (RepoLegacy x) (RepoLegacy y) | x < y = GLT
  gcompare (RepoLegacy x) (RepoLegacy y) | x == y = GEQ

deriving instance Show (Repo r)
deriving instance Eq (Repo r)
deriving instance Ord (Repo r)

-- deriving instance Generic (Repo r)

instance Binary (Repo r) where
  get = error "TODO"
  put = error "TODO"

instance Typeable r => Structured (Repo r) where
  structure = error "TODO"

instance Binary (Some Repo) where
  get = error "TODO"
  put = error "TODO"

instance Structured (Some Repo) where
  structure = error "TODO"

class Repository r where
  repositoryName :: r -> RepoName

instance Repository LocalRepo where
  repositoryName = localRepoName

instance Repository LegacyRepo where
  repositoryName = legacyRepoName

instance Repository SecureRepo where
  repositoryName = secureRepoName

class Repository r => RepositoryIsRemote r where
  _remoteRepositoryURI :: Lens' r URI
  remoteRepositoryShouldTryHttps :: r -> Bool

instance RepositoryIsRemote SecureRepo where
  _remoteRepositoryURI f s =
    fmap (\x -> s{secureRepoURI = x}) (f (secureRepoURI s))
  remoteRepositoryShouldTryHttps = secureRepoShouldTryHttps

instance RepositoryIsRemote LegacyRepo where
  _remoteRepositoryURI f s =
    fmap (\x -> s{legacyRepoURI = x}) (f (legacyRepoURI s))
  remoteRepositoryShouldTryHttps = legacyRepoShouldTryHttps

remoteRepositoryURI :: RepositoryIsRemote r => r -> URI
remoteRepositoryURI = view _remoteRepositoryURI

data RemoteRepository where
  RemoteRepository :: RepositoryIsRemote r => Repo r -> RemoteRepository

deriving instance Show RemoteRepository

-- deriving instance Eq RemoteRepository
-- deriving instance Ord RemoteRepository

-- instance GShow RemoteRepository where
--   gshowsPrec = defaultGshowsPrec

asRepo :: RemoteRepository -> Some Repo
asRepo (RemoteRepository repo) = Some repo

-- TODO: for compat
repoLocalDir :: Some Repo -> FilePath
repoLocalDir (Some (RepoLocalNoIndex (Located dir _))) = dir
repoLocalDir (Some (RepoLegacy (Located dir _))) = dir
repoLocalDir (Some (RepoSecure (Located dir _))) = dir

data Located r = Located
  { repositoryCacheDir :: FilePath
  , repository :: r
  }
  deriving (Show, Eq, Ord, Generic)

instance Binary r => Binary (Located r)
instance Structured r => Structured (Located r)

-- | Check if this is a remote repo
-- isRepoRemote :: Repo -> Bool
-- isRepoRemote RepoLocalNoIndex{} = False
-- isRepoRemote _ = True

-- | Access to repositories
data RepoContext = RepoContext
  { repoContextRepos :: [Some Repo]
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
      unless (FilePath.Posix.isAbsolute (localRepoPath local))
        $ warn verbosity
        $ "file+noindex "
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
          { repoContextRepos = map asRepo allRemoteRepos ++ map Some allLocalNoIndexRepos
          , repoContextGetTransport = getTransport transportRef
          , repoContextWithSecureRepo = withSecureRepo secureRepos'
          , repoContextIgnoreExpiry = fromMaybe False ignoreExpiry
          }
    where
      secureRemoteRepos :: [Located SecureRepo]
      secureRemoteRepos = [repo | (Some (RepoSecure repo)) <- map asRepo allRemoteRepos]

      parseRemoteRepo :: RemoteRepo -> RemoteRepository
      parseRemoteRepo RemoteRepo{..}
        | Just True <- remoteRepoSecure =
            RemoteRepository
              $ RepoSecure
              $ Located cacheDir
              $ SecureRepo
                { secureRepoName = remoteRepoName
                , secureRepoURI = remoteRepoURI
                , secureRepoRootKeys = remoteRepoRootKeys
                , secureRepoKeyThreshold = remoteRepoKeyThreshold
                , secureRepoShouldTryHttps = remoteRepoShouldTryHttps
                }
        | otherwise =
            RemoteRepository
              $ RepoLegacy
              $ Located cacheDir
              $ LegacyRepo
                { legacyRepoName = remoteRepoName
                , legacyRepoURI = remoteRepoURI
                , legacyRepoShouldTryHttps = remoteRepoShouldTryHttps
                }
        where
          cacheDir = sharedCacheDir </> unRepoName remoteRepoName

      allRemoteRepos :: [RemoteRepository]
      allRemoteRepos = map parseRemoteRepo remoteRepos

      allLocalNoIndexRepos :: [Repo LocalRepo]
      allLocalNoIndexRepos =
        [ RepoLocalNoIndex (Located cacheDir local)
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
        :: Map (Located SecureRepo) (Some Sec.Repository)
        -> Located SecureRepo
        -> (forall down. Sec.Repository down -> IO a)
        -> IO a
      withSecureRepo secureRepos repo callback =
        case Map.lookup repo secureRepos of
          Just (Some secureRepo) -> callback secureRepo
          Nothing -> throwIO $ userError "repoContextWithSecureRepo: unknown repo"

-- | Initialize the provided secure repositories
--
-- Assumed invariant: `remoteRepoSecure` should be set for all these repos.
initSecureRepos
  :: forall a
   . Verbosity
  -> Sec.HTTP.HttpLib
  -> [Located SecureRepo]
  -> (Map (Located SecureRepo) (Some Sec.Repository) -> IO a)
  -> IO a
initSecureRepos verbosity httpLib repos callback = go Map.empty repos
  where
    go :: Map (Located SecureRepo) (Some Sec.Repository) -> [Located SecureRepo] -> IO a
    go !acc [] = callback acc
    go !acc (r : rs) = do
      initSecureRepo verbosity httpLib r $ \r' ->
        go (Map.insert r (Some r') acc) rs

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

  let cache :: Sec.Cache
      cache =
        Sec.Cache
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
        info verbosity
          $ "Trying to locate mirrors via DNS for "
          ++ "initial bootstrap of secure "
          ++ "repository '"
          ++ show secureRepoURI
          ++ "' ..."

        Sec.DNS.queryBootstrapMirrors verbosity secureRepoURI
      else pure []

  withRepo mirrors cache $ \r -> do
    when requiresBootstrap
      $ Sec.uncheckClientErrors
      $ Sec.bootstrap
        r
        (map Sec.KeyId secureRepoRootKeys)
        (Sec.KeyThreshold (fromIntegral secureRepoKeyThreshold))
    callback $ r
  where
    -- Initialize local or remote repo depending on the URI
    withRepo :: [URI] -> Sec.Cache -> (forall down. Sec.Repository down -> IO a) -> IO a
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

remoteRepoCheckHttps :: RepositoryIsRemote r => Verbosity -> HttpTransport -> r -> IO ()
remoteRepoCheckHttps verbosity transport repo
  | uriScheme (remoteRepositoryURI repo) == "https:"
  , not (transportSupportsHttps transport) =
      die' verbosity
        $ "The remote repository '"
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
      die' verbosity
        $ "The builtin HTTP implementation does not support HTTPS, but using "
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
