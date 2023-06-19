{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Distribution.Client.Types.Repo
  ( -- * Remote repository
    RemoteRepo (..),
    -- , emptyRemoteRepo

    RemoteRepoLegacy (..),
    RemoteRepoSecure (..),
    SomeRemoteRepo(..),

    -- * Local repository (no-index)
    LocalRepo (..),
    emptyLocalRepo,
    localRepoCacheKey,

    -- * Repository
    Repo (..),
    repoName,
    isRepoRemote,
    -- , maybeRepoRemote
  )
where

import qualified Data.ByteString.Lazy.Char8 as LBS
import Distribution.Client.Compat.Prelude
import Distribution.Client.HashValue (hashValue, showHashValue, truncateHash)
import Distribution.Client.Types.RepoName
import qualified Distribution.Compat.CharParsing as P
import Distribution.Compat.Lens (Lens', view)
import Distribution.Simple.Utils (toUTF8BS)
import Network.URI (URI (..), uriToString)
import qualified Text.PrettyPrint as Disp
import Prelude ()

-------------------------------------------------------------------------------
-- Remote repository
-------------------------------------------------------------------------------

data RemoteRepoLegacy = RemoteRepoLegacy {
    rrlName :: RepoName,
    rrlUri :: URI,
    rrlShouldTryHttps :: Bool
  }
  deriving (Show, Eq, Ord, Generic)

instance Binary RemoteRepoLegacy

instance Structured RemoteRepoLegacy

data RemoteRepoSecure = RemoteRepoSecure {
  rrsName :: RepoName,
  rrsURI :: URI,
  -- | FIXME: doc
  rrsShouldTryHttps :: Bool,
  -- | FIXME: doc
  rrsRootKeys :: [String],
  -- | Threshold for verification during bootstrapping
  rrsKeyThreshold :: Int
  }
  deriving (Show, Eq, Ord, Generic)

instance Binary RemoteRepoSecure

instance Structured RemoteRepoSecure

class RemoteRepo r where
  remoteRepoName :: r -> RepoName
  remoteRepoName = view remoteRepoName'

  remoteRepoURI :: r -> URI
  remoteRepoURI = view remoteRepoURI'

  remoteRepoName' :: Lens' r RepoName
  remoteRepoURI' :: Lens' r URI

  remoteRepoShouldTryHttps :: r -> Bool

instance RemoteRepo RemoteRepoLegacy where
  remoteRepoName' f r =
    fmap (\x -> r { rrlName = x }) (f (rrlName r))

  remoteRepoURI' f r =
    fmap (\x -> r { rrlUri = x }) (f (rrlUri r))

  remoteRepoShouldTryHttps = rrlShouldTryHttps

instance RemoteRepo RemoteRepoSecure where
  remoteRepoName' f r =
    fmap (\x -> r { rrsName = x }) (f (rrsName r))

  remoteRepoURI' f r =
    fmap (\x -> r { rrsURI = x }) (f (rrsURI r))

  remoteRepoShouldTryHttps = rrsShouldTryHttps

prettyRemoteRepo :: (RemoteRepo r) => r -> Disp.Doc
prettyRemoteRepo r =
  pretty (remoteRepoName r)
    <<>> Disp.colon
    <<>> Disp.text (uriToString id (remoteRepoURI r) [])

instance Pretty RemoteRepoLegacy where
  pretty = prettyRemoteRepo

instance Pretty RemoteRepoSecure where
  pretty = prettyRemoteRepo

data SomeRemoteRepo
  = LegacyRemoteRepo RemoteRepoLegacy
  | SecureRemoteRepo RemoteRepoSecure
  deriving (Show, Eq, Ord, Generic)

-- | Note: serialised format represents 'RemoteRepo' only partially.
-- instance Parsec RemoteRepo where
--   parsec = do
--     name <- parsec
--     _ <- P.char ':'
--     uriStr <- P.munch1 (\c -> isAlphaNum c || c `elem` ("+-=._/*()@'$:;&!?~" :: String))
--     uri <- maybe (fail $ "Cannot parse URI:" ++ uriStr) return (parseAbsoluteURI uriStr)
--     return
--       RemoteRepo
--         { remoteRepoName = name
--         , remoteRepoURI = uri
--         , remoteRepoSecure = Nothing
--         , remoteRepoRootKeys = []
--         , remoteRepoKeyThreshold = 0
--         , remoteRepoShouldTryHttps = False
--         }

-- | Construct a partial 'RemoteRepo' value to fold the field parser list over.
-- emptyRemoteRepo :: RepoName -> RemoteRepo
-- emptyRemoteRepo name = RemoteRepo name nullURI Nothing [] 0 False

-------------------------------------------------------------------------------
-- Local repository
-------------------------------------------------------------------------------

-- | /no-index/ style local repositories.
--
-- https://github.com/haskell/cabal/issues/6359
data LocalRepo = LocalRepo
  { localRepoName :: RepoName,
    localRepoPath :: FilePath,
    localRepoSharedCache :: Bool
  }
  deriving (Show, Eq, Ord, Generic)

instance Binary LocalRepo

instance Structured LocalRepo

-- | Note: doesn't parse 'localRepoSharedCache' field.
instance Parsec LocalRepo where
  parsec = do
    n <- parsec
    _ <- P.char ':'
    p <- P.munch1 (const True) -- restrict what can be a path?
    return (LocalRepo n p False)

instance Pretty LocalRepo where
  pretty (LocalRepo n p _) = pretty n <<>> Disp.colon <<>> Disp.text p

-- | Construct a partial 'LocalRepo' value to fold the field parser list over.
emptyLocalRepo :: RepoName -> LocalRepo
emptyLocalRepo name = LocalRepo name "" False

-- | Calculate a cache key for local-repo.
--
-- For remote repositories we just use name, but local repositories may
-- all be named "local", so we add a bit of `localRepoPath` into the
-- mix.
localRepoCacheKey :: LocalRepo -> String
localRepoCacheKey local = unRepoName (localRepoName local) ++ "-" ++ hashPart
  where
    hashPart =
      showHashValue $
        truncateHash 8 $
          hashValue $
            LBS.fromStrict $
              toUTF8BS $
                localRepoPath local

-------------------------------------------------------------------------------
-- Any repository
-------------------------------------------------------------------------------

-- | Different kinds of repositories
--
-- NOTE: It is important that this type remains serializable.
data Repo
  = -- | Local repository, without index.
    --
    -- https://github.com/haskell/cabal/issues/6359
    RepoLocalNoIndex
      { repoLocal :: LocalRepo,
        repoLocalDir :: FilePath
      }
  | -- | Standard (unsecured) remote repositories
    RepoRemoteLegacy
      { repoRemoteLegacy :: RemoteRepoLegacy,
        repoLocalDir :: FilePath
      }
  | -- | Secure repositories
    --
    -- Although this contains the same fields as 'RepoRemote', we use a separate
    -- constructor to avoid confusing the two.
    --
    -- Not all access to a secure repo goes through the hackage-security
    -- library currently; code paths that do not still make use of the
    -- 'repoRemote' and 'repoLocalDir' fields directly.
    RepoRemoteSecure
      { repoRemoteSecure :: RemoteRepoSecure,
        repoLocalDir :: FilePath
      }
  deriving (Show, Eq, Ord, Generic)

instance Binary Repo

instance Structured Repo

-- | Check if this is a remote repo
isRepoRemote :: Repo -> Bool
isRepoRemote RepoLocalNoIndex {} = False
isRepoRemote _ = True

-- -- | Extract @RemoteRepo@ from @Repo@ if remote.
-- maybeRepoRemote :: Repo -> Maybe RemoteRepo
-- maybeRepoRemote (RepoLocalNoIndex _ _localDir) = Nothing
-- maybeRepoRemote (RepoRemote r _localDir) = Just r
-- maybeRepoRemote (RepoSecure r _localDir) = Just r

repoName :: Repo -> RepoName
repoName (RepoLocalNoIndex r _) = localRepoName r
repoName (RepoRemoteLegacy r _) = remoteRepoName r
repoName (RepoRemoteSecure r _) = remoteRepoName r
