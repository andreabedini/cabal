module Distribution.Client.Types.Repo
  ( -- * Remote repository
    RemoteRepo (..)
  , emptyRemoteRepo
  , remoteRepoKeyThresholdLens
  , remoteRepoRootKeysLens
  , remoteRepoSecureLens
  , remoteRepoURILens

    -- * Local repository (no-index)
  , LocalRepo (..)
  , emptyLocalRepo
  , localRepoCacheKey

    -- * Repositories, refined by kind
  , NoIndexRepo (..)
  , LegacyRepo (..)
  , SecureRepo (..)

    -- * Repository
  , Repo (..)
  , repoName
  , repoLocalDir
  , isRepoRemote
  , maybeRepoRemote

    -- * Windows
  , asPosixPath
  , normaliseFileNoIndexURI
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Network.URI (URI (..), nullURI, parseAbsoluteURI, uriToString)

import Distribution.Simple.Utils (toUTF8BS)
import Distribution.System (OS (Windows))

import Distribution.Client.HashValue (hashValue, showHashValue, truncateHash)
import Distribution.Compat.Lens

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp

import Distribution.Client.Types.RepoName

import qualified System.FilePath.Posix as Posix
import qualified System.FilePath.Windows as Windows

-------------------------------------------------------------------------------
-- Remote repository
-------------------------------------------------------------------------------

data RemoteRepo = RemoteRepo
  { remoteRepoName :: RepoName
  , remoteRepoURI :: URI
  , remoteRepoSecure :: Maybe Bool
  -- ^ Enable secure access?
  --
  -- 'Nothing' here represents "whatever the default is"; this is important
  -- to allow for a smooth transition from opt-in to opt-out security
  -- (once we switch to opt-out, all access to the central Hackage
  -- repository should be secure by default)
  , remoteRepoRootKeys :: [String]
  -- ^ Root key IDs (for bootstrapping)
  , remoteRepoKeyThreshold :: Int
  -- ^ Threshold for verification during bootstrapping
  , remoteRepoShouldTryHttps :: Bool
  -- ^ Normally a repo just specifies an HTTP or HTTPS URI, but as a
  -- special case we may know a repo supports both and want to try HTTPS
  -- if we can, but still allow falling back to HTTP.
  --
  -- This field is not currently stored in the config file, but is filled
  -- in automagically for known repos.
  }
  deriving (Show, Eq, Ord, Generic)

instance Binary RemoteRepo
instance NFData RemoteRepo
instance Structured RemoteRepo

instance Pretty RemoteRepo where
  pretty r =
    pretty (remoteRepoName r)
      <<>> Disp.colon
      <<>> Disp.text (uriToString id (remoteRepoURI r) [])

-- | Note: serialised format represents 'RemoteRepo' only partially.
instance Parsec RemoteRepo where
  parsec = do
    name <- parsec
    _ <- P.char ':'
    uriStr <- P.munch1 (\c -> isAlphaNum c || c `elem` ("+-=._/*()@'$:;&!?~" :: String))
    uri <- maybe (fail $ "Cannot parse URI:" ++ uriStr) return (parseAbsoluteURI uriStr)
    return
      RemoteRepo
        { remoteRepoName = name
        , remoteRepoURI = uri
        , remoteRepoSecure = Nothing
        , remoteRepoRootKeys = []
        , remoteRepoKeyThreshold = 0
        , remoteRepoShouldTryHttps = False
        }

-- | Construct a partial 'RemoteRepo' value to fold the field parser list over.
emptyRemoteRepo :: RepoName -> RemoteRepo
emptyRemoteRepo name = RemoteRepo name nullURI Nothing [] 0 False

remoteRepoURILens :: Lens' RemoteRepo URI
remoteRepoURILens f s = fmap (\x -> s{remoteRepoURI = x}) (f (remoteRepoURI s))
{-# INLINE remoteRepoURILens #-}

remoteRepoSecureLens :: Lens' RemoteRepo (Maybe Bool)
remoteRepoSecureLens f s = fmap (\x -> s{remoteRepoSecure = x}) (f (remoteRepoSecure s))
{-# INLINE remoteRepoSecureLens #-}

remoteRepoRootKeysLens :: Lens' RemoteRepo [String]
remoteRepoRootKeysLens f s = fmap (\x -> s{remoteRepoRootKeys = x}) (f (remoteRepoRootKeys s))
{-# INLINE remoteRepoRootKeysLens #-}

remoteRepoKeyThresholdLens :: Lens' RemoteRepo Int
remoteRepoKeyThresholdLens f s = fmap (\x -> s{remoteRepoKeyThreshold = x}) (f (remoteRepoKeyThreshold s))
{-# INLINE remoteRepoKeyThresholdLens #-}

-------------------------------------------------------------------------------
-- Local repository
-------------------------------------------------------------------------------

-- | /no-index/ style local repositories.
--
-- https://github.com/haskell/cabal/issues/6359
data LocalRepo = LocalRepo
  { localRepoName :: RepoName
  , localRepoPath :: FilePath
  , localRepoSharedCache :: Bool
  }
  deriving (Show, Eq, Ord, Generic)

instance Binary LocalRepo
instance NFData LocalRepo
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
-- Repositories, refined by kind
-------------------------------------------------------------------------------

-- | A @file+noindex@ repository: a directory of package tarballs (and
-- optionally a @preferred-versions@ file), without an index tarball.
--
-- https://github.com/haskell/cabal/issues/6359
data NoIndexRepo = NoIndexRepo
  { noIndexRepoLocal :: LocalRepo
  , noIndexRepoCacheDir :: FilePath
  -- ^ Where we keep the @noindex.cache@ file. This is either the
  -- repository directory itself or a directory in the shared cache, see
  -- 'localRepoSharedCache'.
  }
  deriving (Show, Eq, Ord, Generic)

instance Binary NoIndexRepo
instance NFData NoIndexRepo
instance Structured NoIndexRepo

-- | A legacy (unsecured) remote repository, using the @00-index.tar@ format.
--
-- These indices carry no timestamps, so index-states are not supported.
data LegacyRepo = LegacyRepo
  { legacyRepoRemote :: RemoteRepo
  , legacyRepoCacheDir :: FilePath
  -- ^ Where we keep the downloaded index and package tarballs.
  }
  deriving (Show, Eq, Ord, Generic)

instance Binary LegacyRepo
instance NFData LegacyRepo
instance Structured LegacyRepo

-- | A secure remote repository, managed via @hackage-security@ and using the
-- incremental @01-index.tar@ format.
--
-- TODO: Not all access to a secure repo goes through the hackage-security
-- library currently; code paths that do not still make use of the
-- 'secureRepoRemote' and 'secureRepoCacheDir' fields directly.
data SecureRepo = SecureRepo
  { secureRepoRemote :: RemoteRepo
  , secureRepoCacheDir :: FilePath
  -- ^ Where we keep the downloaded index, TUF metadata and package tarballs.
  }
  deriving (Show, Eq, Ord, Generic)

instance Binary SecureRepo
instance NFData SecureRepo
instance Structured SecureRepo

-------------------------------------------------------------------------------
-- Any repository
-------------------------------------------------------------------------------

-- | Different kinds of repositories
--
-- Code that only makes sense for one kind of repository should take the
-- corresponding refined type ('NoIndexRepo', 'LegacyRepo' or 'SecureRepo')
-- rather than 'Repo', so that the case analysis happens once at the call site
-- instead of being repeated (and partially handled) in every helper.
--
-- NOTE: It is important that this type remains serializable.
data Repo
  = -- | Local repository, without index.
    RepoLocalNoIndex NoIndexRepo
  | -- | Standard (unsecured) remote repositories
    RepoRemote LegacyRepo
  | -- | Secure repositories
    RepoSecure SecureRepo
  deriving (Show, Eq, Ord, Generic)

instance Binary Repo
instance NFData Repo
instance Structured Repo

-- | Check if this is a remote repo
isRepoRemote :: Repo -> Bool
isRepoRemote RepoLocalNoIndex{} = False
isRepoRemote _ = True

-- | Extract @RemoteRepo@ from @Repo@ if remote.
maybeRepoRemote :: Repo -> Maybe RemoteRepo
maybeRepoRemote (RepoLocalNoIndex _) = Nothing
maybeRepoRemote (RepoRemote r) = Just (legacyRepoRemote r)
maybeRepoRemote (RepoSecure r) = Just (secureRepoRemote r)

repoName :: Repo -> RepoName
repoName (RepoLocalNoIndex r) = localRepoName (noIndexRepoLocal r)
repoName (RepoRemote r) = remoteRepoName (legacyRepoRemote r)
repoName (RepoSecure r) = remoteRepoName (secureRepoRemote r)

-- | The directory where cabal keeps local state for the repository (index
-- caches, downloaded tarballs, build reports, ...).
repoLocalDir :: Repo -> FilePath
repoLocalDir (RepoLocalNoIndex r) = noIndexRepoCacheDir r
repoLocalDir (RepoRemote r) = legacyRepoCacheDir r
repoLocalDir (RepoSecure r) = secureRepoCacheDir r

-------------------------------------------------------------------------------

-- * Windows utils

-------------------------------------------------------------------------------

-- | When on Windows, we need to convert the paths in URIs to be POSIX-style.
--
-- >>> import Network.URI
-- >>> normaliseFileNoIndexURI Windows (URI "file+noindex:" (Just nullURIAuth) "C:\\dev\\foo" "" "")
-- file+noindex:C:/dev/foo
--
-- Other formats of file paths are not understood by @network-uri@:
--
-- >>> import Network.URI
-- >>> uriPath <$> parseURI "file+noindex://C:/foo.txt"
-- Just "/foo.txt"
-- >>> parseURI "file+noindex://C:\foo.txt"
-- Nothing
-- >>> uriPath <$> parseURI "file+noindex:///C:/foo.txt"
-- Just "/C:/foo.txt"
-- >>> uriPath <$> parseURI "file+noindex:C:/foo.txt"
-- Just "C:/foo.txt"
--
-- Out of the ones above, only the last one can be used from anywhere in the
-- system.
normaliseFileNoIndexURI :: OS -> URI -> URI
normaliseFileNoIndexURI os uri@(URI scheme _auth path query fragment)
  | "file+noindex:" <- scheme
  , Windows <- os =
      URI scheme Nothing (asPosixPath path) query fragment
  | otherwise = uri

-- | Convert a path to POSIX-style.
asPosixPath :: FilePath -> FilePath
asPosixPath p =
  -- We don't use 'isPathSeparator' because @Windows.isPathSeparator
  -- Posix.pathSeparator == True@.
  [if x == Windows.pathSeparator then Posix.pathSeparator else x | x <- p]
