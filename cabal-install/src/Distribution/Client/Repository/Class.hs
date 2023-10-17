module Distribution.Client.Repository.Class
  ( Repository (..)
  , RepositoryIsRemote (..)
  , module Distribution.Compat.Lens
  , module Distribution.Client.Types.RepoName
  , module Network.URI
  )
where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Network.URI (URI (..))

import Distribution.Compat.Lens (Lens')

import Distribution.Client.Types.RepoName (RepoName (..))

class Repository r where
  repositoryName :: r -> RepoName

  -- | Get filename base (i.e. without file extension) for index-related files
  --
  -- /Secure/ cabal repositories use a new extended & incremental
  -- @01-index.tar@. In order to avoid issues resulting from clobbering
  -- new/old-style index data, we save them locally to different names.
  --
  -- Example: Use @indexBaseName repo <.> "tar.gz"@ to compute the 'FilePath' of the
  -- @00-index.tar.gz@/@01-index.tar.gz@ file.
  indexBaseName :: Repo -> FilePath
  indexBaseName repo = repoLocalDir repo </> fn
    where
      fn = case repo of
        RepoSecure{} -> "01-index"
        RepoRemote{} -> "00-index"
        RepoLocalNoIndex{} -> "noindex"

class Repository r => RepositoryIsRemote r where
  _remoteRepositoryURI :: Lens' r URI
  remoteRepositoryShouldTryHttps :: r -> Bool
