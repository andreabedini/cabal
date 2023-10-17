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

module Distribution.Client.Repository.Secure
  ( -- * Repository
    SecureRepo (..)
  , module Distribution.Client.Repository.Class
  )
where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.Repository.Class
  ( RepoName (..)
  , Repository (..)
  , RepositoryIsRemote (..)
  , URI (..)
  )

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
  repositoryName = secureRepoName

instance RepositoryIsRemote SecureRepo where
  _remoteRepositoryURI f s =
    fmap (\x -> s{secureRepoURI = x}) (f (secureRepoURI s))
  remoteRepositoryShouldTryHttps = secureRepoShouldTryHttps
