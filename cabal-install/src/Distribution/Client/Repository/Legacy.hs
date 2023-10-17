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

module Distribution.Client.Repository.Legacy
  ( -- * Repository
    LegacyRepo (..)
  , module Distribution.Client.Repository.Class
  )
where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.Repository.Class
  ( RepoName
  , Repository (..)
  , RepositoryIsRemote (..)
  , URI
  )

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

instance RepositoryIsRemote LegacyRepo where
  _remoteRepositoryURI f s =
    fmap (\x -> s{legacyRepoURI = x}) (f (legacyRepoURI s))
  remoteRepositoryShouldTryHttps = legacyRepoShouldTryHttps
