{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.Client.GlobalFlags
  ( GlobalFlags (..)
  , defaultGlobalFlags
  , withRepoContext
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Simple.Setup
  ( Flag (..)
  , flagToMaybe
  , fromFlag
  )
import Distribution.Utils.NubList
  ( NubList
  , fromNubList
  )

import Distribution.Client.IndexUtils.ActiveRepos
  ( ActiveRepos
  )

import Distribution.Client.Repository

-- ------------------------------------------------------------

-- * Global flags

-- ------------------------------------------------------------

-- | Flags that apply at the top level, not to any sub-command.
data GlobalFlags = GlobalFlags
  { globalVersion :: Flag Bool
  , globalNumericVersion :: Flag Bool
  , globalConfigFile :: Flag FilePath
  , globalConstraintsFile :: Flag FilePath
  , globalRemoteRepos :: NubList RemoteRepo
  -- ^ Available Hackage servers.
  -- NOTE: this is the parsed type RemoteRepo
  , globalCacheDir :: Flag FilePath
  , globalLocalNoIndexRepos :: NubList LocalRepo
  -- ^ NOTE: this is the parsed type LocalRepo
  , globalActiveRepos :: Flag ActiveRepos
  , globalLogsDir :: Flag FilePath
  , globalIgnoreExpiry :: Flag Bool
  -- ^ Ignore security expiry dates
  , globalHttpTransport :: Flag String
  , globalNix :: Flag Bool
  -- ^ Integrate with Nix
  , globalStoreDir :: Flag FilePath
  , globalProgPathExtra :: NubList FilePath
  -- ^ Extra program path used for packagedb lookups in a global context (i.e. for http transports)
  }
  deriving (Show, Generic)

defaultGlobalFlags :: GlobalFlags
defaultGlobalFlags =
  GlobalFlags
    { globalVersion = Flag False
    , globalNumericVersion = Flag False
    , globalConfigFile = mempty
    , globalConstraintsFile = mempty
    , globalRemoteRepos = mempty
    , globalCacheDir = mempty
    , globalLocalNoIndexRepos = mempty
    , globalActiveRepos = mempty
    , globalLogsDir = mempty
    , globalIgnoreExpiry = Flag False
    , globalHttpTransport = mempty
    , globalNix = Flag False
    , globalStoreDir = mempty
    , globalProgPathExtra = mempty
    }

instance Monoid GlobalFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup GlobalFlags where
  (<>) = gmappend

withRepoContext :: Verbosity -> GlobalFlags -> (RepoContext -> IO a) -> IO a
withRepoContext verbosity globalFlags =
  withRepoContext'
    verbosity
    (fromNubList (globalRemoteRepos globalFlags))
    (fromNubList (globalLocalNoIndexRepos globalFlags))
    (fromFlag (globalCacheDir globalFlags))
    (flagToMaybe (globalHttpTransport globalFlags))
    (flagToMaybe (globalIgnoreExpiry globalFlags))
    (fromNubList (globalProgPathExtra globalFlags))
