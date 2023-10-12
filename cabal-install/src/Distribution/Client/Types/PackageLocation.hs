{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Client.Types.PackageLocation
  ( PackageLocation (..)
  , PkgInfo
  , UnresolvedPkgLoc
  , ResolvedPkgLoc
  , UnresolvedSourcePackage
  , ResolvedSourcePackage
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Network.URI (URI)

import Distribution.Types.PackageId (PackageId)

import Distribution.Client.HashValue (HashValue)
import Distribution.Client.Types.Repo
import Distribution.Client.Types.SourceRepo (SourceRepoMaybe)
import Distribution.Solver.Types.SourcePackage (SourcePackage)

-- | Where we have downloaded the package tarball and its hash
type PkgInfo = (HashValue, FilePath)

type UnresolvedPkgLoc = PackageLocation (Maybe PkgInfo)

type ResolvedPkgLoc = PackageLocation PkgInfo

-- TODO: maybe split this in local and remote
data PackageLocation local
  = -- | An unpacked package in the given dir, or current dir
    LocalUnpackedPackage FilePath
  | -- | A package as a tarball that's available as a local tarball
    LocalTarballPackage FilePath
  | -- | A package as a tarball from a remote URI
    RemoteTarballPackage URI local
  | -- | A package available as a tarball from a repository.
    --
    -- It may be from a local repository or from a remote repository, with a
    -- locally cached copy. ie a package available from hackage
    RepoTarballPackage Repo PackageId local
  | -- | A package available from a version control system source repository
    RemoteSourceRepoPackage SourceRepoMaybe local
  deriving (Show, Functor, Eq, Ord, Generic, Typeable)

instance Binary local => Binary (PackageLocation local)
instance Structured local => Structured (PackageLocation local)

-- | Convenience alias for 'SourcePackage UnresolvedPkgLoc'.
type UnresolvedSourcePackage = SourcePackage UnresolvedPkgLoc

-- | Convenience alias for 'SourcePackage ResolvedPkgLoc'.
type ResolvedSourcePackage = SourcePackage ResolvedPkgLoc
