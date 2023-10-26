{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Client.Types.PackageLocation
  ( PackageLocation (..)
  , UnresolvedPkgLoc
  , ResolvedPkgLoc
  , UnresolvedSourcePackage
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Types.PackageId (PackageId)

import Distribution.Client.Repository
import Distribution.Client.Types.SourceRepo (SourceRepoMaybe)
import Distribution.Solver.Types.SourcePackage (SourcePackage)

type UnresolvedPkgLoc = PackageLocation (Maybe FilePath)

type ResolvedPkgLoc = PackageLocation FilePath

data PackageLocation local
  = -- | An unpacked package in the given dir, or current dir
    LocalUnpackedPackage FilePath
  | -- | A package as a tarball that's available as a local tarball
    LocalTarballPackage FilePath
  | -- | A package as a tarball from a remote URI
    RemoteTarballPackage URI local
  | -- | A package available as a tarball from a repository.
    RepoTarballPackage Repo PackageId local
  | -- | A package available from a version control system source repository
    RemoteSourceRepoPackage SourceRepoMaybe local
  deriving (Show, Eq, Ord, Functor, Generic, Typeable)

instance Binary local => Binary (PackageLocation local)
instance Structured local => Structured (PackageLocation local)

-- | Convenience alias for 'SourcePackage UnresolvedPkgLoc'.
type UnresolvedSourcePackage = SourcePackage UnresolvedPkgLoc
