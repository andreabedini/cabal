{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}

module Distribution.Client.Types.SourcePackageDb
  ( SourcePackageDb0(..)
  , SourcePackageDb
  , pattern SourcePackageDb
  , packageIndex
  , packagePreferences
  , lookupDependency
  , lookupPackageName
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Package (packageVersion)
import Distribution.Types.PackageName (PackageName)
import Distribution.Types.VersionRange (VersionRange, withinRange)

import Distribution.Client.Types.PackageLocation (UnresolvedPkgLoc, UnresolvedSourcePackage)
import Distribution.Solver.Types.PackageIndex (PackageIndex)
import qualified Distribution.Solver.Types.PackageIndex as PackageIndex
import Distribution.Solver.Types.SourcePackage (SourcePackage)

import qualified Data.Map as Map

-- | This is the information we get from a @00-index.tar.gz@ hackage index.
data SourcePackageDb0 loc = SourcePackageDb0
  { packageIndex0 :: PackageIndex (SourcePackage loc)
  , packagePreferences0 :: Map PackageName VersionRange
  }
  deriving (Eq, Generic)

instance Binary loc => Binary (SourcePackageDb0 loc)

type SourcePackageDb = SourcePackageDb0 UnresolvedPkgLoc

pattern SourcePackageDb :: PackageIndex UnresolvedSourcePackage -> Map PackageName VersionRange -> SourcePackageDb
pattern SourcePackageDb{packageIndex, packagePreferences} = SourcePackageDb0 packageIndex packagePreferences

-- | Does a case-sensitive search by package name and a range of versions.
--
-- We get back any number of versions of the specified package name, all
-- satisfying the version range constraint.
--
-- Additionally, `preferred-versions` (such as version deprecation) are
-- honoured in this lookup, which is the only difference to
-- 'PackageIndex.lookupDependency'
lookupDependency :: SourcePackageDb0 loc -> PackageName -> VersionRange -> [SourcePackage loc]
lookupDependency sourceDb pname version =
  filterPreferredVersions pref $ PackageIndex.lookupDependency (packageIndex0 sourceDb) pname version
  where
    pref = Map.lookup pname (packagePreferences0 sourceDb)

-- | Does a case-sensitive search by package name.
--
-- Additionally, `preferred-versions` (such as version deprecation) are
-- honoured in this lookup, which is the only difference to
-- 'PackageIndex.lookupPackageName'
lookupPackageName :: SourcePackageDb0 loc -> PackageName -> [SourcePackage loc]
lookupPackageName sourceDb pname =
  filterPreferredVersions pref $ PackageIndex.lookupPackageName (packageIndex0 sourceDb) pname
  where
    pref = Map.lookup pname (packagePreferences0 sourceDb)

-- | @filterPreferredVersions 'range' 'versions'@.
-- If a 'range' is given, only keep versions that satisfy the range.
-- If 'range' is 'Nothing', all versions are kept.
--
-- The 'range' is expected to be obtained from the 'SourcePackageDb.packagePreferences'.
filterPreferredVersions :: Maybe VersionRange -> [SourcePackage loc] -> [SourcePackage loc]
filterPreferredVersions Nothing versions = versions
filterPreferredVersions (Just range) versions = filter ((`withinRange` range) . packageVersion) versions
