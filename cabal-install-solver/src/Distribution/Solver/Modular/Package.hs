{-# LANGUAGE DeriveFunctor #-}
module Distribution.Solver.Modular.Package
  ( I(..)
  , Loc(..)
  , PackageId
  , PackageIdentifier(..)
  , PackageName, mkPackageName, unPackageName
  , PkgconfigName, mkPkgconfigName, unPkgconfigName
  , PI(..)
  , PN
  , QPV
  , instI
  , showI
  , showPI
  , unPN
  ) where

import Prelude ()
import Distribution.Solver.Compat.Prelude

import Distribution.Package -- from Cabal
import Distribution.Pretty (prettyShow)

import Distribution.Solver.Modular.Version
import Distribution.Solver.Types.PackagePath
import Distribution.Solver.Types.Stage (Stage, showStage)

-- | A package name.
type PN = PackageName

-- | Unpacking a package name.
unPN :: PN -> String
unPN = unPackageName

-- | Package version. A package name plus a version number.
type PV = PackageId

-- | Qualified package version.
type QPV = Qualified PV

-- | Package id. Currently just a black-box string.
type PId = UnitId

-- | Location. Info about whether a package is installed or not, and where
-- exactly it is located. For installed packages, uniquely identifies the
-- package instance via its 'PId'.
--
-- TODO: More information is needed about the repo.
data Loc = Inst PId | InRepo PackageName
  deriving (Eq, Ord, Show)

-- | Instance. A version number and a location.
data I = I Stage Ver Loc
  deriving (Eq, Ord, Show)

-- | String representation of an instance.
showI :: I -> String
showI (I s v (InRepo pn)) = prettyShow (PackageIdentifier pn v) ++ " (source, " ++ showStage s ++ ")"
showI (I s _v (Inst uid)) = prettyShow uid ++ " (installed, " ++ showStage s ++ ")"

-- | Package instance. A package name and an instance.
data PI qpn = PI qpn I
  deriving (Eq, Ord, Show, Functor)

-- | String representation of a package instance.
showPI :: PI QPN -> String
showPI (PI qpn i) = showQPN qpn ++ "-" ++ showI i

instI :: I -> Bool
instI (I _ _ (Inst _)) = True
instI _              = False
