-----------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}

-- | Utilities to relax version bounds on dependencies
module Distribution.AllowNewer
  ( AllowNewer (..)
  , AllowOlder (..)
  , RelaxDepMod (..)
  -- , RelaxDepSubject (..)
  , RelaxedDep (..)
  , RelaxedDeps (..)
  , RelaxKind (..)
  , removeBound
  , mkRelaxedDeps
  ) where

import Distribution.Compat.Prelude

import Distribution.Version

import Distribution.Parsec (Parsec (parsec))
import Distribution.Types.PackageName (PackageName, mkPackageName)

import qualified Data.List.NonEmpty as NE
import qualified Distribution.Compat.CharParsing as P
import Distribution.Pretty (Pretty (pretty))
import qualified Text.PrettyPrint as Disp

data RelaxKind = RelaxLower | RelaxUpper

-- $setup
-- >>> import Distribution.Parsec

-- TODO: When https://github.com/haskell/cabal/issues/4203 gets tackled,
-- it may make sense to move these definitions to the Solver.Types
-- module

-- | 'RelaxDeps' in the context of upper bounds (i.e. for @--allow-newer@ flag)
newtype AllowNewer = AllowNewer {unAllowNewer :: RelaxedDeps}
  deriving (Eq, Read, Show, Generic)

instance Binary AllowNewer
instance Structured AllowNewer

instance Semigroup AllowNewer where
  AllowNewer x <> AllowNewer y = AllowNewer (x <> y)

-- | 'RelaxDeps' in the context of lower bounds (i.e. for @--allow-older@ flag)
newtype AllowOlder = AllowOlder {unAllowOlder :: RelaxedDeps}
  deriving (Eq, Read, Show, Generic)

instance Binary AllowOlder
instance Structured AllowOlder

instance Semigroup AllowOlder where
  AllowOlder x <> AllowOlder y = AllowOlder (x <> y)

-- | Dependencies can be relaxed either for all packages in the install plan, or
-- only for some packages.
newtype RelaxedDeps = RelaxedDeps (Either RelaxDepMod (NE.NonEmpty RelaxedDep))
  deriving (Eq, Read, Show, Generic)

instance Binary RelaxedDeps
instance Structured RelaxedDeps

instance Semigroup RelaxedDeps where
  RelaxedDeps lhs <> RelaxedDeps rhs = RelaxedDeps (lhs <> rhs)

mkRelaxedDeps :: NE.NonEmpty RelaxedDep -> RelaxedDeps
mkRelaxedDeps xs = RelaxedDeps
  $ case find (\(RelaxedDep _ pn) -> pn == mkPackageName "all") xs of
    Nothing -> Right xs
    Just (RelaxedDep rdm _pn) -> Left rdm

data RelaxedDep = RelaxedDep !RelaxDepMod !PackageName
  deriving (Eq, Read, Show, Generic)

instance Binary RelaxedDep
instance Structured RelaxedDep

-- | Modifier for dependency relaxation
data RelaxDepMod
  = -- | Default semantics
    RelaxDepModNone
  | -- | Apply relaxation only to @^>=@ constraints
    RelaxDepModCaret
  deriving (Eq, Read, Show, Generic)

instance Binary RelaxDepMod
instance Structured RelaxDepMod

instance Pretty RelaxedDep where
  pretty (RelaxedDep RelaxDepModNone subj) = pretty subj
  pretty (RelaxedDep RelaxDepModCaret subj) = Disp.char '^' Disp.<> pretty subj

instance Parsec RelaxedDep where
  parsec = RelaxedDep <$> modP <*> parsec

modP :: P.CharParsing m => m RelaxDepMod
modP = RelaxDepModCaret <$ P.char '^' <|> pure RelaxDepModNone

removeBound :: RelaxKind -> RelaxDepMod -> VersionRange -> VersionRange
removeBound RelaxLower RelaxDepModNone = removeLowerBound
removeBound RelaxUpper RelaxDepModNone = removeUpperBound
removeBound RelaxLower RelaxDepModCaret = transformCaretLower
removeBound RelaxUpper RelaxDepModCaret = transformCaretUpper
