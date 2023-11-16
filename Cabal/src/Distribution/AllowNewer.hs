{-# LANGUAGE DeriveGeneric #-}

-- | Utilities to relax version bounds on dependencies
module Distribution.AllowNewer
  ( RelaxDeps (..),
    RelaxedDep (..),
    RelaxDepMod (..),
    RelaxKind (..),
    removeBound,
    -- , mkRelaxDeps
  )
where

import qualified Distribution.Compat.CharParsing as P
import Distribution.Compat.Prelude
import Distribution.Parsec (Parsec (parsec), parsecLeadingCommaNonEmpty)
import Distribution.Pretty (Pretty (pretty))
import Distribution.Types.PackageName (PackageName, mkPackageName)
import Distribution.Version
import qualified Text.PrettyPrint as Disp

data RelaxKind = RelaxLower | RelaxUpper

-- $setup
-- >>> import Distribution.Parsec

-- TODO: When https://github.com/haskell/cabal/issues/4203 gets tackled,
-- it may make sense to move these definitions to the Solver.Types
-- module

-- | FIXME: Dependencies can be relaxed either for all packages in the install plan, or
-- only for some packages.
data RelaxDeps = RelaxDeps (Maybe RelaxDepMod) [RelaxedDep]
  deriving (Eq, Read, Show, Generic)

instance Binary RelaxDeps

instance Structured RelaxDeps

instance Semigroup RelaxDeps where
  RelaxDeps mdef deps <> RelaxDeps mdef' deps' =
    RelaxDeps (mdef <> mdef') (deps <> deps')

-- FIXME: adjust these tests

-- |
--
-- >>> simpleParsec "all" :: Maybe RelaxDeps
-- Just RelaxDepsAll
--
-- >>> simpleParsec "none" :: Maybe RelaxDeps
-- Just (RelaxDepsSome [])
--
-- >>> simpleParsec "*, *" :: Maybe RelaxDeps
-- Just RelaxDepsAll
--
-- >>> simpleParsec "*:*" :: Maybe RelaxDeps
-- Just RelaxDepsAll
--
-- >>> simpleParsec "foo:bar, quu:puu" :: Maybe RelaxDeps
-- Just (RelaxDepsSome [RelaxedDep (RelaxDepScopePackage (PackageName "foo")) RelaxDepModNone (RelaxDepSubjectPkg (PackageName "bar")),RelaxedDep (RelaxDepScopePackage (PackageName "quu")) RelaxDepModNone (RelaxDepSubjectPkg (PackageName "puu"))])
--
-- This is not a glitch, even it looks like:
--
-- >>> simpleParsec ", all" :: Maybe RelaxDeps
-- Just RelaxDepsAll
--
-- >>> simpleParsec "" :: Maybe RelaxDeps
-- Nothing
instance Parsec RelaxDeps where
  parsec = do
    xs <- parsecLeadingCommaNonEmpty parsec
    -- FIXME: handle "all"
    pure $ RelaxDeps Nothing (toList xs)

-- mkRelaxDeps :: NE.NonEmpty RelaxedDep -> RelaxDeps
-- mkRelaxDeps xs = RelaxDeps $
--   case find (\(RelaxedDep _ pn) -> pn == mkPackageName "all") xs of
--     Nothing -> Right xs
--     Just (RelaxedDep rdm _pn) -> Left rdm

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

instance Semigroup RelaxDepMod where
  -- FIXME: who wins here?
  lhs <> rhs = lhs

instance Pretty RelaxedDep where
  pretty (RelaxedDep RelaxDepModNone subj) = pretty subj
  pretty (RelaxedDep RelaxDepModCaret subj) = Disp.char '^' Disp.<> pretty subj

instance Parsec RelaxedDep where
  parsec = RelaxedDep <$> modP <*> parsec

modP :: (P.CharParsing m) => m RelaxDepMod
modP = RelaxDepModCaret <$ P.char '^' <|> pure RelaxDepModNone

removeBound :: RelaxKind -> RelaxDepMod -> VersionRange -> VersionRange
removeBound RelaxLower RelaxDepModNone = removeLowerBound
removeBound RelaxUpper RelaxDepModNone = removeUpperBound
removeBound RelaxLower RelaxDepModCaret = transformCaretLower
removeBound RelaxUpper RelaxDepModCaret = transformCaretUpper
