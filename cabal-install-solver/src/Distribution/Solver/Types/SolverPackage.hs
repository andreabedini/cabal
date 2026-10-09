module Distribution.Solver.Types.SolverPackage
    ( SolverPackage(..)
    ) where

import Distribution.Solver.Compat.Prelude
import Prelude ()

import Distribution.Package ( Package(..) )
import Distribution.PackageDescription ( FlagAssignment )
import Distribution.Solver.Types.ComponentDeps ( ComponentDeps )
import Distribution.Solver.Types.OptionalStanza
import Distribution.Solver.Types.SolverId
import Distribution.Solver.Types.SourcePackage

-- | A 'SolverPackage' is a package specified by the dependency solver.
-- It will get elaborated into a 'ConfiguredPackage' or even an
-- 'ElaboratedConfiguredPackage'.
--
-- NB: In cabal-install, 'SolverPackage's are always instantiated at
-- 'UnresolvedPkgLoc'. The parameter is kept so that the solver remains
-- parametric in the package location: it cannot inspect it, and
-- cabal-install-solver does not depend on cabal-install's location types.
--
data SolverPackage loc = SolverPackage {
        solverPkgSource  :: SourcePackage loc,
        solverPkgFlags   :: FlagAssignment,
        solverPkgStanzas :: OptionalStanzaSet,
        solverPkgLibDeps :: ComponentDeps [SolverId],
        solverPkgExeDeps :: ComponentDeps [SolverId]
    }
  deriving (Eq, Show, Generic)

instance Binary loc => Binary (SolverPackage loc)
instance Structured loc => Structured (SolverPackage loc)

instance Package (SolverPackage loc) where
  packageId = packageId . solverPkgSource
