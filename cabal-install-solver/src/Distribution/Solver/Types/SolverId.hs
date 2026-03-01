{-# LANGUAGE DeriveGeneric #-}
module Distribution.Solver.Types.SolverId
    ( SolverId(..)
    )

where

import Distribution.Solver.Compat.Prelude
import Prelude ()

import Distribution.Package (PackageId, Package(..), UnitId)
import Distribution.Pretty (Pretty (..))
import Distribution.Solver.Types.PackagePath (QPN)
import Text.PrettyPrint (colon, punctuate, text)


-- | The solver can produce references to existing packages or
-- packages we plan to install.  Unlike 'ConfiguredId' we don't
-- yet know the 'UnitId' for planned packages, because it's
-- not the solver's job to compute them.
--
data SolverId = PreExistingId { solverQPN :: QPN, solverSrcId :: PackageId, solverInstId :: UnitId }
              | PlannedId     { solverQPN :: QPN, solverSrcId :: PackageId }
  deriving (Eq, Ord, Generic)

instance Binary SolverId
instance Structured SolverId

instance Show SolverId where
    show = show . solverSrcId

instance Package SolverId where
  packageId = solverSrcId

instance Pretty SolverId where
    pretty (PreExistingId qpn pkg unitId) = mconcat $ punctuate colon $ [pretty qpn, pretty pkg, text "installed", pretty unitId]
    pretty (PlannedId qpn pkg)            = mconcat $ punctuate colon $ [pretty qpn, pretty pkg, text "planned"]