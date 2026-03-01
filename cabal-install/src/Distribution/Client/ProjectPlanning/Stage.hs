{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}

module Distribution.Client.ProjectPlanning.Stage
  ( WithStage (..)
  , Stage (..)
  , HasStage (..)
  , Staged (..)
  , showStage
  , stages
  , prevStage
  , qpnStage
  , always
  , getStage
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.Types.ConfiguredId (HasConfiguredId (..))
import Distribution.Compat.Graph (IsNode (..))
import Distribution.Package (HasUnitId (..), Package (..))
import Distribution.Pretty (Pretty (..))
import Distribution.Solver.Types.PackagePath (QPN, Qualified (..), PackagePath (..), Qualifier (..))
import Text.PrettyPrint (colon, text)

-- | Which compilation stage a package belongs to.
--
-- 'Build' means the package runs on the build machine (e.g. setup scripts,
-- build-tool executables). 'Host' means it runs on the target machine
-- (everything else).
data Stage
  = Build  -- ^ runs on the build machine
  | Host   -- ^ runs on the target (host) machine
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Generic)

instance Binary Stage
instance Structured Stage

instance Pretty Stage where
  pretty = text . showStage

showStage :: Stage -> String
showStage Build = "build"
showStage Host  = "host"

-- | A container holding different values for each 'Stage'.
newtype Staged a = Staged
  { getStage :: Stage -> a
  }
  deriving (Functor, Generic)
  deriving Applicative via ((->) Stage)

instance Eq a => Eq (Staged a) where
  lhs == rhs = all (\s -> getStage lhs s == getStage rhs s) [minBound .. maxBound]

instance Show a => Show (Staged a) where
  showsPrec _ staged =
    showList [(s, getStage staged s) | s <- [minBound .. maxBound]]

instance Foldable Staged where
  foldMap f (Staged gs) = foldMap (f . gs) [minBound .. maxBound]

instance Traversable Staged where
  traverse f (Staged gs) = fmap (\vals -> Staged (\s -> vals !! fromEnum s))
                                (traverse (f . gs) [minBound .. maxBound])

instance Binary a => Binary (Staged a) where
  put (Staged gs) = mapM_ (put . gs) [minBound .. maxBound]
  get = do vals <- mapM (\_ -> get) [minBound .. maxBound :: Stage]
           return $ Staged (\s -> vals !! fromEnum s)

instance (Typeable a, Structured a) => Structured (Staged a) where
  structure _ = structure (Proxy :: Proxy [(Stage, a)])

-- | Derive the 'Stage' for a qualified package name.
-- Top-level packages run on the host; setup and exe dependencies run on the
-- build machine.
qpnStage :: QPN -> Stage
qpnStage (Q (PackagePath q) _) = qualifierStage q

qualifierStage :: Qualifier -> Stage
qualifierStage QualToplevel  = Host
qualifierStage (QualSetup _) = Build
qualifierStage (QualExe _ _) = Build

-- | All Stage values in order.
stages :: [Stage]
stages = [minBound .. maxBound]

-- | The previous stage (Build is its own predecessor).
prevStage :: Stage -> Stage
prevStage s
  | s == minBound = s
  | otherwise     = pred s

-- | Create a 'Staged' value that is the same for all stages.
always :: a -> Staged a
always = Staged . const

-- FIXME: blaaah
data WithStage a = WithStage Stage a
  deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

instance Binary a => Binary (WithStage a)
instance Structured a => Structured (WithStage a)

instance Package pkg => Package (WithStage pkg) where
  packageId (WithStage _stage pkg) = packageId pkg

instance IsNode a => IsNode (WithStage a) where
  type Key (WithStage a) = WithStage (Key a)
  nodeKey = fmap nodeKey
  nodeNeighbors = traverse nodeNeighbors

instance HasUnitId a => HasUnitId (WithStage a) where
  installedUnitId (WithStage _stage pkg) = installedUnitId pkg

instance HasConfiguredId a => HasConfiguredId (WithStage a) where
  configuredId (WithStage _stage pkg) = configuredId pkg

instance Pretty a => Pretty (WithStage a) where
  pretty (WithStage s pkg) = pretty s <> colon <> pretty pkg

class HasStage a where
  stageOf :: a -> Stage

instance HasStage (WithStage a) where
  stageOf (WithStage s _) = s
