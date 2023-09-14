module Distribution.Types.DependencyMap
  ( DependencyMap
  , toDepMap
  , fromDepMap
  , constrainBy
  , DependencyMapMeet
  , toDepMapMeet
  , fromDepMapMeet
  , DependencyMapJoin
  , fromDepMapJoin
  , toDepMapJoin
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Types.Dependency
import Distribution.Types.LibraryName
import Distribution.Types.PackageName
import Distribution.Types.PackageVersionConstraint
import Distribution.Version

import qualified Data.Map.Lazy as Map

class Lattice a where
  -- | join
  (\/) :: a -> a -> a

  infixr 5 \/

  -- | meet
  (/\) :: a -> a -> a

  infixr 6 /\

newtype Meet a = Meet {getMeet :: a}
  deriving (Show, Read, Eq)

instance Lattice a => Semigroup (Meet a) where
  Meet lhs <> Meet rhs = Meet (lhs /\ rhs)

newtype Join a = Join {getJoin :: a}
  deriving (Show, Read, Eq)

instance Lattice a => Semigroup (Join a) where
  Join lhs <> Join rhs = Join (lhs \/ rhs)

instance Lattice VersionRange where
  (\/) = unionVersionRanges
  (/\) = intersectVersionRanges

newtype MonoidalMap k v = MonoidalMap {getMonoidalMap :: Map k v}
  deriving (Show, Read, Eq)

instance (Semigroup v, Ord k) => Monoid (MonoidalMap k v) where
  mempty = MonoidalMap mempty

instance (Semigroup v, Ord k) => Semigroup (MonoidalMap k v) where
  MonoidalMap m1 <> MonoidalMap m2 = MonoidalMap (Map.unionWith (<>) m1 m2)

singleton :: k -> v -> MonoidalMap k v
singleton k v = MonoidalMap (Map.singleton k v)

-- data Dependency
--   = Dependency
--       PackageName
--       VersionRange
--       (NonEmptySet LibraryName)
--
-- data MissingDependency
--   = MissingDependency
--       Dependency
--       MissingDependencyReason

type DependencyMap l = MonoidalMap PackageName (l, NonEmptySet LibraryName)

toDepMap :: Semigroup a => (VersionRange -> a) -> [Dependency] -> DependencyMap a
toDepMap meetOrJoin = foldMap (\(Dependency p vr cs) -> singleton p (meetOrJoin vr, cs))

fromDepMap :: (l -> VersionRange) -> MonoidalMap PackageName (l, NonEmptySet LibraryName) -> [Dependency]
fromDepMap unMeetOrJoin m =[Dependency p (unMeetOrJoin vr) cs | (p, (vr, cs)) <- Map.toList (getMonoidalMap m)]

type DependencyMapMeet = DependencyMap (Meet VersionRange)

toDepMapMeet :: [Dependency] -> DependencyMapMeet
toDepMapMeet = toDepMap Meet

fromDepMapMeet :: DependencyMapMeet -> [Dependency]
fromDepMapMeet = fromDepMap getMeet

-- Apply extra constraints to a dependency map.
-- Combines dependencies where the result will only contain keys from the left
-- (first) map.  If a key also exists in the right map, both constraints will
-- be intersected.
constrainBy
  :: DependencyMapMeet
  -> [PackageVersionConstraint]
  -> DependencyMapMeet
constrainBy = foldl' tightenConstraint
  where
    tightenConstraint (MonoidalMap m) (PackageVersionConstraint pn vr) =
      MonoidalMap $ Map.adjust (\(vr', cs) -> (vr' <> Meet vr, cs)) pn m

type DependencyMapJoin = MonoidalMap PackageName (Join VersionRange, NonEmptySet LibraryName)

toDepMapJoin :: [Dependency] -> DependencyMapJoin
toDepMapJoin = toDepMap Join

fromDepMapJoin :: DependencyMapJoin -> [Dependency]
fromDepMapJoin = fromDepMap getJoin
