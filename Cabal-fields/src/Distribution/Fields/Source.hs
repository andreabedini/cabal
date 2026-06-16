{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

-- | The /provenance/ of parsed input: where a warning or error came from.
-- Parametrised over a concrete source type so that different consumers
-- (cabal files, installed-package info, …) can attach their own description.
module Distribution.Fields.Source
  ( PSource (..)
  , CabalFileSource (..)
  , InstalledPackageInfoSource (..)
  , renderCabalFileSource
  , renderInstalledPackageInfoSource
  ) where

import qualified Data.ByteString as BS
import Data.Binary (Binary)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

-- $setup
-- >>> :set -XOverloadedStrings

-- | The source a warning or error is associated with: either a known source
-- @src@, or an unknown one. (Two 'PKnownSource's compare equal iff their @src@s do;
-- 'PUnknownSource' is equal only to itself.)
data PSource src
  = -- | A known source, described by a value of type @src@.
    PKnownSource src
  | -- | No source information available.
    PUnknownSource
  deriving (Ord, Show, Generic, Functor)

-- | A cabal file source: its path and its raw contents. Equality compares the
-- path only (the contents are ignored).
newtype CabalFileSource
  = PCabalFile (FilePath, BS.ByteString)
  deriving (Ord, Show, Generic)

-- | The (single, contentless) source tag for installed-package-info input.
data InstalledPackageInfoSource
  = PInstalledPackageInfo
  deriving (Eq, Ord, Show, Generic)

-- | Render a 'CabalFileSource' as its file path.
--
-- >>> renderCabalFileSource (PCabalFile ("foo.cabal", "name: foo"))
-- "foo.cabal"
renderCabalFileSource :: CabalFileSource -> String
renderCabalFileSource (PCabalFile (path, _)) = path

-- | Render an 'InstalledPackageInfoSource'. There is no meaningful path, so this
-- is the empty string.
renderInstalledPackageInfoSource :: InstalledPackageInfoSource -> String
renderInstalledPackageInfoSource PInstalledPackageInfo = ""

instance Eq CabalFileSource where
  PCabalFile (path, _) == PCabalFile (path', _) = path == path'

instance Eq src => Eq (PSource src) where
  PKnownSource src == PKnownSource src' = src == src'
  PUnknownSource == PUnknownSource = True
  _ == _ = False

instance Binary src => Binary (PSource src)
instance NFData src => NFData (PSource src)
