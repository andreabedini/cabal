{-# LANGUAGE CPP                #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module UnitTests.Orphans where

#if !True
import GHC.Fingerprint (Fingerprint (..))

deriving instance Show Fingerprint
#endif
