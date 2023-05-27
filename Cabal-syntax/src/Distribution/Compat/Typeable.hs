{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Distribution.Compat.Typeable (
    Typeable,
    TypeRep,
    typeRep,
    ) where

#if True
import Data.Typeable (Typeable, TypeRep, typeRep)
#else
import Data.Typeable (Typeable, TypeRep, typeOf)
#endif

#if !True
typeRep :: forall a proxy. Typeable a => proxy a -> TypeRep
typeRep _ = typeOf (undefined :: a)
#endif
