{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.DataNewtype (PDataNewtype (..)) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Plutarch.Internal (PAsData, PData, PEq ((#==)), PInner, PIsData, POrd ((#<), (#<=)), PShow (pshow'), PTryFrom, PlutusType (pcon', pmatch'), S, Term, pdataImpl, pforgetData, pfromData, pfromDataImpl, pmatch, pto)
import Plutarch.Unsafe (punsafeCoerce)

----------------------- other utility functions -----------------------------------------

-- This should be killed

-- | @since 1.7.0
newtype PDataNewtype (a :: S -> Type) (s :: S) = PDataNewtype (Term s (PAsData a))
  deriving stock
    ( -- | @since 1.7.0
      Generic
    )

-- | @since 1.7.0
instance PlutusType (PDataNewtype a) where
  type PInner (PDataNewtype a) = PData
  pcon' (PDataNewtype a) = pforgetData a
  pmatch' x' f = f (PDataNewtype (punsafeCoerce x'))

-- | @since 1.7.0
instance PIsData (PDataNewtype a) where
  pfromDataImpl = punsafeCoerce
  pdataImpl = punsafeCoerce

-- | @since 1.7.0
instance PEq (PDataNewtype a) where
  a #== b = pto a #== pto b

-- | @since 1.7.0
instance (PIsData a, POrd a) => POrd (PDataNewtype a) where
  {-# INLINEABLE (#<=) #-}
  a #<= b =
    pmatch a $ \(PDataNewtype a') ->
      pmatch b $ \(PDataNewtype b') ->
        pfromData a' #<= pfromData b'
  {-# INLINEABLE (#<) #-}
  a #< b =
    pmatch a $ \(PDataNewtype a') ->
      pmatch b $ \(PDataNewtype b') ->
        pfromData a' #< pfromData b'

-- | @since 1.7.0
instance (PIsData a, PShow a) => PShow (PDataNewtype a) where
  pshow' x t =
    pmatch t \(PDataNewtype t') -> pshow' x $ pfromData t'

-- | @since 1.7.0
instance (PIsData a, PTryFrom PData (PAsData a)) => PTryFrom PData (PDataNewtype a)

-- | @since 1.7.0
instance PTryFrom PData (PAsData (PDataNewtype a))
