{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Builtin (
  PData,
  pfstBuiltin,
  psndBuiltin,
  pasConstr,
  pasMap,
  pasList,
  pasInt,
  plistData,
  pconstrBuiltin,
  pasByteStr,
  PBuiltinPair,
  PBuiltinList (..),
  PIsData (..),
  pdata,
  pfromData,
  PAsData,
  pforgetData,
  prememberData,
  prememberData',
  pserialiseData,
  ppairDataBuiltin,
  pchooseListBuiltin,
  pchooseData,
  PDataNewtype (..),
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Plutarch.Builtin.Data (
  PAsData,
  PBuiltinList (PCons, PNil),
  PBuiltinPair,
  PData,
  pasByteStr,
  pasConstr,
  pasInt,
  pasList,
  pasMap,
  pchooseData,
  pchooseListBuiltin,
  pconstrBuiltin,
  pfstBuiltin,
  plistData,
  ppairDataBuiltin,
  pserialiseData,
  psndBuiltin,
 )
import Plutarch.Internal.Eq (PEq ((#==)))
import Plutarch.Internal.IsData (
  PIsData,
  pdata,
  pdataImpl,
  pforgetData,
  pfromData,
  pfromDataImpl,
  prememberData,
  prememberData',
 )
import Plutarch.Internal.Ord (POrd ((#<), (#<=)))
import Plutarch.Internal.Other (pto)
import Plutarch.Internal.PlutusType (
  PInner,
  PlutusType (pcon', pmatch'),
  pmatch,
 )
import Plutarch.Internal.Show (PShow (pshow'))
import Plutarch.Internal.Term (
  S,
  Term,
 )
import Plutarch.Internal.TryFrom (PTryFrom)
import Plutarch.Unsafe (punsafeCoerce)

----------------------- other utility functions -----------------------------------------

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
