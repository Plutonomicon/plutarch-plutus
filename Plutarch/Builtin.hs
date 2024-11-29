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

import Data.Functor.Const (Const)
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import Plutarch.Builtin.Bool (PBool, pif, pif', (#&&), (#||))
import Plutarch.Builtin.ByteString (PByteString)
import Plutarch.Builtin.Data
import Plutarch.Builtin.Integer (PInteger)
import Plutarch.Builtin.String (PString)
import Plutarch.Internal.Eq (PEq ((#==)))
import Plutarch.Internal.IsData
import Plutarch.Internal.Lift (
  DeriveBuiltinPLiftable,
  PLiftable (AsHaskell, PlutusRepr, fromPlutarch, fromPlutarchRepr, toPlutarch, toPlutarchRepr),
  PLifted (PLifted),
  fromPlutarchUni,
  getPLifted,
  pconstant,
  toPlutarchUni,
  unsafeToUni,
 )
import Plutarch.Internal.Newtype (PlutusTypeNewtype)
import Plutarch.Internal.Ord (POrd ((#<), (#<=)))
import Plutarch.Internal.Other (POpaque, pfix, pto)
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.PlutusType (
  DerivePlutusType (DPTStrat),
  PCovariant,
  PVariant,
  PlutusType (PContravariant', PCovariant', PInner, PVariant', pcon', pmatch'),
  pcon,
  pmatch,
 )
import Plutarch.Internal.Show (PShow (pshow'), pshow)
import Plutarch.Internal.Term (
  S,
  Term,
  pdelay,
  pforce,
  phoistAcyclic,
  plet,
  (#),
  (#$),
  (:-->),
 )
import Plutarch.Internal.TermCont (runTermCont, tcont, unTermCont)
import Plutarch.Internal.TryFrom (PSubtype, PTryFrom, PTryFromExcess, ptryFrom, ptryFrom', pupcast, pupcastF)
import Plutarch.Internal.Witness (witness)
import Plutarch.List (
  PListLike (
    PElemConstraint,
    pcons,
    pelimList,
    phead,
    pnil,
    pnull,
    ptail
  ),
  pfoldr',
  phead,
  plistEquals,
  pmap,
  pshowList,
  ptail,
 )
import Plutarch.Trace (ptraceInfoError)
import Plutarch.Unsafe (punsafeBuiltin, punsafeCoerce, punsafeDowncast)
import PlutusCore qualified as PLC
import PlutusTx (Data (Constr), ToData)
import PlutusTx qualified
import PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString), BuiltinData (BuiltinData))

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
