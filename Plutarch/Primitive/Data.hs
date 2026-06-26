{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Plutarch.Primitive.Data (
  -- * Types
  PData,
  PAsData,

  -- * Functions
  piData,
  pbData,
  pconstrData,
  plistData,
  pmapData,
  punIData,
  punBData,
  punConstrData,
  punListData,
  punMapData,
  pequalsData,
  pserialiseData,
) where

import Data.Kind (Type)
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (Term, punsafeBuiltin)
import Plutarch.Primitive.Apply (
  PCanRepresent,
  PlutarchType (PRepresentation),
  PlutarchTypeRep (PlutarchTypeRep),
 )
import Plutarch.Primitive.Bool (PBool)
import Plutarch.Primitive.ByteString (PByteString)
import Plutarch.Primitive.Function ((:-->))
import Plutarch.Primitive.Liftable (
  PLiftable,
  PLiftableDirect (PLiftableDirect),
 )
import Plutarch.Primitive.List (PBList)
import Plutarch.Primitive.Numeric (PInteger)
import Plutarch.Primitive.Pair (PBPair)
import PlutusCore qualified as PLC
import PlutusTx qualified as PTx

-- | @since wip
data PData (s :: S)

type role PData nominal

-- | @since wip
deriving via (PlutarchTypeRep PData PData) instance PlutarchType PData

-- | @since wip
deriving via (PLiftableDirect PData PTx.Data) instance PLiftable PData

-- | @since wip
data PAsData (a :: S -> Type) (s :: S)

type role PAsData nominal nominal

-- | @since wip
instance PlutarchType (PAsData a) where
  type PRepresentation (PAsData _) = PData

-- | @since wip
piData ::
  forall (s :: S).
  Term s (PInteger :--> PAsData PInteger)
piData = punsafeBuiltin PLC.IData

-- | @since wip
pbData ::
  forall (s :: S).
  Term s (PByteString :--> PAsData PByteString)
pbData = punsafeBuiltin PLC.BData

-- | @since wip
pconstrData ::
  forall (s :: S).
  Term s (PInteger :--> PBList PData :--> PData)
pconstrData = punsafeBuiltin PLC.ConstrData

-- | @since wip
plistData ::
  forall (a :: S -> Type) (s :: S).
  PData `PCanRepresent` a =>
  Term s (PBList a :--> PAsData (PBList (PAsData a)))
plistData = punsafeBuiltin PLC.ListData

-- | @since wip
pmapData ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  (PData `PCanRepresent` a, PData `PCanRepresent` b) =>
  Term s (PBList (PBPair a b) :--> PAsData (PBList (PAsData (PBPair (PAsData a) (PAsData b)))))
pmapData = punsafeBuiltin PLC.MapData

-- | @since wip
punIData :: forall (s :: S). Term s (PData :--> PInteger)
punIData = punsafeBuiltin PLC.UnIData

-- | @since wip
punBData :: forall (s :: S). Term s (PData :--> PByteString)
punBData = punsafeBuiltin PLC.UnBData

-- | @since wip
punConstrData :: forall (s :: S). Term s (PData :--> PBPair PInteger (PBList PData))
punConstrData = punsafeBuiltin PLC.UnConstrData

-- | @since wip
punListData :: forall (s :: S). Term s (PData :--> PBList PData)
punListData = punsafeBuiltin PLC.UnListData

-- | @since wip
punMapData :: forall (s :: S). Term s (PData :--> PBList (PBPair PData PData))
punMapData = punsafeBuiltin PLC.UnMapData

-- | @since wip
pequalsData :: forall (s :: S). Term s (PData :--> PData :--> PBool)
pequalsData = punsafeBuiltin PLC.EqualsData

-- | @since wip
pserialiseData :: forall (s :: S). Term s (PData :--> PByteString)
pserialiseData = punsafeBuiltin PLC.SerialiseData
