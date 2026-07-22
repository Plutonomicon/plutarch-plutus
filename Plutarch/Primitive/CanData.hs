{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Plutarch.Primitive.CanData (
  PCanData (..),
) where

import Data.Kind (Type)
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEVector
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (
  SomeTerm,
  Term,
  plam',
  punsafeCase,
  punsafeCoerce,
  punsafeConstant,
  toSomeTerm,
 )
import Plutarch.Primitive.Apply (
  PlutarchType (PRepresentation),
  pcoerce,
  (#),
  (#$),
 )
import Plutarch.Primitive.BuiltinFun (
  pbData,
  pconstrData,
  pfstPair,
  pheadList,
  piData,
  plistData,
  pmkCons,
  pmkPairData,
  pnilData,
  psndPair,
  punBData,
  punConstrData,
  punIData,
  punListData,
 )
import Plutarch.Primitive.ByteString (PByteString)
import Plutarch.Primitive.Data (PAsData)
import Plutarch.Primitive.List (PBList)
import Plutarch.Primitive.Numeric (PByte, PInteger, PNatural, PPositive)
import Plutarch.Primitive.Pair (PBPair)
import PlutusCore qualified as PLC

-- | @since wip
class PlutarchType a => PCanData (a :: S -> Type) where
  pfromData ::
    forall (s :: S).
    Term s (PAsData a) -> Term s a
  default pfromData ::
    forall (s :: S).
    PCanData (PRepresentation a) =>
    Term s (PAsData a) -> Term s a
  pfromData = punsafeCoerce . pfromData @(PRepresentation a) . punsafeCoerce
  ptoData ::
    forall (s :: S).
    Term s a -> Term s (PAsData a)
  default ptoData ::
    forall (s :: S).
    PCanData (PRepresentation a) =>
    Term s a -> Term s (PAsData a)
  ptoData = punsafeCoerce . ptoData . pcoerce

-- | @since wip
instance PCanData PInteger where
  pfromData x = punIData # pcoerce x
  ptoData x = piData # x

-- | @since wip
instance PCanData PNatural

-- | @since wip
instance PCanData PPositive

-- | @since wip
instance PCanData PByte

-- | @since wip
instance PCanData PByteString where
  pfromData x = punBData # pcoerce x
  ptoData x = pbData # x

-- | @since wip
instance PCanData a => PCanData (PBList (PAsData a)) where
  pfromData x = punsafeCoerce (punListData # pcoerce x)
  ptoData x = punsafeCoerce (plistData @(PAsData a)) # x

-- | @since wip
instance (PCanData a, PCanData b) => PCanData (PBPair (PAsData a) (PAsData b)) where
  pfromData ::
    forall (s :: S).
    Term s (PAsData (PBPair (PAsData a) (PAsData b))) ->
    Term s (PBPair (PAsData a) (PAsData b))
  pfromData x =
    let asConstr = punConstrData # pcoerce x
        fields = psndPair # asConstr
     in punsafeCase fields handlers
    where
      handlers :: NonEmptyVector (SomeTerm s)
      handlers =
        let t = plam' $ \h1 -> plam' $ \rest ->
              pmkPairData # h1 #$ pheadList # rest
         in NEVector.singleton (toSomeTerm t)
  ptoData x =
    punsafeCoerce
      (pconstrData # i0 #$ pmkCons # pcoerce (pfstPair # x) #$ pmkCons # pcoerce (psndPair # x) # pnilData)
    where
      i0 :: forall (s :: S). Term s PInteger
      i0 = punsafeConstant . PLC.someValue @Integer $ 0
