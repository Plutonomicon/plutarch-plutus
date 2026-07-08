{-# LANGUAGE UndecidableSuperClasses #-}

module Plutarch.Primitive.Eq (
  PEq (..),
) where

import Data.Kind (Type)
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (
  Term,
  pfix,
  plam',
  punsafeCoerce,
  punsafeConstant,
 )
import Plutarch.Primitive.Apply (
  PlutarchType (PRepresentation),
  pcoerce,
  (#),
 )
import Plutarch.Primitive.Array (PBArray)
import Plutarch.Primitive.BLS (
  PBLS12_381_G1_Element,
  PBLS12_381_G2_Element,
 )
import Plutarch.Primitive.Bool (PBool, pfalse, pif, pnot, ptrue)
import Plutarch.Primitive.BuiltinFun (
  paddInteger,
  pbls12_381_G1_equal,
  pbls12_381_G2_equal,
  pequalsByteString,
  pequalsData,
  pequalsInteger,
  pequalsString,
  pindexArray,
  plengthOfArray,
  pvalueData,
 )
import Plutarch.Primitive.ByteString (PByteString)
import Plutarch.Primitive.Data (PAsData, PData)
import Plutarch.Primitive.Function ((:-->))
import Plutarch.Primitive.List (PBList (PBCons, PBNil))
import Plutarch.Primitive.Match (pmatch)
import Plutarch.Primitive.Numeric (PByte, PInteger, PNatural, PPositive)
import Plutarch.Primitive.Pair (PBPair (PBPair))
import Plutarch.Primitive.String (PString)
import Plutarch.Primitive.Value (PBValue)
import PlutusCore qualified as PLC

-- | @since wip
class PlutarchType a => PEq (a :: S -> Type) where
  peq :: forall (s :: S). Term s (a :--> a :--> PBool)
  default peq ::
    forall (s :: S).
    PEq (PRepresentation a) => Term s (a :--> a :--> PBool)
  peq = punsafeCoerce (peq @(PRepresentation a))

-- | @since wip
instance PEq PBool where
  peq = plam' $ \x -> plam' $ \y ->
    pif x y (pnot y)

-- | @since wip
instance PEq PInteger where
  peq = pequalsInteger

-- | @since wip
instance PEq PNatural

-- | @since wip
instance PEq PPositive

-- | @since wip
instance PEq PByte

-- | @since wip
instance PEq a => PEq (PBArray a) where
  peq = plam' $ \xs -> plam' $ \ys ->
    pif
      (peq # (plengthOfArray # xs) # (plengthOfArray # ys))
      (go xs ys (plengthOfArray # xs) # punsafeConstant (PLC.someValue @Integer 0))
      pfalse
    where
      go ::
        forall (s :: S).
        Term s (PBArray a) ->
        Term s (PBArray a) ->
        Term s PNatural ->
        Term s (PNatural :--> PBool)
      go xs ys limit = pfix $ \self -> plam' $ \i ->
        pif
          (peq # i # limit)
          ptrue
          ( pif
              (peq # (pindexArray # xs # i) # (pindexArray # ys # i))
              (self # punsafeCoerce (paddInteger # pcoerce i # punsafeConstant (PLC.someValue @Integer 1)))
              pfalse
          )

-- | @since wip
instance PEq PBLS12_381_G1_Element where
  peq = pbls12_381_G1_equal

-- | @since wip
instance PEq PBLS12_381_G2_Element where
  peq = pbls12_381_G2_equal

-- | @since wip
instance PEq PByteString where
  peq = pequalsByteString

-- | @since wip
instance PEq PData where
  peq = pequalsData

-- | @since wip
instance PEq (PAsData a)

-- | @since wip
instance PEq a => PEq (PBList a) where
  peq = pfix $ \self -> plam' $ \xs -> plam' $ \ys ->
    pmatch xs $ \case
      PBNil -> pmatch ys $ \case
        PBNil -> ptrue
        PBCons _ _ -> pfalse
      PBCons x' xs' -> pmatch ys $ \case
        PBNil -> pfalse
        PBCons y' ys' ->
          pif
            (peq # x' # y')
            (self # xs' # ys')
            pfalse

-- | @since wip
instance (PEq a, PEq b) => PEq (PBPair a b) where
  peq = plam' $ \xs -> plam' $ \ys ->
    pmatch xs $ \(PBPair x1 x2) ->
      pmatch ys $ \(PBPair y1 y2) ->
        pif
          (peq # x1 # y1)
          ( pif
              (peq # x2 # y2)
              ptrue
              pfalse
          )
          pfalse

-- | @since wip
instance PEq PString where
  peq = pequalsString

-- | @since wip
instance PEq PBValue where
  peq = plam' $ \v1 -> plam' $ \v2 ->
    pequalsData # (pvalueData # v1) # (pvalueData # v2)
