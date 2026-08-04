{-# LANGUAGE UndecidableSuperClasses #-}

module Plutarch.Numeric.Additive (
  PAdditiveSemigroup (..),
  PAdditiveMonoid (..),
  PAdditiveGroup (..),
  (#+),
  (#-),
) where

import Data.Kind (Type)
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (
  Term,
  plam',
  punsafeCoerce,
  punsafeConstant,
 )
import Plutarch.Helpers.Numeric (
  pexpBySquaring,
  pione,
  pizero,
 )
import Plutarch.Primitive.Apply (
  PlutarchType (PRepresentation),
  pcoerce,
  (#),
  (#$),
 )
import Plutarch.Primitive.BLS (
  PBLS12_381_G1_Element,
  PBLS12_381_G2_Element,
 )
import Plutarch.Primitive.Bool (pif)
import Plutarch.Primitive.BuiltinFun (
  paddInteger,
  pbls12_381_G1_add,
  pbls12_381_G1_neg,
  pbls12_381_G1_scalarMul,
  pbls12_381_G2_add,
  pbls12_381_G2_neg,
  pbls12_381_G2_scalarMul,
  pequalsInteger,
  plessThanEqualsInteger,
  pmultiplyInteger,
  psubtractInteger,
 )
import Plutarch.Primitive.Function ((:-->))
import Plutarch.Primitive.Liftable (pconstant)
import Plutarch.Primitive.Numeric (PInteger, PNatural, PPositive)
import PlutusCore qualified as PLC
import PlutusCore.Crypto.BLS12_381.G1 as G1
import PlutusCore.Crypto.BLS12_381.G2 as G2

-- | @since wip
class PlutarchType a => PAdditiveSemigroup (a :: S -> Type) where
  padd :: forall (s :: S). Term s (a :--> a :--> a)
  default padd ::
    forall (s :: S).
    PAdditiveSemigroup (PRepresentation a) => Term s (a :--> a :--> a)
  padd = plam' $ \x -> plam' $ \y -> punsafeCoerce (padd # pcoerce x # pcoerce y)
  pscalePositive :: forall (s :: S). Term s (a :--> PPositive :--> a)
  pscalePositive = punsafeCoerce . pexpBySquaring @a $ padd

-- | @since wip
instance PAdditiveSemigroup PInteger where
  padd = paddInteger
  pscalePositive = punsafeCoerce pmultiplyInteger

-- | @since wip
instance PAdditiveSemigroup PNatural

-- | @since wip
instance PAdditiveSemigroup PPositive

-- | @since wip
instance PAdditiveSemigroup PBLS12_381_G1_Element where
  padd = pbls12_381_G1_add
  pscalePositive = plam' $ \x -> plam' $ \p ->
    pbls12_381_G1_scalarMul # punsafeCoerce p # x

-- | @since wip
instance PAdditiveSemigroup PBLS12_381_G2_Element where
  padd = pbls12_381_G2_add
  pscalePositive = plam' $ \x -> plam' $ \p ->
    pbls12_381_G2_scalarMul # punsafeCoerce p # x

-- | @since wip
class PAdditiveSemigroup a => PAdditiveMonoid (a :: S -> Type) where
  pzero :: forall (s :: S). Term s a
  default pzero ::
    forall (s :: S).
    PAdditiveMonoid (PRepresentation a) => Term s a
  pzero = punsafeCoerce (pzero @(PRepresentation a))
  pscaleNatural :: forall (s :: S). Term s (a :--> PNatural :--> a)
  pscaleNatural = plam' $ \x -> plam' $ \n ->
    pif
      (pequalsInteger # pizero # pcoerce n)
      pzero
      (pscalePositive # x # punsafeCoerce n)

-- | @since wip
instance PAdditiveMonoid PInteger where
  pzero = pizero
  pscaleNatural = punsafeCoerce pmultiplyInteger

-- | @since wip
instance PAdditiveMonoid PNatural

-- | @since wip
instance PAdditiveMonoid PBLS12_381_G1_Element where
  pzero = pconstant G1.offchain_zero
  pscaleNatural = plam' $ \x -> plam' $ \n ->
    pbls12_381_G1_scalarMul # pcoerce n # x

-- | @since wip
instance PAdditiveMonoid PBLS12_381_G2_Element where
  pzero = pconstant G2.offchain_zero
  pscaleNatural = plam' $ \x -> plam' $ \n ->
    pbls12_381_G2_scalarMul # pcoerce n # x

-- | @since wip
class PAdditiveMonoid a => PAdditiveGroup (a :: S -> Type) where
  pnegate :: forall (s :: S). Term s (a :--> a)
  pnegate = plam' $ \x -> pminus # pzero # x
  pminus :: forall (s :: S). Term s (a :--> a :--> a)
  pminus = plam' $ \x -> plam' $ \y -> padd # x #$ pnegate # y
  pscaleInteger :: forall (s :: S). Term s (a :--> PInteger :--> a)
  pscaleInteger = plam' $ \x -> plam' $ \i ->
    -- Note (Koz, 29/06/2026): We do the `<= (-1)` comparison instead of the
    -- more straightforward `< 0` because `<=` is _slightly_ more efficient in
    -- its costing. No, I have no idea why either.
    pif
      (plessThanEqualsInteger # pcoerce i # punsafeConstant (PLC.someValue @Integer (-1)))
      -- Since `i` is negative, its negation is guaranteed positive.
      (pnegate #$ pscalePositive # x # punsafeCoerce (psubtractInteger # pione # i))
      (pscaleNatural # x # punsafeCoerce i)

-- | @since wip
instance PAdditiveGroup PInteger where
  pminus = psubtractInteger
  pscaleInteger = punsafeCoerce pmultiplyInteger

-- | @since wip
instance PAdditiveGroup PBLS12_381_G1_Element where
  pnegate = pbls12_381_G1_neg
  pscaleInteger = plam' $ \x -> plam' $ \i ->
    pbls12_381_G1_scalarMul # i # x

-- | @since wip
instance PAdditiveGroup PBLS12_381_G2_Element where
  pnegate = pbls12_381_G2_neg
  pscaleInteger = plam' $ \x -> plam' $ \i ->
    pbls12_381_G2_scalarMul # i # x

-- | @since wip
(#+) ::
  forall (a :: S -> Type) (s :: S).
  PAdditiveSemigroup a =>
  Term s a -> Term s a -> Term s a
x #+ y = padd # x # y

infix 6 #+

-- | @since wip
(#-) ::
  forall (a :: S -> Type) (s :: S).
  PAdditiveGroup a =>
  Term s a -> Term s a -> Term s a
x #- y = pminus # x # y

infix 6 #-
