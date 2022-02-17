{-# LANGUAGE Trustworthy #-}

{- | Module: Plutarch.Numeric.Extra
 Copyright: (C) MLabs 2022
 License: MIT
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 Extra useful numeric functionality.
-}
module Plutarch.Numeric.Extra (
  -- * Functions

  -- ** Reductions
  sum1,
  product1,
  sum,
  product,
  sumNZ,
  productNZ,
) where

import Data.Foldable (foldl')
import Data.Kind (Type)
import Data.Semigroup.Foldable (Foldable1 (foldMap1))
import Plutarch.Numeric (
  Additive (Additive),
  AdditiveMonoid (zero),
  AdditiveSemigroup,
  Euclidean ((*^), (+^)),
  Multiplicative (Multiplicative),
  MultiplicativeMonoid (one),
  MultiplicativeSemigroup,
  getAdditive,
  getMultiplicative,
 )
import Prelude hiding (div, divMod, mod, product, quot, quotRem, rem, sum)

{- | \'Add up\' a non-empty collection of values.

 @since 1.0
-}
sum1 ::
  forall (a :: Type) (f :: Type -> Type).
  (Foldable1 f, AdditiveSemigroup a) =>
  f a ->
  a
sum1 = getAdditive . foldMap1 Additive

{- | As 'sum1', but allows empty collections.

 @since 1.0
-}
sum ::
  forall (a :: Type) (f :: Type -> Type).
  (Foldable f, AdditiveMonoid a) =>
  f a ->
  a
sum = getAdditive . foldMap Additive

{- | Multiply together a non-empty collection of values.

 @since 1.0
-}
product1 ::
  forall (a :: Type) (f :: Type -> Type).
  (Foldable1 f, MultiplicativeSemigroup a) =>
  f a ->
  a
product1 = getMultiplicative . foldMap1 Multiplicative

{- | As 'product1', but allows empty collections.

 @since 1.0
-}
product ::
  forall (a :: Type) (f :: Type -> Type).
  (Foldable f, MultiplicativeMonoid a) =>
  f a ->
  a
product = getMultiplicative . foldMap Multiplicative

{- | A version of 'sum' for zerofree values.

 @since 1.0
-}
sumNZ ::
  forall (a :: Type) (f :: Type -> Type) (nz :: Type).
  (Foldable f, Euclidean a nz) =>
  f nz ->
  a
sumNZ = foldl' (+^) zero

{- | A version of 'product' for zerofree values.

 @since 1.0
-}
productNZ ::
  forall (a :: Type) (f :: Type -> Type) (nz :: Type).
  (Foldable f, Euclidean a nz) =>
  f nz ->
  a
productNZ = foldl' (*^) one
