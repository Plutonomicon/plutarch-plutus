{-# LANGUAGE QuantifiedConstraints #-}

module Plutarch.Numeric.Monoidal (
  Additive (..),
  Multiplicative (..),
  sum1,
  product1,
  sum,
  psum,
  product,
  pproduct,
  sumNZ,
  psumNZ,
  productNZ,
  pproductNZ,
  scaleNZNatural,
  powNZNatural,
  scaleNatural,
  powNatural,
  scaleInteger,
  powInteger,
  powIntegerZero,
) where

import Data.Foldable (foldl')
import Data.Kind (Type)
import Data.Semigroup (stimes, stimesMonoid)
import Data.Semigroup.Foldable (Foldable1 (foldMap1))
import Plutarch (S, Term, (#))
import Plutarch.List (PListLike (PElemConstraint), pfoldl')
import Plutarch.Numeric.Additive (
  AdditiveGroup (negate),
  AdditiveMonoid (zero),
  AdditiveSemigroup ((+)),
 )
import Plutarch.Numeric.Combination (
  Divisible (reciprocal),
  Euclidean ((*^), (+^)),
  RemoveZero (removeZero, zeroExtend),
 )
import Plutarch.Numeric.Group (Group (gtimes, inverse))
import Plutarch.Numeric.Multiplicative (
  MultiplicativeMonoid (one),
  MultiplicativeSemigroup ((*)),
 )
import Plutarch.Numeric.NZNatural (NZNatural (NZNatural))
import Plutarch.Numeric.Natural (Natural (Natural))
import Prelude hiding (negate, product, sum, (*), (+))

{- | A restriction of the type being wrapped to the \'additive half\' of its
 capabilities. This allows us to treat such types as 'Semigroup's, 'Monoid's
 or 'Group's when convenient to do so.

 @since 1.0
-}
newtype Additive (a :: Type) = Additive
  { getAdditive :: a
  -- ^ @since 1.0
  }

-- | @since 1.0
instance (AdditiveSemigroup a) => Semigroup (Additive a) where
  {-# INLINEABLE (<>) #-}
  Additive x <> Additive y = Additive $ x + y

-- | @since 1.0
instance (AdditiveMonoid a) => Monoid (Additive a) where
  {-# INLINEABLE mempty #-}
  mempty = Additive zero

-- | @since 1.0
instance (AdditiveGroup a) => Group (Additive a) where
  {-# INLINEABLE inverse #-}
  inverse (Additive x) = Additive . negate $ x

{- | A restriction of the type being wrapped to the \'multiplicative half\' of
 its capabilities. This allows us to treat such types as 'Semigroup's,
 'Monoid's or 'Group's when convenient to do so.

 @since 1.0
-}
newtype Multiplicative (a :: Type) = Multiplicative
  { getMultiplicative :: a
  -- ^ @since 1.0
  }

-- | @since 1.0
instance (MultiplicativeSemigroup a) => Semigroup (Multiplicative a) where
  {-# INLINEABLE (<>) #-}
  Multiplicative x <> Multiplicative y = Multiplicative $ x * y

-- | @since 1.0
instance (MultiplicativeMonoid a) => Monoid (Multiplicative a) where
  {-# INLINEABLE mempty #-}
  mempty = Multiplicative one

-- | @since 1.0
instance (Divisible a nz) => Group (Multiplicative nz) where
  {-# INLINEABLE inverse #-}
  inverse (Multiplicative x) = Multiplicative . reciprocal $ x

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

{- | As 'sum', but specialized for Plutarch types.

 @since 1.0
-}
psum ::
  forall (a :: S -> Type) (f :: (S -> Type) -> S -> Type) (s :: S).
  (PListLike f, forall s'. AdditiveMonoid (Term s' a), PElemConstraint f a) =>
  Term s (f a) ->
  Term s a
psum xs = pfoldl' (+) # zero # xs

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

{- | As 'product', but specialized for Plutarch types.

 @since 1.0
-}
pproduct ::
  forall (a :: S -> Type) (f :: (S -> Type) -> S -> Type) (s :: S).
  (PListLike f, forall s'. MultiplicativeMonoid (Term s' a), PElemConstraint f a) =>
  Term s (f a) ->
  Term s a
pproduct xs = pfoldl' (*) # one # xs

{- | A version of 'sum' for zerofree values.

 @since 1.0
-}
sumNZ ::
  forall (a :: Type) (f :: Type -> Type) (nz :: Type).
  (Foldable f, Euclidean a nz) =>
  f nz ->
  a
sumNZ = foldl' (+^) zero

{- | As 'sumNZ', but specialized for Plutarch types.

 @since 1.0
-}
psumNZ ::
  forall (a :: S -> Type) (f :: (S -> Type) -> S -> Type) (nz :: S -> Type) (s :: S).
  (PListLike f, forall s'. Euclidean (Term s' a) (Term s' nz), PElemConstraint f nz) =>
  Term s (f nz) ->
  Term s a
psumNZ xs = pfoldl' (+^) # zero # xs

{- | A version of 'product' for zerofree values.

 @since 1.0
-}
productNZ ::
  forall (a :: Type) (f :: Type -> Type) (nz :: Type).
  (Foldable f, Euclidean a nz) =>
  f nz ->
  a
productNZ = foldl' (*^) one

{- | As 'productNZ', but specialized for Plutarch types.

 @since 1.0
-}
pproductNZ ::
  forall (a :: S -> Type) (f :: (S -> Type) -> S -> Type) (nz :: S -> Type) (s :: S).
  (PListLike f, forall s'. Euclidean (Term s' a) (Term s' nz), PElemConstraint f nz) =>
  Term s (f nz) ->
  Term s a
pproductNZ xs = pfoldl' (*^) # one # xs

{- | Scales any 'AdditiveSemigroup' by an 'NZNatural'. Essentially a
 specialized, safer 'stimes'.

 @since 1.0
-}
scaleNZNatural ::
  forall (a :: Type).
  (AdditiveSemigroup a) =>
  NZNatural ->
  a ->
  a
scaleNZNatural (NZNatural i) = getAdditive . stimes i . Additive

{- | Exponentiates any 'MultiplicativeSemigroup' by an 'NZNatural'. Essentially
 a specialized, safer 'stimes'.

 @since 1.0
-}
powNZNatural ::
  forall (a :: Type).
  (MultiplicativeSemigroup a) =>
  NZNatural ->
  a ->
  a
powNZNatural (NZNatural i) = getMultiplicative . stimes i . Multiplicative

{- | Scales any 'AdditiveMonoid' by a 'Natural'. Essentially a specialized,
 safer 'stimesMonoid'.

 @since 1.0
-}
scaleNatural ::
  forall (a :: Type).
  (AdditiveMonoid a) =>
  Natural ->
  a ->
  a
scaleNatural (Natural i) = getAdditive . stimesMonoid i . Additive

{- | Exponentiates any 'MultiplicativeMonoid' by a 'Natural'. Essentially a
 specialized, safer 'stimesMonoid'.

 @since 1.0
-}
powNatural ::
  forall (a :: Type).
  (MultiplicativeMonoid a) =>
  Natural ->
  a ->
  a
powNatural (Natural i) = getMultiplicative . stimesMonoid i . Multiplicative

{- | Scales any 'AdditiveGroup' by an 'Integer'. Essentially a specialized
 'gtimes'.

 @since 1.0
-}
scaleInteger ::
  forall (a :: Type).
  (AdditiveGroup a) =>
  Integer ->
  a ->
  a
scaleInteger i = getAdditive . gtimes i . Additive

{- | Exponentiates a known non-zero 'Divisible' by an 'Integer'. Essentially a
 specialized 'gtimes'.

 @since 1.0
-}
powInteger ::
  forall (nz :: Type) (a :: Type).
  (Divisible a nz) =>
  Integer ->
  nz ->
  nz
powInteger i = getMultiplicative . gtimes i . Multiplicative

{- | Exponentiates any Haskell 'Divisible' by an 'Integer', under the convention that
 'zero' to any power is 'zero', except for 'one', in which case it is 'one'.
 This is a tad awkward, but this way, we are consistent with 'powNatural'.

 @since 1.0
-}
powIntegerZero ::
  forall (a :: Type) (nz :: Type).
  (Divisible a nz, RemoveZero a nz) =>
  Integer ->
  a ->
  a
powIntegerZero i x = case removeZero x of
  Nothing -> if i == zero then one else zero
  Just x' -> zeroExtend . powInteger i $ x'
