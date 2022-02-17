module Plutarch.Numeric.Monoidal (
  Additive(..),
  Multiplicative(..),
  sum1,
  product1,
  sum,
  product,
  sumNZ,
  productNZ,
  ) where

import Data.Semigroup.Foldable (Foldable1 (foldMap1))
import Data.Kind (Type)
import Plutarch.Numeric.Additive (AdditiveSemigroup ((+)), AdditiveMonoid (zero),
  AdditiveGroup (negate))
import Plutarch.Numeric.Multiplicative (MultiplicativeSemigroup ((*)),
  MultiplicativeMonoid (one))
import Plutarch.Numeric.Combination (Divisible (reciprocal), Euclidean ((+^), (*^)))
import Plutarch.Numeric.Group (Group (inverse, gtimes))
import Data.Semigroup (stimesMonoid)
import Prelude hiding ((+), negate, (*), sum, product)
import qualified Prelude
import Data.Foldable (foldl')

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
  {-# INLINEABLE gtimes #-}
  gtimes i x = case Prelude.signum i of
    0 -> mempty
    1 -> stimesMonoid i x
    _ -> inverse . stimesMonoid (Prelude.negate i) $ x

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
