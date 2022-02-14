module Plutarch.Numeric.Additive (
  -- * Type classes
  AdditiveSemigroup (..),
  AdditiveMonoid (..),
  AdditiveCMM (..),
  AdditiveGroup (..),

  -- * Helper wrapper
  Additive (..),
) where

import Data.Kind (Type)
import Data.Semigroup (stimes, stimesMonoid)
import Plutarch.Numeric.Group (Group (gtimes, inverse))
import Plutarch.Numeric.NZNatural (NZNatural)
import Plutarch.Numeric.NZNatural qualified as NZN
import Plutarch.Numeric.Natural (Natural)
import Plutarch.Numeric.Natural qualified as Nat
import Prelude hiding (negate, (+))
import Prelude qualified

-- | @since 1.0
class (Eq a) => AdditiveSemigroup a where
  {-# MINIMAL (+) #-}

  -- | @since 1.0
  (+) :: a -> a -> a

  -- This uses a default \'sum by squaring\' implementation. While this is
  -- reasonably good by default, many types allow a more efficient
  -- implementation; try and define one if you can.
  --
  -- @since 1.0
  {-# INLINEABLE scaleNZNatural #-}
  scaleNZNatural :: a -> NZNatural -> a
  scaleNZNatural x (NZN.NZNatural n) = getAdditive . stimes n . Additive $ x

-- | @since 1.0
instance AdditiveSemigroup Integer where
  {-# INLINEABLE (+) #-}
  (+) = (Prelude.+)
  {-# INLINEABLE scaleNZNatural #-}
  scaleNZNatural i (NZN.NZNatural n) = i * n

-- | @since 1.0
class (AdditiveSemigroup a) => AdditiveMonoid a where
  {-# MINIMAL zero #-}

  -- | @since 1.0
  zero :: a

  -- This uses a default \'sum by squaring\' implementation. While this is
  -- reasonably good by default, many types allow a more efficient
  -- implementation; try and define one if you can.
  --
  -- @since 1.0
  {-# INLINEABLE scaleNatural #-}
  scaleNatural :: a -> Natural -> a
  scaleNatural x (Nat.Natural n) = getAdditive . stimesMonoid n . Additive $ x

instance AdditiveMonoid Integer where
  {-# INLINEABLE zero #-}
  zero = 0
  {-# INLINEABLE scaleNatural #-}
  scaleNatural i (Nat.Natural n) = i * n

-- | @since 1.0
class (AdditiveMonoid a) => AdditiveGroup a where
  {-# MINIMAL negate #-}

  -- | @since 1.0
  (-) :: a -> a -> a
  x - y = x + negate y

  -- | @since 1.0
  negate :: a -> a

  -- This uses a default \'sum by squaring\' implementation. While this is
  -- reasonably good by default, many types allow a more efficient
  -- implementation; try and define one if you can.
  --
  -- @since 1.0
  {-# INLINEABLE scaleInteger #-}
  scaleInteger :: a -> Integer -> a
  scaleInteger x i = getAdditive . gtimes i . Additive $ x

-- | @since 1.0
instance AdditiveGroup Integer where
  {-# INLINEABLE (-) #-}
  (-) = (Prelude.-)
  {-# INLINEABLE negate #-}
  negate = Prelude.negate
  {-# INLINEABLE scaleInteger #-}
  scaleInteger = (*)

-- | @since 1.0
class (AdditiveMonoid a) => AdditiveCMM a where
  -- | @since 1.0
  (^-) :: a -> a -> a

-- | @since 1.0
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

instance (AdditiveGroup a) => Group (Additive a) where
  {-# INLINEABLE inverse #-}
  inverse (Additive x) = Additive . negate $ x
