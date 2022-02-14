module Plutarch.Numeric.Ring (
  -- * Type classes
  Semirig (..),
  Semiring (..),
  Ring (..),
) where

import Plutarch.Numeric.Additive (AdditiveGroup, AdditiveMonoid, AdditiveSemigroup)
import Plutarch.Numeric.Multiplicative (MultiplicativeMonoid)
import Plutarch.Numeric.NZNatural (NZNatural)
import Plutarch.Numeric.Natural (Natural)

-- | @since 1.0
class (AdditiveSemigroup a, MultiplicativeMonoid a) => Semirig a where
  -- | @since 1.0
  fromNZNatural :: NZNatural -> a

-- | @since 1.0
class (Semirig a, AdditiveMonoid a) => Semiring a where
  -- | @since 1.0
  fromNatural :: Natural -> a

-- | @since 1.0
class (AdditiveGroup a, Semiring a) => Ring a where
  -- | @since 1.0
  fromInteger :: Integer -> a
