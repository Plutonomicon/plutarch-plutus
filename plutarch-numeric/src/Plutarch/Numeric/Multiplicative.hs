{-# LANGUAGE FunctionalDependencies #-}

module Plutarch.Numeric.Multiplicative (
  -- * Type classes
  MultiplicativeSemigroup (..),
  MultiplicativeMonoid (..),
  MultiplicativeGroup (..),
  ZeroExtendable (..),

  -- * Helper wrapper
  Multiplicative (..),
) where

import Data.Kind (Type)
import Data.Semigroup (stimes, stimesMonoid)
import Plutarch.Numeric.Additive (AdditiveMonoid)
import Plutarch.Numeric.Group (Group (gtimes, inverse))
import Plutarch.Numeric.NZNatural (NZNatural)
import Plutarch.Numeric.NZNatural qualified as NZN
import Plutarch.Numeric.Natural (Natural)
import Plutarch.Numeric.Natural qualified as Nat
import Prelude hiding ((*))
import Prelude qualified

-- | @since 1.0
class (Eq a) => MultiplicativeSemigroup a where
  {-# MINIMAL (*) #-}

  -- | @since 1.0
  (*) :: a -> a -> a

  -- This uses a default \'exponentiation by squaring\' implementation.
  --
  -- @since 1.0
  powNZNatural :: a -> NZNatural -> a
  powNZNatural x (NZN.NZNatural n) = getMultiplicative . stimes n . Multiplicative $ x

-- | @since 1.0
instance MultiplicativeSemigroup Integer where
  {-# INLINEABLE (*) #-}
  (*) = (Prelude.*)

-- | @since 1.0
class (MultiplicativeSemigroup a) => MultiplicativeMonoid a where
  {-# MINIMAL one, abs, signum #-}

  -- | @since 1.0
  one :: a

  -- | @since 1.0
  abs :: a -> a

  -- | @since 1.0
  signum :: a -> a

  -- This uses a default \'exponentiation by squaring\' implementation.
  --
  -- @since 1.0
  powNatural :: a -> Natural -> a
  powNatural x (Nat.Natural n) =
    getMultiplicative . stimesMonoid n . Multiplicative $ x

-- | @since 1.0
instance MultiplicativeMonoid Integer where
  {-# INLINEABLE one #-}
  one = 1
  {-# INLINEABLE abs #-}
  abs = Prelude.abs
  {-# INLINEABLE signum #-}
  signum = Prelude.signum

-- | @since 1.0
class
  (MultiplicativeMonoid a, MultiplicativeMonoid b, ZeroExtendable b a) =>
  MultiplicativeGroup a b
    | b -> a
  where
  {-# MINIMAL reciprocal #-}

  -- | @since 1.0
  (/) :: a -> b -> a
  x / y = x * (zeroExtend . reciprocal $ y)

  -- | This can currently trigger ambiguity, due to the direction of the fundep.
  --
  -- @since 1.0
  reciprocal :: b -> b

  -- This uses a default \'exponentiation by squaring\' implementation.
  --
  -- @since 1.0
  powInteger :: b -> Integer -> b
  powInteger x i = getMultiplicative . gtimes i . Multiplicative $ x

-- | @since 1.0
class (Eq a, Eq b, AdditiveMonoid a) => ZeroExtendable b a | b -> a where
  -- | @since 1.0
  removeZero :: a -> Maybe b

  -- | @since 1.0
  zeroExtend :: b -> a

-- | @since 1.0
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
instance (MultiplicativeGroup a b) => Group (Multiplicative b) where
  {-# INLINEABLE inverse #-}
  inverse (Multiplicative x) = Multiplicative . reciprocal $ x
