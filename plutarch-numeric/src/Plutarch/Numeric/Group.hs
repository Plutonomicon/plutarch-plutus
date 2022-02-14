module Plutarch.Numeric.Group (
  -- * Type class
  Group (..),
) where

import Data.Semigroup (stimesMonoid)

-- | @since 1.0
class (Monoid m) => Group m where
  {-# MINIMAL inverse #-}

  -- | @since 1.0
  inverse :: m -> m

  -- | @since 1.0
  {-# INLINEABLE gtimes #-}
  gtimes :: Integer -> m -> m
  gtimes i x = case signum i of
    0 -> mempty
    1 -> stimesMonoid i x
    _ -> inverse . stimesMonoid (negate i) $ x
