{-# LANGUAGE FunctionalDependencies #-}

module Plutarch.Numeric.Division (
  -- * Type classes
  Fractionable (..),
  Euclidean (..),
) where

import Plutarch.Numeric.Multiplicative (MultiplicativeMonoid, ZeroExtendable)
import Plutarch.Numeric.NZNatural (NZNatural)
import Plutarch.Numeric.Ring (Semiring)

{- | Laws:

 * @'scale' 'one' = 'unscale' 'one' = 'id'@
 * @'scale' (n '*' m) = 'scale' n '.' 'scale' m@
 * @'unscale' n '.' 'scale' n = 'id'@
 * @'findScale' x 'one' = 'one'@
 * @'findScale' ('scale' x m) (m '*' n) = m '*' 'findScale' x n@
 * If @m = 'findScale' x n@, then 'findScale' ('unscale' x m) m = 'one'@

 @since 1.0
-}
class (Eq a) => Fractionable a where
  -- | @since 1.0
  scale :: NZNatural -> a -> a

  -- | @since 1.0
  unscale :: NZNatural -> a -> a

  -- | @since 1.0
  findScale :: a -> NZNatural -> NZNatural

-- | @since 1.0
class
  (Semiring a, MultiplicativeMonoid b, Ord b, ZeroExtendable b a) =>
  Euclidean a b
    | b -> a
  where
  -- | @since 1.0
  divMod :: a -> b -> (a, a)
