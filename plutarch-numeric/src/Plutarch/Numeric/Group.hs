{-# LANGUAGE Safe #-}
{-# LANGUAGE NoDerivingVia #-}
{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}

{- | Module: Plutarch.Numeric.Group
 Copyright: (C) MLabs 2022
 License: MIT
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 Representation of the mathematical group structure.
-}
module Plutarch.Numeric.Group (
  -- * Type class
  Group (..),
) where

import Data.Semigroup (stimesMonoid)

{- | A 'Group' is a 'Monoid' where every element possesses an inverse.

 = Laws

 * @x '<>' 'inverse' x@ @=@ @'inverse' x '<>' x@ @=@ @'mempty'@
 * @'gtimes' 0 x@ @=@ @'mempty'@
 * If @i < 0@, then @'gtimes' i x = 'inverse' '.' 'gtimes' ('abs' i) '$' x@
 * If @i > 0@, then @'gtimes' i x = 'stimesMonoid' i x@
 * @x '<->' y@ @=@ @x '<>' 'inverse' y@

 All but the first law are guaranteed by the default implementations.

 @since 1.0
-}
class (Monoid m) => Group m where
  {-# MINIMAL inverse #-}

  -- | @since 1.0
  inverse :: m -> m

  -- | @since 1.0
  (<->) :: m -> m -> m
  x <-> y = x <> inverse y

  -- | @since 1.0
  {-# INLINEABLE gtimes #-}
  gtimes :: Integer -> m -> m
  gtimes i x = case signum i of
    0 -> mempty
    1 -> stimesMonoid i x
    _ -> inverse . stimesMonoid (negate i) $ x

infixr 6 <->
