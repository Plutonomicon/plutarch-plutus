{-# LANGUAGE Trustworthy #-}

{- | Module: Plutarch.Numeric
 Copyright: (C) MLabs 2022
 License: MIT
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 Improved numerical hierarchy.
-}
module Plutarch.Numeric (
  -- * Types

  -- ** Haskell
  Nat.Natural,
  NZN.NZNatural,
  NZI.NZInteger,
  Rat.Ratio,

  -- ** Plutarch
  Nat.PNatural,
  NZN.PNZNatural,
  NZI.PNZInteger,
  Rat.PRatio,

  -- ** Helpers
  Monoidal.Additive (..),
  Monoidal.Multiplicative (..),

  -- * Type classes

  -- ** Additive
  Additive.AdditiveSemigroup (..),
  Additive.AdditiveMonoid (..),
  Additive.AdditiveGroup (..),
  Additive.AdditiveCMM (..),

  -- ** Multiplicative
  Multiplicative.MultiplicativeSemigroup (..),
  Multiplicative.MultiplicativeMonoid (..),

  -- ** Combination
  Combination.Distributive (..),
  Combination.Euclidean (..),
  Combination.Arithmetical (..),
  Combination.Divisible (..),

  -- * Functions
  
  -- ** Reductions
  Monoidal.sum1,
  Monoidal.product1,
  Monoidal.sum,
  Monoidal.product,
  Monoidal.sumNZ,
  Monoidal.productNZ,

  -- ** Conversions

  -- *** Haskell
  Nat.toNatural,
  Nat.toAbsNatural,

  -- *** Plutarch
  Nat.ptoNatural,
  Nat.ptoAbsNatural,

  -- ** Fraction-related

  -- *** Haskell
  Rat.numerator,
  Rat.denominator,

  -- *** Plutarch
  Rat.pnumerator,
  Rat.pdenominator,
  Rat.pmatchRatio,
  Rat.pmatchRatios,

  -- ** Other
  (Combination.^-^),
) where

import qualified Plutarch.Numeric.Natural as Nat
import qualified Plutarch.Numeric.NZNatural as NZN
import qualified Plutarch.Numeric.NZInteger as NZI
import qualified Plutarch.Numeric.Ratio as Rat
import qualified Plutarch.Numeric.Monoidal as Monoidal
import qualified Plutarch.Numeric.Additive as Additive
import qualified Plutarch.Numeric.Multiplicative as Multiplicative
import qualified Plutarch.Numeric.Combination as Combination
