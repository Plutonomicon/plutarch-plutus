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
  Combination.RemoveZero (..),
  Combination.Euclidean (..),
  Combination.Arithmetical (..),
  Combination.Divisible (..),

  -- ** Fraction-related

  -- *** Haskell
  Fractional.Fractionable (..),

  -- *** Plutarch
  Fractional.PFractionable (..),

  -- * Functions

  -- ** Reductions
  Monoidal.sum1,
  Monoidal.product1,
  Monoidal.sum,
  Monoidal.product,
  Monoidal.sumNZ,
  Monoidal.productNZ,

  -- ** Scaling
  Monoidal.scaleNZNatural,
  Monoidal.scaleNatural,
  Monoidal.scaleInteger,

  -- ** Exponentiation
  Monoidal.powNZNatural,
  Monoidal.powNatural,
  Monoidal.powInteger,
  Monoidal.powIntegerZero,

  -- ** Conversions

  -- *** Haskell
  Nat.toNatural,
  Nat.toAbsNatural,

  -- *** Plutarch
  Nat.ptoNatural,
  Nat.ptoAbsNatural,

  -- ** Fraction-related

  -- *** Haskell
  Rat.ratio,
  Rat.numerator,
  Rat.denominator,

  -- *** Plutarch
  Rat.pconRatio,
  Rat.pnumerator,
  Rat.pdenominator,
  Rat.pmatchRatio,
  Rat.pmatchRatios,
) where

import Plutarch.Numeric.Additive qualified as Additive
import Plutarch.Numeric.Combination qualified as Combination
import Plutarch.Numeric.Fractional qualified as Fractional
import Plutarch.Numeric.Monoidal qualified as Monoidal
import Plutarch.Numeric.Multiplicative qualified as Multiplicative
import Plutarch.Numeric.NZInteger qualified as NZI
import Plutarch.Numeric.NZNatural qualified as NZN
import Plutarch.Numeric.Natural qualified as Nat
import Plutarch.Numeric.Ratio qualified as Rat
