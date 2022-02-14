module Plutarch.Numeric (
  -- * Types

  -- ** Plutarch
  PNatural,
  PNZNatural,
  PNZInteger,

  -- ** Haskell
  Natural,
  NZNatural,
  NZInteger,

  -- ** Helpers
  Additive.Additive (..),
  Multiplicative.Multiplicative (..),

  -- * Type classes

  -- ** Additive
  Additive.AdditiveSemigroup (..),
  Additive.AdditiveMonoid (..),
  Additive.AdditiveGroup (..),
  Additive.AdditiveCMM (..),

  -- ** Multiplicative
  Multiplicative.MultiplicativeSemigroup (..),
  Multiplicative.MultiplicativeMonoid (..),
  Multiplicative.MultiplicativeGroup (..),

  -- ** Combination
  Ring.Semirig (..),
  Ring.Semiring (..),
  Ring.Ring (..),

  -- ** Other
  Division.Fractionable (..),
  Division.Euclidean (..),
  Multiplicative.ZeroExtendable (..),
  Group.Group (..),
) where

import Plutarch.Numeric.Additive qualified as Additive
import Plutarch.Numeric.Division qualified as Division
import Plutarch.Numeric.Group qualified as Group
import Plutarch.Numeric.Multiplicative qualified as Multiplicative
import Plutarch.Numeric.NZInteger (NZInteger, PNZInteger)
import Plutarch.Numeric.NZNatural (NZNatural, PNZNatural)
import Plutarch.Numeric.Natural (Natural, PNatural)
import Plutarch.Numeric.Ring qualified as Ring
