-- | Generator helpers
module Plutarch.Test.Property.Gen (
  genRational,
  genInteger,
  genList,
) where

import Control.Monad (MonadPlus, liftM2, mfilter)
import Data.Ratio ((%))

import Hedgehog (MonadGen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genInteger :: MonadGen g => g Integer
genInteger = Gen.integral (Range.linear (-1_000_000_000) 1_000_000_000)

genRational :: (MonadPlus g, MonadGen g) => g Rational
genRational = liftM2 (%) genInteger (mfilter (/= 0) genInteger)

genList :: MonadGen g => g a -> g [a]
genList = Gen.list (Range.linear 0 100)
