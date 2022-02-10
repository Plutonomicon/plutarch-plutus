module Properties.Gen (
  rationalGen,
  integerGen,
  genList,
  genMaybe,
  genPair,
  genPair',
) where

import Control.Monad (join, liftM2)
import Data.Ratio ((%))

import Hedgehog (Gen)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

integerGen :: Gen Integer
integerGen = Gen.integral (Range.linear (-1_000_000_000) 1_000_000_000)

rationalGen :: Gen Rational
rationalGen = liftM2 (%) integerGen integerGen

genList :: Gen a -> Gen [a]
genList = Gen.list (Range.linear 0 100)

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe g = Gen.choice [pure Nothing, Just <$> g]

genPair :: Gen a -> Gen b -> Gen (a, b)
genPair = liftM2 (,)

genPair' :: Gen a -> Gen (a, a)
genPair' = join genPair
