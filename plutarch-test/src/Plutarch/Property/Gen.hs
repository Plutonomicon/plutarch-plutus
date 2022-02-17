module Plutarch.Property.Gen (
  rationalGen,
  integerGen,
  genList,
  genNonEmpty,
  genMaybe,
  genPair,
  genPair',
  genPosInt,
  genPosRat,
  genPerm,
) where

import Control.Monad (MonadPlus, join, liftM2, mfilter)
import Data.List (sortOn)
import Data.Ratio ((%))

import Hedgehog (MonadGen)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

integerGen :: MonadGen g => g Integer
integerGen = Gen.integral (Range.linear (-1_000_000_000) 1_000_000_000)

rationalGen :: (MonadPlus g, MonadGen g) => g Rational
rationalGen = liftM2 (%) integerGen (mfilter (/= 0) integerGen)

genList :: MonadGen g => g a -> g [a]
genList = Gen.list (Range.linear 0 100)

genNonEmpty :: MonadGen g => g a -> g [a]
genNonEmpty = Gen.list (Range.linear 1 100)

genMaybe :: MonadGen g => g a -> g (Maybe a)
genMaybe g = Gen.choice [pure Nothing, Just <$> g]

genPair :: MonadGen g => g a -> g b -> g (a, b)
genPair = liftM2 (,)

genPair' :: MonadGen g => g a -> g (a, a)
genPair' = join genPair

genPosInt :: MonadGen g => g Integer
genPosInt = Gen.integral (Range.linear 1 1_000_000_000)

genPosRat :: MonadGen g => g Rational
genPosRat = liftM2 (%) genPosInt genPosInt

genPerm :: MonadGen g => [a] -> g [a]
genPerm xs = do
  vs <- Gen.list (Range.singleton $ length xs) integerGen
  let xs' = map snd $ sortOn fst $ zip vs xs
  return xs'
