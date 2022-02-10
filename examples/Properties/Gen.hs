module Properties.Gen (
  rationalGen,
  mapGen,
  smallDomainMapGen,
  integerGen,
  genList,
  genMaybe,
  genPair,
  genPair',
  genMap,
) where

import Control.Monad (join, liftM2)
import Data.Ratio ((%))

import Hedgehog (Gen)

import qualified Data.Map as M

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

integerGen :: Gen Integer
integerGen = Gen.integral (Range.linear (-1_000_000_000) 1_000_000_000)

rationalGen :: Gen Rational
rationalGen = liftM2 (%) integerGen integerGen

mapGen :: Gen (M.Map Integer Rational)
mapGen = genMap integerGen rationalGen

smallDomainMapGen :: Gen (M.Map Integer Rational)
smallDomainMapGen = genMap (Gen.integral (Range.linear 0 100)) rationalGen

genList :: Gen a -> Gen [a]
genList = Gen.list (Range.linear 0 100)

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe g = Gen.choice [pure Nothing, Just <$> g]

genPair :: Gen a -> Gen b -> Gen (a, b)
genPair = liftM2 (,)

genPair' :: Gen a -> Gen (a, a)
genPair' = join genPair

genMap :: Ord a => Gen a -> Gen b -> Gen (M.Map a b)
genMap ga gb = M.fromList <$> genList (genPair ga gb)
