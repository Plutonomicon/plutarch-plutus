module Plutarch.Test.Property.Gen (
  integer,
  positiveInteger,
  rational,
  positiveRational,
  defaultMap,
  smallDomainMap,
  pair,
  listOf,
  shortNonEmptyListOf,
  maybeOf,
  pairOf,
  pairOfBoth,
  permutationOf,
  mapOf,
) where

import Control.Monad (MonadPlus, join, liftM2, mfilter)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (sortOn)
import Data.Ratio ((%))

import Hedgehog (MonadGen)

import qualified Data.Map as M

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

integer :: MonadGen g => g Integer
integer = Gen.integral (Range.linear (-1_000_000_000) 1_000_000_000)

positiveInteger :: MonadGen g => g Integer
positiveInteger = Gen.integral (Range.linear 1 1_000_000_000)

rational :: (MonadPlus g, MonadGen g) => g Rational
rational = liftM2 (%) integer (mfilter (/= 0) integer)

positiveRational :: MonadGen g => g Rational
positiveRational = liftM2 (%) positiveInteger positiveInteger

defaultMap :: (MonadPlus g, MonadGen g) => g (M.Map Integer Rational)
defaultMap = mapOf integer rational

-- Generates a Map Integer Rational where the integers
-- are drawn from a small domain (0 to 100)
-- usefull for tests where two maps having a
-- signifigant interesection is an important case
-- ie. unionWith mapSub etc
smallDomainMap :: (MonadPlus g, MonadGen g) => g (M.Map Integer Rational)
smallDomainMap = mapOf (Gen.integral (Range.linear 0 100)) rational

pair :: (MonadPlus g, MonadGen g) => g (Integer, Rational)
pair = pairOf integer rational

-- Transformers

listOf :: MonadGen g => g a -> g [a]
listOf = Gen.list (Range.linear 0 100)

shortNonEmptyListOf :: MonadGen g => g a -> g [a]
shortNonEmptyListOf = Gen.list (Range.linear 1 10)

maybeOf :: MonadGen g => g a -> g (Maybe a)
maybeOf g = Gen.choice [pure Nothing, Just <$> g]

pairOf :: MonadGen g => g a -> g b -> g (a, b)
pairOf = liftM2 (,)

-- Pair with the same gen for fst and snd
pairOfBoth :: MonadGen g => g a -> g (a, a)
pairOfBoth = join pairOf

-- Given a list generate a random
-- permutation of that list
permutationOf :: MonadGen g => [a] -> g [a]
permutationOf xs = do
  -- generates a list of random integers the same length as xs
  is <- Gen.list (Range.singleton $ length xs) integer
  -- zip the lists sort on the random integers then discard the integers
  return $ xs & zip is & sortOn fst <&> snd

mapOf :: (MonadGen g, Ord a) => g a -> g b -> g (M.Map a b)
mapOf ga gb = M.fromList <$> listOf (pairOf ga gb)
