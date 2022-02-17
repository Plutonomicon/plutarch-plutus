module Plutarch.ListPropSpec (spec) where

import Hedgehog (Property)

import Plutarch.List (pmergeSort, pelemAt, pfind, preverse, ptimSort)
import Plutarch.Prelude

import Data.List (find, sort)

import Gen (genList, integerGen)
import Util (haskPlutEquiv, viaBothPartial, viaPEq)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Syd (Spec, describe, it)
import Test.Syd.Hedgehog ()

spec :: Spec
spec = describe "list spec" $ do
  it "find" findTest
  it "reverse" reverseTest
  it "elemAt" elemAtTest
  it "pmergeSort" pmergeSortTest
  it "ptimSort" ptimSortTest

findTest :: Property
findTest =
  haskPlutEquiv
    viaPEq
    (find @[] @Integer even)
    (pfind # peven)
    (genList integerGen)
      where
        peven :: Term s (PInteger :--> PBool)
        peven = plam $ \n -> pmod # n # 2 #== 0

reverseTest :: Property
reverseTest =
  haskPlutEquiv
    viaPEq
    (reverse :: [Integer] -> [Integer])
    preverse
    (genList integerGen)

elemAtTest :: Property
elemAtTest =
  haskPlutEquiv
    viaBothPartial
    elemAt
    pelemAt
    (Gen.integral $ Range.linear (-10) 100, Gen.list (Range.linear 0 100) integerGen)

elemAt :: Integer -> [Integer] -> Integer
elemAt n xs = xs !! fromInteger n

pmergeSortTest :: Property
pmergeSortTest =
  haskPlutEquiv
    viaPEq
    (sort @Integer)
    pmergeSort
    (genList integerGen)

ptimSortTest :: Property
ptimSortTest =
  haskPlutEquiv
    viaPEq
    (sort @Integer)
    ptimSort
    (genList integerGen)
