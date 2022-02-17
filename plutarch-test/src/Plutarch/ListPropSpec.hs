module Plutarch.ListPropSpec (spec) where

import Hedgehog (Property)

import Plutarch.List (pelemAt, pfind, pmergeSort, preverse, ptimSort)
import Plutarch.Prelude

import Data.List (find, sort)

import Plutarch.Property.Gen (genList, integerGen)
import Plutarch.Property.Util (haskPlutEquiv, marshal, viaBothPartial, viaPEq)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Plutarch.Test (PlutarchGolden (All), goldens)
import Test.Syd (Spec, describe, it)
import Test.Syd.Hedgehog ()

spec :: Spec
spec = describe "list spec" $ do
  describe "find" $ do
    it "base agreement" $ findTest
    goldens
      All
      [ ("find (== 3) [1..4]", pfind # plam (#== 3) #$ marshal [1 .. 4 :: Integer])
      , ("find (== 5) [1..4]", pfind # plam (#== 5) #$ marshal [1 .. 4 :: Integer])
      ]
  describe "reverse" $ do
    it "base agreement" reverseTest
    goldens
      All
      [ ("reverse [1..5]", preverse # marshal [1 .. 5 :: Integer])
      , ("reverse []", preverse # marshal ([] :: [Integer]))
      ]
  describe "elemAt" $ do
    it "base agreement" elemAtTest
    goldens
      All
      [ ("elemAt 3 [1..10]", pelemAt # 3 # marshal [1 .. 10 :: Integer])
      , ("elemAt 0 [1..10]", pelemAt # 0 # marshal [1 .. 10 :: Integer])
      , ("elemAt 9 [1..10]", pelemAt # 9 # marshal [1 .. 10 :: Integer])
      ]
  let xs1 = marshal [1 .. 10 :: Integer]
      xs2 = marshal $ reverse [1 .. 10 :: Integer]
      xs3 = marshal $ [1 .. 10 :: Integer] ++ reverse [11 .. 20]
  describe "pmergeSort" $ do
    it "base agreement" pmergeSortTest
    goldens
      All
      [ ("sort xs1", pmergeSort # xs1)
      , ("sort xs2", pmergeSort # xs2)
      , ("sort xs3", pmergeSort # xs3)
      ]
  describe "ptimSort" $ do
    it "base agreement" ptimSortTest
    goldens
      All
      [ ("sort xs1", ptimSort # xs1)
      , ("sort xs2", ptimSort # xs2)
      , ("sort xs3", ptimSort # xs3)
      ]

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
