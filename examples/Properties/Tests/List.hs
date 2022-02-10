module Properties.Tests.List (listTests) where

import Hedgehog (Gen, Property)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Plutarch.List (mergesort, pelemAt, pfind, preverse, timSort)
import Plutarch.Prelude

import Data.List (find, sort)

import Properties.Gen (genList, integerGen)
import Properties.Utils (haskPlutEquiv, viaBothPartial, viaPEq)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

listTests :: TestTree
listTests =
  testGroup
    "listTests"
    [ testProperty "find test" findTest
    , testProperty "reverse test" reverseTest
    , testProperty "elemAt test" elemAtTest
    , testProperty "mergesort test" mergesortTest
    , testProperty "timSort test" timsortTest
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
    peven = phoistAcyclic $ plam $ \n -> (pmod # n # 2) #== 0

reverseTest :: Property
reverseTest =
  haskPlutEquiv
    viaPEq
    (reverse @Integer)
    preverse
    (genList integerGen)

elemAtTest :: Property
elemAtTest =
  haskPlutEquiv
    viaBothPartial
    elemAt
    pelemAt
    (Gen.integral $ Range.linear (-10) 100 :: Gen Integer, Gen.list (Range.linear 0 100) integerGen)

elemAt :: Integer -> [Integer] -> Integer
elemAt n xs = xs !! fromInteger n

mergesortTest :: Property
mergesortTest =
  haskPlutEquiv
    viaPEq
    (sort @Integer)
    mergesort
    (genList integerGen)

timsortTest :: Property
timsortTest =
  haskPlutEquiv
    viaPEq
    (sort @Integer)
    timSort
    (genList integerGen)
