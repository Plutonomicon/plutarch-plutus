module Plutarch.ListSpec (spec) where

import Test.Syd
import Test.Syd.Hedgehog ()

import Plutarch.List (pconvertLists, pfoldl', pmergeSort, ptimSort)
import Plutarch.Prelude
import Plutarch.Test

import Hedgehog (Property)

import Data.List (find, sort)

import Plutarch.Test.Property.Gen (genList, integerGen)
import Plutarch.Test.Property.Util (haskPlutEquiv, marshal, viaBothPartial, viaPEq)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

integerList :: [Integer] -> Term s (PList PInteger)
integerList xs = pconvertLists #$ pconstant @(PBuiltinList PInteger) xs

spec :: Spec
spec = do
  describe "list" $ do
    let xs10 :: Term _ (PList PInteger)
        xs10 = integerList [1 .. 10]
    describe "pmatch" $ do
      let p = pmatch (integerList [1, 3, 1]) $ \_ -> perror
      golden PrintTerm p
    describe "phead" $ do
      let p = phead # xs10
      golden All p
      it "works" $ passert $ p #== 1
    describe "ptail" $ do
      let p = ptail # xs10
      golden All p
      it "works" $ passert $ p #== integerList [2 .. 10]
    describe "pnull" $ do
      let p0 = pnull # integerList []
          p1 = pnull # xs10
      goldens All [("p0", p0), ("p1", p1)]
      it "empty" $ passert p0
      it "nonempty" $ passert $ pnot # p1
    describe "pconcat" $ do
      describe "identity" $ do
        let xs :: Term s (PList PInteger)
            xs = psingleton # (fromInteger @(Term _ PInteger) 0)
            p = pconcat # xs # pnil
        golden All p
        it "works" $ passert $ p #== xs
    describe "pmap" $ do
      let p = pmap # (plam $ \x -> x + x) # xs10
      golden All p
      it "eg" $ passert $ p #== (integerList $ fmap (* 2) [1 .. 10])
      it "identity" $ passert $ pmap @PList # (plam $ \(x :: Term _ PInteger) -> x) # pnil #== pnil
    describe "pfilter" $ do
      let p1 = pfilter # (plam $ \x -> pmod # x # 2 #== 0) # xs10
          p2 = pfilter # (plam $ \x -> 5 #< x) # xs10
      goldens All [("p1", p1), ("p2", p2)]
      it "p1" $ passert $ p1 #== integerList [2, 4, 6, 8, 10]
      it "p2" $ passert $ p2 #== integerList [6 .. 10]
    describe "pzipWith" $ do
      let p = pzipWith' (+) # xs10 # xs10
      golden All p
      it "works" $ passert $ p #== integerList (fmap (* 2) [1 .. 10])
    describe "pfoldl" $ do
      let p1 = pfoldl # plam (-) # 0 # xs10
          p1' = pfoldl' (-) # 0 # xs10
          p2 = pfoldl # plam (-) # 0 # integerList []
          p2' = pfoldl' (-) # 0 # integerList []
      goldens All [("p1", p1), ("p1'", p1'), ("p2", p2), ("p2'", p2)]
      it "nonempty" $ passert $ p1 #== pconstant (foldl (-) 0 [1 .. 10])
      it "nonempty'" $ passert $ p1' #== pconstant (foldl (-) 0 [1 .. 10])
      it "empty" $ passert $ p2 #== pconstant 0
      it "empty'" $ passert $ p2' #== pconstant 0
    describe "property" $ do
      describe "find" $ do
        it "baseAgreement" $ findTest
        goldens
          All
          [ ("find_(==3)_[1..4]", pfind # plam (#== 3) #$ marshal [1 .. 4 :: Integer])
          , ("find_(==5)_[1..4]", pfind # plam (#== 5) #$ marshal [1 .. 4 :: Integer])
          ]
      describe "reverse" $ do
        it "base agreement" reverseTest
        goldens
          All
          [ ("reverse_[1..5]", preverse # marshal [1 .. 5 :: Integer])
          , ("reverse_[]", preverse # marshal ([] :: [Integer]))
          ]
      describe "elemAt" $ do
        it "base agreement" elemAtTest
        goldens
          All
          [ ("elemAt_3_[1..10]", pelemAt # 3 # marshal [1 .. 10 :: Integer])
          , ("elemAt_0_[1..10]", pelemAt # 0 # marshal [1 .. 10 :: Integer])
          , ("elemAt_9_[1..10]", pelemAt # 9 # marshal [1 .. 10 :: Integer])
          ]
      let xs1 = marshal [1 .. 10 :: Integer]
          xs2 = marshal $ reverse [1 .. 10 :: Integer]
          xs3 = marshal $ [1 .. 10 :: Integer] ++ reverse [11 .. 20]
      describe "pmergeSort" $ do
        it "baseAgreement" pmergeSortTest
        goldens
          All
          [ ("sort_xs1", pmergeSort # xs1)
          , ("sort_xs2", pmergeSort # xs2)
          , ("sort_xs3", pmergeSort # xs3)
          ]
      describe "ptimSort" $ do
        it "baseAgreement" ptimSortTest
        goldens
          All
          [ ("sort_xs1", ptimSort # xs1)
          , ("sort_xs2", ptimSort # xs2)
          , ("sort_xs3", ptimSort # xs3)
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
