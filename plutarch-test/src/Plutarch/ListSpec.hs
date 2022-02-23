module Plutarch.ListSpec (spec) where

import Test.Syd
import Test.Syd.Hedgehog ()

import Plutarch.List (pconvertLists, pfoldl')
import Plutarch.Prelude
import Plutarch.Test

import Hedgehog (Property)

import Data.List (find)

import qualified Plutarch.Test.Property.Gen as EGen
import Plutarch.Test.Property.Util (haskPlutEquiv, marshal, viaBothPartial, viaPEq)

import qualified Hedgehog.Gen as HGen
import qualified Hedgehog.Range as Range

integerList :: [Integer] -> Term s (PList PInteger)
integerList xs = pconvertLists #$ pconstant @(PBuiltinList PInteger) xs

spec :: Spec
spec = do
  describe "list" $ do
    let xs10 :: Term _ (PList PInteger)
        xs10 = integerList [1 .. 10]
    describe "type" . pgoldenSpec $ do
      "phead" @| phead # xs10 @== pconstant @PInteger 1
      "ptail" @| ptail # xs10 @== integerList [2 .. 10]
      let matchP = pmatch (integerList [1, 3, 1]) $ \case
            PSNil -> perror
            PSCons x _ -> x
      "pmatch" @| matchP @== pconstant @PInteger 1
    describe "fun" . pgoldenSpec $ do
      "pnull" @\ do
        "empty" @| pnull # (integerList []) @-> passert
        "nonempty" @| pnot # (pnull # xs10) @-> passert
      "pconcat" @\ do
        let xs :: Term s (PList PInteger)
            xs = psingleton # (fromInteger @(Term _ PInteger) 0)
        "identity" @| pconcat # xs # pnil @== xs
      "pmap" @\ do
        "eg" @| pmap # (plam $ \x -> x + x) # xs10
          @== (integerList $ fmap (* 2) [1 .. 10])
        "identity" @| pmap @PList # (plam $ \(x :: Term _ PInteger) -> x) # pnil
          @== pnil @PList
      "pfilter" @\ do
        "1" @| pfilter # (plam $ \x -> pmod # x # 2 #== 0) # xs10
          @== integerList [2, 4, 6, 8, 10]
        "2" @| pfilter # (plam $ \x -> 5 #< x) # xs10
          @== integerList [6 .. 10]
      "pzipWith" @| pzipWith' (+) # xs10 # xs10
        @== (integerList (fmap (* 2) [1 .. 10]))
      "pfoldl" @\ do
        "primed" @\ do
          "nonempty" @| pfoldl' (-) # 0 # xs10
            @== pconstant @PInteger (foldl (-) 0 [1 .. 10])
          "empty" @| pfoldl' (-) # 0 # integerList []
            @== pconstant @PInteger 0
        "primed" @\ do
          "nonempty" @| pfoldl # plam (-) # 0 # xs10
            @== pconstant @PInteger (foldl (-) 0 [1 .. 10])
          "empty" @| pfoldl # plam (-) # 0 # integerList []
            @== pconstant @PInteger 0
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
      describe "isSorted" $ do
        it "[1..10]" $ passert $ pcheckSorted # marshal [1 .. 10 :: Integer]
        it "reverse_[1..10]" $ passert $ pnot #$ pcheckSorted #$ marshal $ reverse [1 .. 10 :: Integer]

findTest :: Property
findTest =
  haskPlutEquiv
    viaPEq
    (find @[] @Integer even)
    (pfind # peven)
    (EGen.listOf EGen.integer)
  where
    peven :: Term s (PInteger :--> PBool)
    peven = plam $ \n -> pmod # n # 2 #== 0

reverseTest :: Property
reverseTest =
  haskPlutEquiv
    viaPEq
    (reverse :: [Integer] -> [Integer])
    preverse
    (EGen.listOf EGen.integer)

elemAtTest :: Property
elemAtTest =
  haskPlutEquiv
    viaBothPartial
    elemAt
    pelemAt
    (HGen.integral $ Range.linear (-10) 100, HGen.list (Range.linear 0 100) EGen.integer)

elemAt :: Integer -> [Integer] -> Integer
elemAt n xs = xs !! fromInteger n
