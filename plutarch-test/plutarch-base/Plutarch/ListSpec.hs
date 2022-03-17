module Plutarch.ListSpec (spec, integerList) where

import Test.Syd
import Test.Syd.Hedgehog ()

import Plutarch.List (pconvertLists, pfoldl')
import Plutarch.Prelude
import Plutarch.Test

import Hedgehog (Property)

import Data.List (find)

import Plutarch.Test.Property.Gen (genList, integerGen)
import Plutarch.Test.Property.Util (haskPlutEquiv, marshal, viaBothPartial, viaPEq)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

integerList :: [Integer] -> Term s (PList PInteger)
integerList xs = pconvertLists #$ pconstant @(PBuiltinList PInteger) xs

spec :: Spec
spec = do
  describe "list" $ do
    describe "properties" $ do
      describe "find" $ do
        it "baseAgreement" $ findTest
      describe "elemAt" $ do
        it "base agreement" elemAtTest
    describe "goldens" . pgoldenSpec $ do
      let xs10 :: Term _ (PList PInteger)
          xs10 = integerList [1 .. 10]
      "pmatch" @| (pmatch (integerList [1, 3, 1]) $ \_ -> perror) @-> pfails
      "phead" @| 1 #== (phead # xs10) @-> passert
      "ptail" @| integerList [2 .. 10] #== ptail # xs10 @-> passert
      "pnull" @\ do
        "nonempty" @| (pnot #$ pnull # xs10) @-> passert
        "empty" @| (pnull # integerList []) @-> passert
      "pconcat" @\ do
        "identity" @| (pconcat # xs10 # pnil #== pconcat # pnil # xs10) #&& (pconcat # pnil # xs10 #== xs10) @-> passert
      "pmap" @\ do
        "eg" @| pmap # (plam $ \x -> x + x) # xs10 #== (integerList $ fmap (* 2) [1 .. 10]) @-> passert
        "identity" @| pmap @PList # (plam $ \(x :: Term _ PInteger) -> x) # pnil #== pnil @-> passert
      "pfilter" @\ do
        "evens" @| pfilter # (plam $ \x -> pmod # x # 2 #== 0) # xs10 #== integerList [2, 4, 6, 8, 10] @-> passert
        "gt5" @| pfilter # (plam $ \x -> 5 #< x) # xs10 #== integerList [6 .. 10] @-> passert
      "pzipWith" @\ do
        "double" @| pzipWith' (+) # xs10 # xs10 #== integerList (fmap (* 2) [1 .. 10]) @-> passert
      "pfoldl" @\ do
        "nonempty0" @| pfoldl # plam (-) # 0 # xs10 #== pconstant (foldl (-) 0 [1 .. 10]) @-> passert
        "nonempty1" @| pfoldl' (-) # 0 # xs10 #== pconstant (foldl (-) 0 [1 .. 10]) @-> passert
        "empty0" @| pfoldl # plam (-) # 0 # integerList [] #== pconstant 0 @-> passert
        "empty1" @| pfoldl' (-) # 0 # integerList [] #== pconstant 0 @-> passert
      "elemAt" @\ do
        "elemAt_3_[1..10]" @| pelemAt # 3 # marshal [1 .. 10 :: Integer]
        "elemAt_0_[1..10]" @| pelemAt # 0 # marshal [1 .. 10 :: Integer]
        "elemAt_9_[1..10]" @| pelemAt # 9 # marshal [1 .. 10 :: Integer]
      "find" @\ do
        "find_(==3)_[1..4]" @| pfind # plam (#== 3) #$ marshal [1 .. 4 :: Integer]
        "find_(==5)_[1..4]" @| pfind # plam (#== 5) #$ marshal [1 .. 4 :: Integer]

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

elemAtTest :: Property
elemAtTest =
  haskPlutEquiv
    viaBothPartial
    elemAt
    pelemAt
    (Gen.integral $ Range.linear (-10) 100, Gen.list (Range.linear 0 100) integerGen)

elemAt :: Integer -> [Integer] -> Integer
elemAt n xs = xs !! fromInteger n
