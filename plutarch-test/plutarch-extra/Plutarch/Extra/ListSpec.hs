module Plutarch.Extra.ListSpec (spec) where

import Plutarch.Extra.List (
  pcheckSorted,
  pisUniq,
  pmapMaybe,
  pmergeBy,
  pmsort,
  pnubSort,
  preplicate,
  preverse,
 )
import Plutarch.Extra.Maybe (pjust)
import Plutarch.Prelude

import Data.List (nub, sort)
import Data.Maybe (mapMaybe)
import Hedgehog (Property)
import qualified Hedgehog.Gen as Gen
import Hedgehog.Internal.Property (propertyTest)
import qualified Hedgehog.Range as Range
import Plutarch.Test
import Plutarch.Test.Property
import Plutarch.Test.Property.Gen (genInteger, genList)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

spec :: Spec
spec = do
  describe "extra.listutils" $ do
    describe "properties" $ do
      describe "reverse" $ do
        it "plutarch level reversing behaves like haskell level reversing" . hedgehog . propertyTest $ prop_preverseEquiv
      describe "sort" $ do
        it "plutarch level sorting behaves like haskell level sorting" . hedgehog . propertyTest $ prop_pmsortEquiv
      describe "mergeBy" $ do
        it "plutarch level 'mergeBy' behaves like haskell level counterpart" . hedgehog . propertyTest $ prop_pmergeByEquiv
      describe "pnubSort" $ do
        it "plutarch level 'pnubSort' behaves like haskell level counterpart" . hedgehog . propertyTest $ prop_pnubSort
      describe "pisUniq" $ do
        it "plutarch level 'pisUniq' behaves like haskell level counterpart" . hedgehog . propertyTest $ prop_pisUniq
      describe "pmapMaybe" $ do
        it "plutarch level 'pmapMaybe' behaves like haskell level counterpart" . hedgehog . propertyTest $ prop_pmapMaybe
      describe "preplicate" $ do
        it "plutarch level 'prelicate' behaves like haskell level counterpart" . hedgehog . propertyTest $ prop_preplicate
    pgoldenSpec $ do
      "reverse" @\ do
        "reverse_[1..5]" @| preverse # marshal [1 .. 5 :: Integer]
      "isSorted" @\ do
        "[1..10]" @| pcheckSorted # marshal [1 .. 10 :: Integer] @-> passert
        "reverse_[1..10]" @| (pnot #$ pcheckSorted #$ marshal $ reverse [1 .. 10 :: Integer]) @-> passert
        "reverse_[]" @| preverse # marshal ([] :: [Integer])
      "sort" @\ do
        "sort_[1..5]" @| pmsort # marshal ([1 .. 5 :: Integer])
        "sort_[]" @| pmsort # marshal ([] :: [Integer])
      "mergeBy" @\ do
        "mergeBy_[1..5]_[1..5]" @| pmergeBy # (plam \x y -> x #< y) # marshal [1 .. 5 :: Integer] # marshal [1 .. 5 :: Integer]
        "mergeBy_[]_[]" @| pmergeBy # (plam \x y -> x #< y) # marshal ([] :: [Integer]) # marshal ([] :: [Integer])
      "nubSort" @\ do
        "nubSort_[1, 1, 2, 2, 3, 4, 5]" @| pcheckSorted #$ pnubSort # marshal [1, 1, 2, 2, 3, 4, 5 :: Integer]
        "nubSort_[]" @| pnubSort # marshal ([] :: [Integer])
      "pisUniq" @\ do
        "isUniq_1_[1..5]" @| pisUniq # marshal [1 .. 5 :: Integer] @-> passert
        "isUniq_1_[]" @| pisUniq # marshal ([] :: [Integer])
      "pmapMaybe" @\ do
        "mapMaybe_[1..5]" @| pmapMaybe # pjust # marshal [1 .. 5 :: Integer]
        "mapMaybe_[]" @| pmapMaybe # pjust # marshal ([] :: [Integer])
        "mapMaybe_[1..5]_Nothing" @| (0 #== (plength #$ pmapMaybe # plam (const $ pcon PNothing) # marshal [1 .. 5 :: Integer])) @-> passert
      "preplicate" @\ do
        "preplicate_5_0" @| (5 #== (plength #$ preplicate @PBuiltinList # marshal (5 :: Integer) # marshal (0 :: Integer))) @-> passert
        "preplicate_0_0" @| (0 #== (plength #$ preplicate @PBuiltinList # marshal (0 :: Integer) # marshal (0 :: Integer))) @-> passert

-- plutarch level reversing behaves like haskell level reversing
prop_preverseEquiv :: Property
prop_preverseEquiv = do
  prop_haskEquiv
    @( 'OnPEq)
    @( 'TotalFun)
    (reverse :: [Integer] -> [Integer])
    preverse
    (genList genInteger :* Nil)

prop_pmsortEquiv :: Property
prop_pmsortEquiv = do
  prop_haskEquiv
    @( 'OnPEq)
    @( 'TotalFun)
    (sort :: [Integer] -> [Integer])
    pmsort
    (genList genInteger :* Nil)

prop_pmergeByEquiv :: Property
prop_pmergeByEquiv = do
  prop_haskEquiv
    @( 'OnPEq)
    @( 'TotalFun)
    ((hmergeBy cond) :: [Integer] -> [Integer] -> [Integer])
    (pmergeBy # pcond)
    (genList genInteger :* genList genInteger :* Nil)
  where
    cond x y = x < y
    hmergeBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
    hmergeBy _ xs [] = xs
    hmergeBy _ [] ys = ys
    hmergeBy f (x : xs) (y : ys)
      | f x y = x : hmergeBy f xs (y : ys)
      | otherwise = y : hmergeBy f (x : xs) ys
    pcond :: POrd a => Term s (a :--> a :--> PBool)
    pcond = plam $ \x y -> x #< y

prop_pnubSort :: Property
prop_pnubSort = do
  prop_haskEquiv
    @( 'OnPEq)
    @( 'TotalFun)
    (sort . nub :: [Integer] -> [Integer])
    pnubSort
    (genList genInteger :* Nil)

prop_pisUniq :: Property
prop_pisUniq = do
  prop_haskEquiv
    @( 'OnPEq)
    @( 'TotalFun)
    (hisUniq :: [Integer] -> Bool)
    pisUniq
    (genList genInteger :* Nil)
  where
    hisUniq x = length x == length (nub x)

prop_pmapMaybe :: Property
prop_pmapMaybe = do
  prop_haskEquiv
    @( 'OnPEq)
    @( 'TotalFun)
    (mapMaybe hcond :: [Integer] -> [Integer])
    (pmapMaybe # pcond)
    (genList genInteger :* Nil)
  where
    pcond = plam \x ->
      pif
        (x #< 5)
        (pcon PNothing)
        (pcon . PJust $ x + 10)
    hcond x = if x < 5 then Nothing else Just $ x + 10

prop_preplicate :: Property
prop_preplicate = do
  prop_haskEquiv
    @( 'OnPEq)
    @( 'TotalFun)
    (replicate . fromInteger :: Integer -> Integer -> [Integer])
    preplicate
    (manageable :* genInteger :* Nil)
  where
    manageable = Gen.integral (Range.linear 0 1_000)
