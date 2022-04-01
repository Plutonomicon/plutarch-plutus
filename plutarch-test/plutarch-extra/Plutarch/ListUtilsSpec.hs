{-# LANGUAGE TemplateHaskell #-}

module Plutarch.ListUtilsSpec (spec, props) where

import Plutarch.Prelude
import Plutarch.Test

import Hedgehog (Property, checkParallel, discover)

import Plutarch.Test.Property
import Plutarch.Test.Property.Gen (genInteger, genList)

import Plutarch.ListUtils (pcheckSorted, preverse)

spec :: TrailSpec
spec = do
  describe "extra.listutils" $ do
    pgoldenSpec $ do
      "reverse" @\ do
        "reverse_[1..5]" @| preverse # marshal [1 .. 5 :: Integer]
      "isSorted" @\ do
        "[1..10]" @| pcheckSorted # marshal [1 .. 10 :: Integer] @-> passert
        "reverse_[1..10]" @| (pnot #$ pcheckSorted #$ marshal $ reverse [1 .. 10 :: Integer]) @-> passert
        "reverse_[]" @| preverse # marshal ([] :: [Integer])

props :: IO Bool
props = checkParallel $$(discover)

-- plutarch level reversing behaves like haskell level reversing
prop_preverseEquiv :: Property
prop_preverseEquiv = do
  prop_haskEquiv
    @( 'OnPEq)
    @( 'TotalFun)
    (reverse :: [Integer] -> [Integer])
    preverse
    (genList genInteger :* Nil)
