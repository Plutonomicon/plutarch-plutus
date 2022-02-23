module Plutarch.Extra.ListSpec (spec) where

import Hedgehog (Property)

import Plutarch.Prelude

import Plutarch.Extra.List (mergeSort, timSort)
import Plutarch.Extra.Maybe ()
import Plutarch.Test

import Data.List (sort)

import qualified Plutarch.Test.Property.Gen as EGen
import Plutarch.Test.Property.Util (haskPlutEquiv, marshal, viaPEq)

import Test.Syd (Spec, describe, it)
import Test.Syd.Hedgehog ()

spec :: Spec
spec = describe "list" $ do
  let xs1 = marshal [1 .. 10 :: Integer]
      xs2 = marshal $ reverse [1 .. 10 :: Integer]
      xs3 = marshal $ [1 .. 10 :: Integer] ++ reverse [11 .. 20]
  it "mergeSort" mergeSortTest
  it "timSort" timSortTest
  pgoldenSpec $ do
    "mergeSort" @\ do
      "xs1" @| mergeSort # xs1
      "xs2" @| mergeSort # xs2
      "xs3" @| mergeSort # xs3
    "timSort" @\ do
      "xs1" @| timSort # xs1
      "xs2" @| timSort # xs2
      "xs3" @| timSort # xs3

mergeSortTest :: Property
mergeSortTest =
  haskPlutEquiv
    viaPEq
    (sort @Integer)
    mergeSort
    (EGen.listOf EGen.integer)

timSortTest :: Property
timSortTest =
  haskPlutEquiv
    viaPEq
    (sort @Integer)
    timSort
    (EGen.listOf EGen.integer)
