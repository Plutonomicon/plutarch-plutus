module Plutarch.Extra.IntegerSpec (spec) where

import Hedgehog (Property)

import Plutarch.Extra.Integer (peven, podd)
import Plutarch.Prelude

import qualified Plutarch.Test.Property.Gen as EGen
import Plutarch.Test.Property.Util (haskPlutEquiv, viaData)

import Plutarch.Test

import Test.Syd (Spec, describe, it)
import Test.Syd.Hedgehog ()

spec :: Spec
spec = describe "integesr" $ do
  it "even" evenTest
  it "odd" oddTest
  pgoldenSpec $ do
    "even" @\ do
      "3" @| peven # 3
      "4" @| peven # 4
    "odd" @\ do
      "3" @| podd # 3
      "4" @| podd # 4

evenTest :: Property
evenTest =
  haskPlutEquiv
    viaData
    (even @Integer)
    peven
    EGen.integer

oddTest :: Property
oddTest =
  haskPlutEquiv
    viaData
    (odd @Integer)
    podd
    EGen.integer
