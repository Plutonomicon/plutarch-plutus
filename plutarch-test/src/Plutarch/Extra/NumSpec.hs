module Plutarch.Extra.NumSpec (
  spec,
) where

import Plutarch.Extra.Num (pexp, pproduct, psum)

import qualified Plutarch.Test.Property.Gen as EGen
import Plutarch.Test.Property.Util (Marshal (marshal), haskPlutEquiv, viaBoth, viaBothPartial)

import Hedgehog (Property)
import qualified Hedgehog.Gen as HGen
import qualified Hedgehog.Range as Range

import Plutarch.Prelude
import Plutarch.Test

import Test.Syd (Spec, describe, it)
import Test.Syd.Hedgehog ()

spec :: Spec
spec = describe "num" $ do
  it "pproduct" productTest
  it "psum" sumTest
  describe "pexp" $ do
    it "Integers" intExpTest
    it "Rationals" ratExpTest
  pgoldenSpec $ do
    "product" @\ do
      "exa1" @| pproduct #$ marshal [1 .. 10 :: Integer]
      "exa1" @| pproduct #$ marshal ([] :: [Integer])
    "sum" @\ do
      "exa1" @| psum #$ marshal [1 .. 10 :: Integer]
      "exa1" @| psum #$ marshal ([] :: [Integer])
    "exp" @\ do
      "rational" @\ do
        "exa1" @| pexp # marshal (4 / 3 :: Rational) # 100
        "exa1" @| pexp # marshal (1 / 2 :: Rational) # 0
      "integer" @\ do
        "exa1" @| pexp # (4 :: Term s PInteger) # 100
        "exa1" @| pexp # (2 :: Term s PInteger) # 0

productTest :: Property
productTest =
  haskPlutEquiv
    viaBoth
    (product @[] @Rational)
    pproduct
    (EGen.listOf EGen.rational)

sumTest :: Property
sumTest =
  haskPlutEquiv
    viaBoth
    (sum @[] @Rational)
    psum
    (EGen.listOf EGen.rational)

intExpTest :: Property
intExpTest =
  haskPlutEquiv
    viaBothPartial
    ((^) @Integer @Integer)
    pexp
    (EGen.integer, HGen.integral (Range.linear (-10) 100))

ratExpTest :: Property
ratExpTest =
  haskPlutEquiv
    viaBothPartial
    ((^) @Rational @Integer)
    pexp
    (EGen.rational, HGen.integral (Range.linear (-10) 100))

-- using the normal range really slows down the test
