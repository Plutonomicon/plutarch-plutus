module Plutarch.Extra.PairSpec (spec) where

import Hedgehog (Property)

import qualified Plutarch.Test.Property.Gen as EGen
import Plutarch.Test.Property.Util (haskPlutEquiv, marshal, viaBoth, viaPEq)

import Plutarch.Prelude
import Plutarch.Test

import Control.Arrow (first, second)

import Plutarch.Extra.Pair (pfirst, pfst, psecond, psnd)

import Test.Syd (Spec, describe, it)
import Test.Syd.Hedgehog ()

spec :: Spec
spec = describe "pair" $ do
  it "fst test" fstTest
  it "snd test" sndTest
  it "first test" firstTest
  it "second test" secondTest
  pgoldenSpec $ do
    "fst" @\ do
      "exa1" @| pfst # marshal ((1, 2) :: (Integer, Rational))
    "snd" @\ do
      "exa1" @| psnd # marshal ((1, 2) :: (Integer, Rational))
    "first" @\ do
      "exa1" @| pfirst # plam (* 2) # marshal ((1, 2) :: (Integer, Rational))
    "second" @\ do
      "exa1" @| psecond # plam (* 2) # marshal ((1, 2) :: (Integer, Rational))

fstTest :: Property
fstTest =
  haskPlutEquiv
    viaBoth
    (fst @Integer @Rational)
    pfst
    EGen.pair

sndTest :: Property
sndTest =
  haskPlutEquiv
    viaBoth
    (snd @Integer @Rational)
    psnd
    EGen.pair

firstTest :: Property
firstTest =
  haskPlutEquiv
    viaPEq
    (first (* 2) :: (Integer, Rational) -> (Integer, Rational))
    (pfirst # plam (* 2))
    EGen.pair

secondTest :: Property
secondTest =
  haskPlutEquiv
    viaPEq
    (second (* 2) :: (Integer, Rational) -> (Integer, Rational))
    (psecond # plam (* 2))
    EGen.pair
