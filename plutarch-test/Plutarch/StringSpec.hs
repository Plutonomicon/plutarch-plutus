module Plutarch.StringSpec (spec) where

import Test.Syd

import Plutarch.Prelude
import Plutarch.Test

spec :: Spec
spec = do
  describe "str" $ do
    describe "eq" $ do
      let p = "foo" #== pconstant @PString "foo"
      golden All p
      it "works" $ passert p
    describe "semigroup" $ do
      let s1 = pconstant @PString "foo"
          s2 = pconstant @PString "bar"
      golden All $ s1 <> s2
      it "laws" $ do
        passert $ (mempty <> s1) #== s1
        passert $ s1 #== (mempty <> s1)
      it "concats" $ do
        passert $ s1 <> s2 #== pconstant @PString "foobar"
      it "mempty" $ do
        passert $ mempty #== pconstant @PString ""
