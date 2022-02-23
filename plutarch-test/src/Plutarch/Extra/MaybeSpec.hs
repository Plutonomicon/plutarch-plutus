module Plutarch.Extra.MaybeSpec (spec) where

import Hedgehog (Property)

import Data.Maybe (fromJust)
import qualified Plutarch.Test.Property.Gen as EGen
import Plutarch.Test.Property.Util (haskPlutEquiv, viaBothPartial)

import Plutarch.Extra.Maybe (pfromJust)

import Test.Syd (Spec, describe, it)
import Test.Syd.Hedgehog ()

spec :: Spec
spec =
  describe "maybeTests" $ do
    it "fromJust test" fromJustTest

fromJustTest :: Property
fromJustTest =
  haskPlutEquiv
    viaBothPartial
    (fromJust @Integer)
    pfromJust
    (EGen.maybeOf EGen.integer)
