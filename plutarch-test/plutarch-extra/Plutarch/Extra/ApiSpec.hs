module Plutarch.Extra.ApiSpec (spec) where

import Plutarch.Extra.Api
import Plutarch.Prelude

import Plutarch.ApiSpec (d0Dat, inp, validContext0, validOutputs0)
import Plutarch.Test
import Test.Hspec

spec :: Spec
spec = do
  describe "extra.api" $ do
    let ctx = validContext0
    plutarchDevFlagDescribe . pgoldenSpec $ do
      "findOwnInput"
        @| findOwnInput # ctx @-> \res ->
          passert $ res #== pcon (PJust $ pconstant inp)
      "getContinuingOutputs"
        @| getContinuingOutputs # ctx @-> \txOuts ->
          passert $ txOuts #== pconstant validOutputs0
      "findDatum"
        @| findDatum # pconstant "d0" # (pfield @"txInfo" # ctx) @-> \res ->
          passert $ res #== pcon (PJust $ pconstant d0Dat)
