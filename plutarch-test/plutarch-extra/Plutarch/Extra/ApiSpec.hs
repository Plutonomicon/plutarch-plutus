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
      "pfindOwnInput"
        @| pfindOwnInput # ctx @-> \res ->
          passert $ res #== pcon (PJust $ pconstant inp)
      "pgetContinuingOutputs"
        @| pgetContinuingOutputs # ctx @-> \txOuts ->
          passert $ txOuts #== pconstant validOutputs0
      "pfindDatum"
        @| pfindDatum # pconstant "d0" # (pfield @"txInfo" # ctx) @-> \res ->
          passert $ res #== pcon (PJust $ pconstant d0Dat)
