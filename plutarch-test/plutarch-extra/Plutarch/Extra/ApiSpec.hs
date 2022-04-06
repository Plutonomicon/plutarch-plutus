module Plutarch.Extra.ApiSpec (spec) where

import Plutarch.Extra.Api
import Plutarch.Prelude

import Plutarch.Test
import Test.Hspec (Spec, describe)

import Plutarch.ApiSpec (validContext0)

spec :: Spec
spec = do
  describe "extra.api" $ do
    let ctx = validContext0
    plutarchDevFlagDescribe . pgoldenSpec $ do
      "findOwnInput"
        @| findOwnInput # ctx
      "getContinuingOutputs"
        @| getContinuingOutputs # ctx
      "findDatum"
        @| findDatum # pconstant "d0" # (pfield @"txInfo" # ctx)
