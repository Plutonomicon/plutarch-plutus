module Plutarch.Extra.ApiSpec (spec) where

import Plutarch.Extra.Api
import Plutarch.Prelude

import Plutarch.Test
import Test.Hspec (Spec, describe)

import qualified Plutarch.TryFromSpec as TFS

spec :: Spec
spec = do
  describe "extra.api" $ do
    -- Let's reuse the mock ctx from `Plutarch.TryFromSpec`, because that has
    -- datums.
    let ctx = TFS.ctx TFS.validOutputs0 TFS.validList1
    pgoldenSpec $ do
      "findOwnInput"
        @| findOwnInput # ctx
      "getContinuingOutputs"
        @| getContinuingOutputs # ctx
      "findDatum"
        @| findDatum # pconstant "d0" # (pfield @"txInfo" # ctx)
