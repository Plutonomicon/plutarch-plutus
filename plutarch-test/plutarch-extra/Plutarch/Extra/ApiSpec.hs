module Plutarch.Extra.ApiSpec (spec) where

import Plutarch.Extra.Api
import Plutarch.Prelude

import Plutarch.Api.V1 (PScriptPurpose (PSpending))
import Plutarch.ApiSpec (d0Dat, inp, validContext0, validOutputs0)
import Plutarch.Extra.Monad (pmatchC)
import Plutarch.Test
import Test.Hspec

spec :: Spec
spec = do
  describe "extra.api" $ do
    let ctx = validContext0
    plutarchDevFlagDescribe . pgoldenSpec $ do
      "pfindOwnInput"
        @| ( unTermCont $ do
              ctxF <- tcont $ pletFields @["txInfo", "purpose"] ctx
              pmatchC (getField @"purpose" ctxF) >>= \case
                PSpending outRef' -> do
                  let outRef = pfield @"_0" # outRef'
                      inputs = pfield @"inputs" # (getField @"txInfo" ctxF)
                  pure $ pfindOwnInput # inputs # outRef
                _ ->
                  pure perror
           )
          @-> \res ->
            passert $ res #== pcon (PJust $ pconstant inp)
      "pgetContinuingOutputs"
        @| ( unTermCont $ do
              ctxF <- tcont $ pletFields @["txInfo", "purpose"] ctx
              pmatchC (getField @"purpose" ctxF) >>= \case
                PSpending outRef' -> do
                  let outRef = pfield @"_0" # outRef'
                      inputs = pfield @"inputs" # (getField @"txInfo" ctxF)
                      outputs = pfield @"outputs" # (getField @"txInfo" ctxF)
                  pure $ pgetContinuingOutputs # inputs # outputs # outRef
                _ ->
                  pure perror
           )
          @-> \txOuts ->
            passert $ txOuts #== pconstant validOutputs0
      "pfindDatum"
        @| ( pfindDatum # pconstant "d0" # (pfield @"datums" #$ pfield @"txInfo" # ctx)
           )
          @-> \res ->
            passert $ res #== pcon (PJust $ pconstant d0Dat)
