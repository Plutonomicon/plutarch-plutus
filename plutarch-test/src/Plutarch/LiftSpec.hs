module Plutarch.LiftSpec (spec) where

import Test.Syd

import Plutarch.Builtin
import Plutarch.Internal.Other (popaque)
import Plutarch.Lift (PLift, PLifted, pconstant)
import Plutarch.Test

import Plutus.V1.Ledger.Api
import qualified PlutusTx

spec :: Spec
spec = do
  describe "lift" $ do
    describe "pconstantData" $ do
      let p1 = False
          p2 = 42 :: Integer
          p3 = PubKeyHash "04"
          p4 = Minting ""
          p5 = TxOutRef "41" 12
      goldens
        All
        [ ("bool", popaque $ pconstantData p1)
        , ("int", popaque $ pconstantData p2)
        , ("pkh", popaque $ pconstantData p3)
        , ("minting", popaque $ pconstantData p4)
        , ("txoutref", popaque $ pconstantData p5)
        ]
      it "works" $ testPConstantDataSan False

testPConstantDataSan :: forall p. (PIsData p, PLift p, PlutusTx.ToData (PLifted p)) => PLifted p -> Expectation
testPConstantDataSan x =
  pconstantData @p x `pshouldBe` pdata (pconstant @p x)
