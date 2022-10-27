module Plutarch.LiftSpec (spec) where

import Data.Text (Text)
import Plutarch.Api.V1 ()
import PlutusLedgerApi.V1 (PubKeyHash (PubKeyHash), ScriptPurpose (Minting), TxOutRef (TxOutRef))
import PlutusTx qualified

import Plutarch.Lift (PLifted)
import Plutarch.Prelude
import Plutarch.Test
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "lift" $ do
    describe "plift" $ do
      it "bool" $ do
        plift (pcon PTrue) `shouldBe` True
        plift (pcon PFalse) `shouldBe` False
        plift (pconstant False) `shouldBe` False
        plift (pconstant True) `shouldBe` True
      it "list" $ do
        plift (pconstant ([1, 2, 3] :: [Integer])) `shouldBe` [1, 2, 3]
        plift (pconstant ("IOHK" :: Text, 42 :: Integer)) `shouldBe` ("IOHK", 42)
      it "nested" $ do
        -- List of pairs
        let v1 = [("IOHK", 42), ("Plutus", 31)] :: [(Text, Integer)]
        plift (pconstant v1) `shouldBe` v1
        -- List of pair of lists
        let v2 = [("IOHK", [1, 2, 3]), ("Plutus", [9, 8, 7])] :: [(Text, [Integer])]
        plift (pconstant v2) `shouldBe` v2
      it "data" $ do
        let d :: PlutusTx.Data
            d = PlutusTx.toData @(Either Bool Bool) $ Right False
        plift (pconstant d) `shouldBe` d
    describe "pconstant" $ do
      it "string" $ do
        pconstant @PString "abc" `pshouldBe` pconstant @PString "abc"
        pconstant @PString "foo" `pshouldBe` ("foo" :: Term _ PString)
    describe "pconstantData" $ do
      pgoldenSpec $ do
        "bool" @| pconstantData False
        "int" @| pconstantData (42 :: Integer)
        "pkh" @| pconstantData (PubKeyHash "04")
        "minting" @| pconstantData (Minting "")
        "txoutref" @| pconstantData (TxOutRef "41" 12)
      it "works" $ testPConstantDataSan False

testPConstantDataSan :: forall p. (PIsData p, PLift p, PlutusTx.ToData (PLifted p)) => PLifted p -> Expectation
testPConstantDataSan x =
  pconstantData @p x `pshouldBe` pdata (pconstant @p x)
