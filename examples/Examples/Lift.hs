module Examples.Lift (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

import Utils

import Plutarch (printTerm)
import Plutarch.Api.V1 ()
import Plutarch.Lift (PLifted)
import Plutarch.Prelude
import Plutus.V1.Ledger.Api
import qualified PlutusTx

testPConstantDataSan :: forall p. (HasTester, PIsData p, PLift p, PlutusTx.ToData (PLifted p)) => PLifted p -> Assertion
testPConstantDataSan x = pconstantData @p x `equal` pdata (pconstant @p x)

tests :: HasTester => TestTree
tests = testGroup "pconstant/plift tests" [pconstantDataTests]

pconstantDataTests :: HasTester => TestTree
pconstantDataTests =
  testGroup
    "pconstantData"
    [ testCase "pconstantData ≡ pdata . pconstant" $ do
        testPConstantDataSan False
        testPConstantDataSan @PInteger 42
        testPConstantDataSan $ PubKeyHash "04"
        testPConstantDataSan $ Minting ""
        testPConstantDataSan $ TxOutRef "41" 12
    , testCase "pconstantData compiled output" $ do
        printTerm (pconstantData @PInteger 42) @?= "(program 1.0.0 #182a)"
        printTerm (pconstantData True) @?= "(program 1.0.0 #d87a80)"
        printTerm (pconstantData $ PubKeyHash "04") @?= "(program 1.0.0 #423034)"
        printTerm (pconstantData $ Minting "") @?= "(program 1.0.0 #d8799f40ff)"
        printTerm (pconstantData $ TxOutRef "41" 12) @?= "(program 1.0.0 #d8799fd8799f4141ff0cff)"
    ]
