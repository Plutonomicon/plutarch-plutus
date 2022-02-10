{-# LANGUAGE AllowAmbiguousTypes #-}

module Examples.PIsData (tests) where

import Data.Text.Encoding (encodeUtf8)

import Test.Tasty
import Test.Tasty.HUnit

import Utils

import Plutarch.Api.V1
import Plutarch.Api.V1.Tuple (pbuiltinPairFromTuple, ptupleFromBuiltin)
import Plutarch.Builtin (pforgetData, ppairDataBuiltin)
import Plutarch.Lift (PLifted)
import Plutarch.Prelude

import Plutus.V1.Ledger.Credential
import qualified PlutusTx

--------------------------------------------------------------------------------

testPToFrom ::
  forall p.
  ( PIsData p
  , PLift p
  , HasTester
  ) =>
  PLifted p ->
  Assertion
testPToFrom t =
  pfromData (pdata $ pconstant @p t)
    `equal` pconstant @p t

testPFromDataCompat ::
  forall p.
  ( PIsData p
  , PlutusTx.ToData (PLifted p)
  , PLift p
  , Eq (PLifted p)
  , Show (PLifted p)
  ) =>
  PLifted p ->
  Assertion
testPFromDataCompat x =
  plift (pfromData $ pconstantData @p x)
    @?= x

testPDataCompat ::
  forall p.
  ( PLift p
  , PIsData p
  , PlutusTx.FromData (PLifted p)
  , Eq (PLifted p)
  , Show (PLifted p)
  ) =>
  PLifted p ->
  Assertion
testPDataCompat x =
  PlutusTx.fromData @(PLifted p) (plift $ pforgetData $ pdata $ pconstant @p x)
    @?= Just x

--------------------------------------------------------------------------------
-- TODO: These shoutld probably be property tests instead...

tests :: HasTester => TestTree
tests =
  testGroup
    "Builtin PIsData instances"
    [pboolTests, pintegerTests, punitTests, ppairTests]

pboolTests :: HasTester => TestTree
pboolTests =
  testGroup
    "Builtin PIsData instances: PBool"
    [ testCase "pfromData (pdata True) ≡ True" $
        testPToFrom @PBool True
    , testCase "pfromData (pdata False) ≡ False" $
        testPToFrom @PBool False
    , testCase "pfromData (PlutusTx.toData True) ≡ True" $
        testPFromDataCompat @PBool True
    , testCase "pfromData (PlutusTx.toData False) ≡ False" $
        testPFromDataCompat @PBool False
    , testCase "PlutusTx.fromData (pdata True) ≡ Just True" $
        testPDataCompat @PBool True
    , testCase "PlutusTx.fromData (pdata False) ≡ Just False" $
        testPDataCompat @PBool False
    ]

pintegerTests :: HasTester => TestTree
pintegerTests =
  testGroup
    "PInteger"
    [ testCase "pfromData (pdata 100) ≡ 100" $
        testPToFrom @PInteger 100
    , testCase "pfromData (PlutusTx.toData 100) ≡ 100" $
        testPFromDataCompat @PInteger 100
    , testCase "PlutusTx.fromData (pdata 100) ≡ Just 100" $
        testPFromDataCompat @PInteger 100
    ]

punitTests :: HasTester => TestTree
punitTests =
  testGroup
    "Builtin PIsData instances: PUnit"
    [ testCase "pfromData (pdata ()) ≡ ()" $
        testPToFrom @PUnit ()
    , testCase "pfromData (PlutusTx.toData ()) ≡ ()" $
        testPFromDataCompat @PUnit ()
    , testCase "PlutusTx.fromData (pdata ()) ≡ Just ()" $
        testPDataCompat @PUnit ()
    ]

ppairTests :: HasTester => TestTree
ppairTests =
  testGroup
    "Builtin PIsData instances: PBuiltinPair & PTuple"
    [ testCase "pfromData (pdata (I 1, B 0x41)) ≡ (I 1, I 2)" $
        let x =
              ppairDataBuiltin # pconstantData @PInteger 1
                -- ByteString doesn't have a ToData instance - can't use pconstantData....
                #$ pdata
                $ pconstant $ encodeUtf8 "A"
         in pfromData (pdata x)
              `equal` x
    , testCase
        "pfromData (pdata (PTxId 0x41, PScriptCredential 0x82)) ≡ (PTxId 0x41, PScriptCredential 0x82)"
        $ let x =
                ppairDataBuiltin
                  # pconstantData @PTxId "41" #$ pconstantData
                  $ ScriptCredential "82"
           in pfromData (pdata x) `equal` x
    , testCase "ptuple isomorphism" $ do
        let p =
              ppairDataBuiltin
                # pconstantData @PTxId "41" #$ pconstantData
                $ ScriptCredential "82"
            tup = pdata $ ptuple # pconstantData @PTxId "41" #$ pconstantData $ ScriptCredential "82"
        pforgetData (pdata p) `equal` pforgetData tup
        pfromData (pbuiltinPairFromTuple tup) `equal` p
        ptupleFromBuiltin (pdata p) `equal` tup
    ]
