{-# LANGUAGE AllowAmbiguousTypes #-}

module Examples.PIsData (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Utils

import Plutarch.Builtin (pforgetData)
import Plutarch.Lift (PLifted)
import Plutarch.Prelude

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
    [pBool, pInteger]

pBool :: HasTester => TestTree
pBool =
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

pInteger :: HasTester => TestTree
pInteger =
  testGroup
    "PInteger"
    [ testCase "pfromData (pdata 100) ≡ 100" $
        testPToFrom @PInteger 100
    , testCase "pfromData (PlutusTx.toData 100) ≡ 100" $
        testPFromDataCompat @PInteger 100
    , testCase "PlutusTx.fromData (pdata 100) ≡ Just 100" $
        testPFromDataCompat @PInteger 100
    ]
