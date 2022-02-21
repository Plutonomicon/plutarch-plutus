{-# LANGUAGE AllowAmbiguousTypes #-}

module Plutarch.PIsDataSpec (spec) where

import Data.Text.Encoding (encodeUtf8)

import Plutarch.Api.V1
import Plutarch.Api.V1.Tuple (pbuiltinPairFromTuple, ptupleFromBuiltin)
import Plutarch.Builtin (pforgetData, ppairDataBuiltin)
import Plutarch.Lift (PLifted)
import Plutarch.Prelude

import Plutus.V1.Ledger.Credential (Credential (ScriptCredential))
import qualified PlutusTx

import Plutarch.Test
import Test.Syd
import Test.Tasty.QuickCheck (Arbitrary, property)

spec :: Spec
spec = do
  describe "pisdata" $ do
    propertySet @PBool "PBool"
    propertySet @PInteger "PInteger"
    propertySet @PUnit "PUnit"
    describe "ppair" . pgoldenSpec $ do
      -- pfromData (pdata (I 1, B 0x41)) ≡ (I 1, I A)
      "simple"
        @| ( ppairDataBuiltin @_ @PInteger @PByteString
              # pconstantData @PInteger 1
              #$ pdata (pconstant $ encodeUtf8 "A")
           )
          @-> \p ->
            pfromData (pdata p) `pshouldBe` p
      -- pfromdata (pdata (ptxid 0x41, pscriptcredential 0x82)) ≡ (ptxid 0x41, pscriptcredential 0x82)
      let scPair =
            ppairDataBuiltin
              # pconstantData @PTxId "41"
              #$ pconstantData (ScriptCredential "82")
      "scriptcredential" @| scPair @-> \p ->
        pfromData (pdata p) `pshouldBe` p
      let scTuple = pdata $ ptuple # pconstantData @PTxId "41" #$ pconstantData $ ScriptCredential "82"
      "isomorphism" @\ do
        "pforgetData" @| pforgetData (pdata scPair) @== pforgetData scTuple
        "pbuiltinPairFromTuple" @| pfromData (pbuiltinPairFromTuple scTuple) @== scPair
        "ptupleFromBuiltin" @| ptupleFromBuiltin (pdata scPair) @== scTuple

propertySet ::
  forall p.
  ( PIsData p
  , PLift p
  , PlutusTx.ToData (PLifted p)
  , PlutusTx.FromData (PLifted p)
  , Eq (PLifted p)
  , Show (PLifted p)
  , Arbitrary (PLifted p)
  ) =>
  String ->
  Spec
propertySet typeName = do
  describe typeName $ do
    specify ("x ~ " <> typeName <> ": pfromData (pdata x) ≡ x") $
      property $ ptoFromEqual @p
    specify ("x ~ " <> typeName <> ": pfromData (PlutusTx.toData x) ≡ x") $
      property $ pfromDataCompat @p
    specify ("x ~ " <> typeName <> ": PlutusTx.fromData (pdata x) ≡ Just x") $
      property $ pdataCompat @p

ptoFromEqual ::
  forall p.
  ( PIsData p
  , PLift p
  ) =>
  PLifted p ->
  _
ptoFromEqual t = pfromData (pdata $ pconstant @p t) `pshouldBe` pconstant @p t

pfromDataCompat ::
  forall p.
  ( PIsData p
  , PlutusTx.ToData (PLifted p)
  , PLift p
  , Eq (PLifted p)
  , Show (PLifted p)
  ) =>
  PLifted p ->
  IO ()
pfromDataCompat x = plift (pfromData $ pconstantData @p x) `shouldBe` x

pdataCompat ::
  forall p.
  ( PLift p
  , PIsData p
  , PlutusTx.FromData (PLifted p)
  , Eq (PLifted p)
  , Show (PLifted p)
  ) =>
  PLifted p ->
  IO ()
pdataCompat x = PlutusTx.fromData @(PLifted p) (plift $ pforgetData $ pdata $ pconstant @p x) `shouldBe` Just x
