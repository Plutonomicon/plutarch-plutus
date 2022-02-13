{-# LANGUAGE AllowAmbiguousTypes #-}
module Plutarch.PIsDataSpec (spec) where

import Data.Text.Encoding (encodeUtf8)

import Plutarch.Api.V1
import Plutarch.Api.V1.Tuple (pbuiltinPairFromTuple, ptupleFromBuiltin)
import Plutarch.Builtin (pforgetData, ppairDataBuiltin)
import Plutarch.Lift (PLifted)
import Plutarch.Prelude

import Plutus.V1.Ledger.Credential
import qualified PlutusTx

import Test.Syd
import Plutarch.Test
import Test.Tasty.QuickCheck (Arbitrary, property)

spec :: Spec
spec = do 
  describe "pisdata" $ do 
    propertySet @PBool "PBool"
    propertySet @PInteger "PInteger"
    propertySet @PUnit "PUnit"
    describe "ppair" $ do 
      describe "pfromData (pdata (I 1, B 0x41)) ≡ (I 1, I 2)" $ do
        let p :: Term s (PBuiltinPair (PAsData PInteger) (PAsData PByteString))
            p = ppairDataBuiltin # pconstantData @PInteger 1
                -- ByteString doesn't have a ToData instance - can't use pconstantData....
                #$ pdata
                $ pconstant $ encodeUtf8 "A"
        it "works" $ pfromData (pdata p) #@?= p
      describe "pfromData (pdata (PTxId 0x41, PScriptCredential 0x82)) ≡ (PTxId 0x41, PScriptCredential 0x82)" $ do 
        let p = ppairDataBuiltin
                  # pconstantData @PTxId "41" #$ pconstantData
                  $ ScriptCredential "82"
        it "works" $ pfromData (pdata p) #@?= p
      describe "ptuple isomorphism" $ do
        let p = ppairDataBuiltin
                  # pconstantData @PTxId "41" #$ pconstantData
                  $ ScriptCredential "82"
            tup = pdata $ ptuple # pconstantData @PTxId "41" #$ pconstantData $ ScriptCredential "82"
        it "works" $ pforgetData (pdata p) #@?= pforgetData tup
        it "works" $ pfromData (pbuiltinPairFromTuple tup) #@?= p
        it "works" $ ptupleFromBuiltin (pdata p) #@?= tup


propertySet :: 
  forall p. 
  ( PIsData p
  , PLift p
  , PlutusTx.ToData (PLifted p)
  , PlutusTx.FromData (PLifted p)
  , Eq (PLifted p)
  , Show (PLifted p)
  , Arbitrary (PLifted p)
  ) => String -> Spec 
propertySet typeName = do
  describe typeName $ do 
      specify ("x ~ " <> typeName <> ": pfromData (pdata x) ≡ x") $
        property $ pToFromEqual @p
      specify ("x ~ " <> typeName <> ": pfromData (PlutusTx.toData x) ≡ x") $
        property $ pFromDataCompat @p
      specify ("x ~ " <> typeName <> ": PlutusTx.fromData (pdata x) ≡ Just x") $
        property $ pDataCompat @p  

pToFromEqual :: 
  forall p. 
  ( PIsData p 
  , PLift p 
  ) => 
  PLifted p -> _
pToFromEqual t = pfromData (pdata $ pconstant @p t) `pshouldBe` pconstant @p t

pFromDataCompat ::
  forall p.
  ( PIsData p
  , PlutusTx.ToData (PLifted p)
  , PLift p
  , Eq (PLifted p)
  , Show (PLifted p)
  ) =>
  PLifted p -> IO ()
pFromDataCompat x = plift (pfromData $ pconstantData @p x) `shouldBe` x

pDataCompat ::
  forall p.
  ( PLift p
  , PIsData p
  , PlutusTx.FromData (PLifted p)
  , Eq (PLifted p)
  , Show (PLifted p)
  ) =>
  PLifted p -> IO ()
pDataCompat x = PlutusTx.fromData @(PLifted p) (plift $ pforgetData $ pdata $ pconstant @p x) `shouldBe` Just x
