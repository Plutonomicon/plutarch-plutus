{-# LANGUAGE OverloadedRecordDot #-}
module Examples.Api (tests) where

import Plutarch
import Plutarch.Api.V1 (
  PScriptContext (..),
  PTxInInfo (..),
  PTxInfo (..),
  PValue (..),
  PScriptContext (..),
  PCredential (..)
 )
import Plutarch.Builtin 
  (PAsData, PBuiltinList, PIsData (..), PData, pforgetData, pasConstr, psndBuiltin)
import Plutarch.Field (pfield)
import Plutarch.Lift (pconstant, plift)
import Plutarch.List (phead, pmap)
import Plutus.V1.Ledger.Api (
  Address (..),
  Credential (..),
  CurrencySymbol,
  DatumHash,
  ScriptContext (..),
  ScriptPurpose (..),
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  TxOutRef (..),
  ValidatorHash,
  Value,
  toData
 )
import qualified Plutus.V1.Ledger.Interval as Interval
import qualified Plutus.V1.Ledger.Value as Value
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Utils

--------------------------------------------------------------------------------

{- |
  An example 'PScriptContext' Term,
  lifted with 'pconstant'
-}
ctx :: Term s PScriptContext
ctx =
  pconstant
    (ScriptContext info purpose)

-- | Simple script context, with minting and a single input
info :: TxInfo
info =
  TxInfo
    { txInfoInputs = [inp]
    , txInfoOutputs = []
    , txInfoFee = mempty
    , txInfoMint = mint
    , txInfoDCert = []
    , txInfoWdrl = []
    , txInfoValidRange = Interval.always
    , txInfoSignatories = []
    , txInfoData = []
    , txInfoId = "b0"
    }

-- | A script input
inp :: TxInInfo
inp =
  TxInInfo
    { txInInfoOutRef = ref
    , txInInfoResolved =
        TxOut
          { txOutAddress =
              Address (ScriptCredential validator) Nothing
          , txOutValue = mempty
          , txOutDatumHash = Just datum
          }
    }

-- | Minting a single token
mint :: Value
mint = Value.singleton sym "sometoken" 1

ref :: TxOutRef
ref = TxOutRef "a0" 0

purpose :: ScriptPurpose
purpose = Spending ref

validator :: ValidatorHash
validator = "a1"

datum :: DatumHash
datum = "d0"

sym :: CurrencySymbol
sym = "c0"

--------------------------------------------------------------------------------

getTxInfo :: Term s (PScriptContext :--> PAsData PTxInfo)
getTxInfo =
  plam $ \ctx -> 
    pfield @"txInfo" # ctx

getMint :: Term s (PAsData PTxInfo :--> PAsData PValue)
getMint =
 plam $ \info ->
   pfield @"mint" # info

-- | Get validator from first input in ScriptContext's TxInfo
getCredentials :: Term s PScriptContext -> Term s (PBuiltinList PData)
getCredentials ctx = 
  let inp = pfield @"inputs" #$ pfield @"txInfo" # ctx in
  pmap # inputCredentialHash # pfromData inp

{- | 
  Get the hash of the Credential in an input, treating 
  PubKey & ValidatorHash identically.
-}
inputCredentialHash :: Term s (PAsData PTxInInfo :--> PData)
inputCredentialHash =
  phoistAcyclic $ plam $ \inp ->
    --pfield @"resolved" # inp
    let 
      credential :: Term _ (PAsData PCredential)
      credential = 
        (pfield @"credential")
          #$ (pfield @"address") 
          #$ (pfield @"resolved" # inp)
    in 
      phead #$ psndBuiltin #$ pasConstr # pforgetData credential
      
---- | Get first CurrencySymbol from Value
-- getSym :: Term s (PValue :--> PAsData PCurrencySymbol)
-- getSym =
--  plam $ \v -> pfstBuiltin #$ phead #$ v

tests :: HasTester => TestTree
tests =
  testGroup
    "Api examples"
    [ testCase "ScriptContext" $ do
        ctx `equal'` ctx_compiled
    , testCase "getting txInfo" $ do
        plift (pfromData $ getTxInfo # ctx)
          @?= info
    , testCase "getting mint" $ do
        plift (pforgetData $ getMint #$ getTxInfo # ctx)
          @?= toData mint
    , testCase "getting credentials" $ do
        plift (getCredentials ctx)
          @?= [toData validator]
    --, testCase "getting sym" $ do
    --   plift (getSym #$ pfromData $ getMint #$ pfromData $ getTxInfo # ctx)
    --    @?= sym
    ]

ctx_compiled :: String
ctx_compiled = "(program 1.0.0 #d8799fd8799f9fd8799fd8799fd8799f41a0ff00ffd8799fd8799fd87a9f41a1ffd87a80ffa0d8799f41d0ffffffff80a0a141c0a149736f6d65746f6b656e018080d8799fd8799fd87980d87a80ffd8799fd87b80d87a80ffff8080d8799f41b0ffffd87a9fd8799fd8799f41a0ff00ffffff)"
