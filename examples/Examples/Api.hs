{-# LANGUAGE QualifiedDo #-}

module Examples.Api (tests) where

import Data.Proxy (Proxy (..))
import Plutarch
import Plutarch.Api.V1 (
  PAddress (PAddress),
  PCredential (PScriptCredential),
  PCurrencySymbol,
  PPubKeyHash,
  PScriptContext (PScriptContext),
  PScriptPurpose (PSpending),
  PTxInInfo (PTxInInfo),
  PTxInfo (PTxInfo),
  PTxOut (PTxOut),
  PValidatorHash,
  PValue,
 )
import Plutarch.Bool (pif)
import Plutarch.Builtin (PAsData, PBuiltinList, pdata, pfromData, pfstBuiltin)
import Plutarch.DataRepr (pindexDataList)
import Plutarch.Lift (pconstant, plift)
import Plutarch.List (pelem, phead)
import qualified Plutarch.Monadic as P
import Plutarch.Trace (ptrace)
import Plutarch.Unit (PUnit)

import Plutus.V1.Ledger.Api (
  Address (..),
  Credential (..),
  CurrencySymbol,
  DatumHash,
  PubKeyHash,
  ScriptContext (..),
  ScriptPurpose (..),
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  TxOutRef (..),
  ValidatorHash,
  Value,
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
    , txInfoSignatories = signatories
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

signatories :: [PubKeyHash]
signatories = ["ab01fe235c", "123014", "abcdef"]

--------------------------------------------------------------------------------

getTxInfo :: Term s (PScriptContext :--> PAsData PTxInfo)
getTxInfo = plam $ \x -> P.do
  PScriptContext c <- pmatch x
  pindexDataList (Proxy @0) # c

getMint :: Term s (PTxInfo :--> PAsData PValue)
getMint = plam $ \x -> P.do
  PTxInfo i <- pmatch x
  pindexDataList (Proxy @3) # i

getInputs :: Term s (PTxInfo :--> PAsData (PBuiltinList (PAsData PTxInInfo)))
getInputs = plam $ \x -> P.do
  PTxInfo i <- pmatch x
  ptrace "xhuawdhauywhd"
  i' <- plet i
  pindexDataList (Proxy @0) # i'

-- | Get first validator from TxInInfo
getValidator :: Term s (PBuiltinList (PAsData PTxInInfo) :--> PAsData PValidatorHash)
getValidator =
  plam $ \xs -> P.do
    PTxInInfo i <- pmatch . pfromData $ phead # xs
    PTxOut o <- pmatch . pfromData $ pindexDataList (Proxy @1) # i
    PAddress a <- pmatch . pfromData $ pindexDataList (Proxy @0) # o
    PScriptCredential v <- pmatch (pfromData $ pindexDataList (Proxy @0) # a)
    pindexDataList (Proxy @0) # v

-- | Get first CurrencySymbol from Value
getSym :: Term s (PValue :--> PAsData PCurrencySymbol)
getSym =
  plam $ \v -> pfstBuiltin #$ phead # pto (pto v)

checkSignatory :: Term s (PPubKeyHash :--> PScriptContext :--> PUnit)
checkSignatory = plam $ \ph ctx -> P.do
  PScriptContext ctxFields <- pmatch ctx
  PSpending _ <- pmatch . pfromData $ pindexDataList (Proxy @1) # ctxFields
  PTxInfo txInfoFields <- pmatch . pfromData $ pindexDataList (Proxy @0) # ctxFields
  let signatories = pindexDataList (Proxy @7) # txInfoFields
  pif
    (pelem # pdata ph # pfromData signatories)
    -- Success!
    (pconstant ())
    -- Signature not present.
    perror

tests :: HasTester => TestTree
tests =
  testGroup
    "Api examples"
    [ testCase "ScriptContext" $ do
        ctx `equal'` ctx_compiled
    , testCase "getting txInfo" $ do
        plift (pfromData $ getTxInfo # ctx) @?= info
    , -- FIXME: Need 'PConstant' etc. instance for 'PValue'
      -- , testCase "getting mint" $ do
      --     plift (pfromData $ getMint #$ pfromData $ getTxInfo # ctx) @?= mint
      testCase "getting validator" $ do
        plift (pfromData $ getValidator #$ pfromData $ getInputs #$ pfromData $ getTxInfo # ctx)
          @?= validator
    , testCase "getting sym" $ do
        plift (pfromData $ getSym #$ pfromData $ getMint #$ pfromData $ getTxInfo # ctx) @?= sym
    , testCase "signatory validator" $ do
        () <$ traverse (\x -> succeeds $ checkSignatory # pconstant x # ctx) signatories
        fails $ checkSignatory # pconstant "41" # ctx
    ]

ctx_compiled :: String
ctx_compiled = "(program 1.0.0 #d8799fd8799f9fd8799fd8799fd8799f41a0ff00ffd8799fd8799fd87a9f41a1ffd87a80ffa0d8799f41d0ffffffff80a0a141c0a149736f6d65746f6b656e018080d8799fd8799fd87980d87a80ffd8799fd87b80d87a80ffff9f45ab01fe235c4312301443abcdefff80d8799f41b0ffffd87a9fd8799fd8799f41a0ff00ffffff)"
