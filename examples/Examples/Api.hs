module Examples.Api (tests) where

import Data.Proxy (Proxy (..))
import Plutarch
import Plutarch.Api.V1 (
  PScriptContext (..),
  PTxInInfo (..),
  PTxInfo (..),
  PValue (..),
  PScriptContext (..),
  PAddress (..),
  PTxOut (..),
  PValidatorHash (..),
  PCredential (..)
 )
import Plutarch.Builtin (PAsData, PBuiltinList, PIsData (..))
import Plutarch.DataRepr (pindexDataList)
import Plutarch.Lift (pconstant)
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
 )
import qualified Plutus.V1.Ledger.Interval as Interval
import qualified Plutus.V1.Ledger.Value as Value
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

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

_getTxInfo :: Term s (PScriptContext :--> PAsData PTxInfo)
_getTxInfo =
  plam $ \x -> pmatch x $ \case
    (PScriptContext c) -> pindexDataList (Proxy @0) # c

_getMint :: Term s (PTxInfo :--> PAsData PValue)
_getMint =
  plam $ \x -> pmatch x $ \case
    (PTxInfo i) -> pindexDataList (Proxy @3) # i

_getInputs :: Term s (PTxInfo :--> PAsData (PBuiltinList (PAsData PTxInInfo)))
_getInputs =
  plam $ \x -> pmatch x $ \case
    (PTxInfo i) -> pindexDataList (Proxy @0) # i

-- | Get first validator from TxInInfo
getValidator :: Term s (PBuiltinList (PAsData PTxInInfo) :--> PAsData PValidatorHash)
getValidator =
  plam $ \xs ->
    pmatch (pfromData $ phead # xs) $ \case
      (PTxInInfo i) -> pmatch (pfromData $ pindexDataList (Proxy @1) # i) $ \case
        (PTxOut o) -> pmatch (pfromData $ pindexDataList (Proxy @0) # o) $ \case
          (PAddress a) -> pmatch (pfromData $ pindexDataList (Proxy @0) # a) $ \case
            (PPubKeyCredential _) -> perror
            (PScriptCredential v) -> pindexDataList (Proxy @0) # v

---- | Get first CurrencySymbol from Value
--getSym :: Term s (PValue :--> PAsData PCurrencySymbol)
--getSym =
--  plam $ \v -> pfstBuiltin #$ phead #$ v

tests :: HasTester => TestTree
tests =
  testGroup
    "Api examples"
    [ testCase "ScriptContext" $ do
        ctx `equal'` ctx_compiled
        -- FIXME
        --, testCase "getting txInfo" $ do
        --    plift (getTxInfo # ctx) @?= info
        --, testCase "getting mint" $ do
        --    plift (getMint #$ pfromData $ getTxInfo # ctx) @?= mint
        --, testCase "getting validator" $ do
        --    plift (getValidator #$ pfromData $ getInputs #$ pfromData $ getTxInfo # ctx)
        --      @?= validator
        --, testCase "getting sym" $ do
        --    plift (getSym #$ pfromData $ getMint #$ pfromData $ getTxInfo # ctx)
        --      @?= sym
    ]

ctx_compiled :: String
ctx_compiled = "(program 1.0.0 #d8799fd8799f9fd8799fd8799fd8799f41a0ff00ffd8799fd8799fd87a9f41a1ffd87a80ffa0d8799f41d0ffffffff80a0a141c0a149736f6d65746f6b656e018080d8799fd8799fd87980d87a80ffd8799fd87b80d87a80ffff8080d8799f41b0ffffd87a9fd8799fd8799f41a0ff00ffffff)"
