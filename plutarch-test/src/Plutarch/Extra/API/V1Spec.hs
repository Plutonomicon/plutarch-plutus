module Plutarch.Extra.API.V1Spec (spec) where

import Plutarch.Prelude

import Plutarch.Api.V1 (
  PScriptContext (..),
 )

import Plutarch.Extra.API.V1 (
  PAssetClass (..),
  assetClassValue,
  assetClassValueOf,
  convertBackValue,
  convertValue,
  findDatum,
  findOwnInput,
  getContinuingOutputs,
  valSub,
  valueLTE,
 )
import Plutus.V1.Ledger.Api (
  Address (..),
  Credential (ScriptCredential),
  CurrencySymbol,
  Datum (..),
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
  toBuiltinData,
 )
import qualified Plutus.V1.Ledger.Interval as Interval
import Plutus.V1.Ledger.Value (singleton)

import Plutarch.Test

import Test.Syd

spec :: Spec
spec = describe "api" $
  plutarchDevFlagDescribe $
    pgoldenSpec $ do
      "<=" @\ do
        "1<=2" @| valueLTE # pconstant v1 # pconstant v2 @-> passert
        "!(2<=1)" @| valueLTE # pconstant v2 # pconstant v1 @-> passertNot
        "1" @| valueLTE # pconstant v3 # pconstant v4 @-> passertNot
        "2" @| valueLTE # pconstant v4 # pconstant v3 @-> passertNot
      "round trip tests" @\ do
        let valueRoundTrips v =
              pdata (convertBackValue #$ convertValue # pconstant v) @== pdata (pconstant v)
        "1" @| valueRoundTrips v1
        "2" @| valueRoundTrips v2
        "3" @| valueRoundTrips v3
        "4" @| valueRoundTrips v4
      "assetClassValue" @\ do
        "assetClassValue" @| pdata (assetClassValue # someToken # 1) @== pdata (pconstant v1)
      "assetClassValueOf" @\ do
        "1" @| assetClassValueOf # pconstant v1 # someToken #== 1 @-> passert
        "2" @| assetClassValueOf # pconstant v2 # someToken #== 2 @-> passert
        "3" @| assetClassValueOf # pconstant v3 # token1 #== 1 @-> passert
        "4" @| assetClassValueOf # pconstant v1 # token1 #== 0 @-> passert
      "valSub" @\ do
        "1" @| valSub # pconstant v2 # pconstant v1 #== pconstant v1 @-> passert
        "2" @| valSub # pconstant v2 # pconstant v1 #== pconstant v1 @-> passert
        "3" @| valSub # pconstant v5 # pconstant v4 #== pconstant v3 @-> passert
        "4" @| valSub # pconstant v5 # pconstant v3 #== pconstant v4 @-> passert
        "5" @| valSub # pconstant v1 # pconstant v2 #== pconstant negv1 @-> passert
        "6" @| valSub # pconstant v3 # pconstant v2 #== pconstant v3mv2 @-> passert
      "value monoid" @\ do
        "1" @| pconstant (v4 <> v5) #== pconstant v4 <> pconstant v5 @-> passert
        "2" @| pconstant (v1 <> v5) #== pconstant v1 <> pconstant v5 @-> passert
        "3" @| pconstant (v3 <> v5) #== pconstant v3 <> pconstant v5 @-> passert
        "4" @| pconstant (v2 <> v3) #== pconstant v3 <> pconstant v2 @-> passert
        "5" @| pconstant (v2 <> v3) #== pconstant v2 <> pconstant v3 @-> passert
        "7" @| pconstant (v3 <> v2) #== pconstant v3 <> pconstant v2 @-> passert
        "8" @| pconstant (v3 <> v2) #== pconstant v2 <> pconstant v3 @-> passert
      "<> inverts valSub" @\ do
        "1" @| pconstant v3 #== (valSub # pconstant v3 # pconstant v2) <> pconstant v2 @-> passert
        "2" @| pconstant v3 #== valSub # (pconstant v3 <> pconstant v2) # pconstant v2 @-> passert
        "3" @| pconstant v3 #== (valSub # pconstant v3 # pconstant v4) <> pconstant v4 @-> passert
        "4" @| pconstant v4 #== (valSub # pconstant v4 # pconstant v5) <> pconstant v5 @-> passert
        "5" @| pconstant v4mv5 #== valSub # pconstant v4 # pconstant v5 @-> passert
        "6" @| pconstant v3mv2 #== valSub # pconstant v3 # pconstant v2 @-> passert
      "getCotiuingOutputs" @\ do
        "1" @| getContinuingOutputs # ctx #== psingleton # pconstant out1 @-> passert
        "2" @| getContinuingOutputs # ctx2' [inp2, inp1] #== psingleton # pconstant out2 @-> passert
        "3" @| getContinuingOutputs # ctx2' [inp1, inp2] #== psingleton # pconstant out2 @-> passert
        "4" @| getContinuingOutputs # ctx2' [inp2] #== psingleton # pconstant out2 @-> passert
      "findDatum" @\ do
        "1" @| findDatum # pconstant datumhash1 # pconstant info #== pcon (PJust $ pconstant datum1) @-> passert
        "2" @| findDatum # pconstant datumhash2 # pconstant info #== pcon (PJust $ pconstant datum2) @-> passert
        "3" @| findDatum # pconstant datumhash3 # pconstant info #== pcon PNothing @-> passert
      "findOwnInput" @\ do
        "1" @| findOwnInput # ctx #== pcon (PJust $ pconstant inp1) @-> passert
        "2" @| findOwnInput # ctx2' [inp2] #== pcon (PJust $ pconstant inp2) @-> passert

-- TODO unit tests for mustPayToPubKey

someToken :: Term s PAssetClass
someToken =
  pcon $
    PAssetClass $
      pdcons
        # pdata (pconstant sym) #$ pdcons
        # pdata (pconstant "sometoken") #$ pdnil

token1 :: Term s PAssetClass
token1 =
  pcon $
    PAssetClass $
      pdcons
        # pdata (pconstant sym) #$ pdcons
        # pdata (pconstant "token1") #$ pdnil

v1 :: Value
v1 = singleton sym "sometoken" 1

negv1 :: Value
negv1 = singleton sym "sometoken" (-1)

v2 :: Value
v2 = singleton sym "sometoken" 2

v3 :: Value
v3 = singleton sym "token1" 1 <> singleton sym "token2" 1

v3mv2 :: Value
v3mv2 = singleton sym "token1" 1 <> singleton sym "token2" 1 <> singleton sym "sometoken" (-2)

v4 :: Value
v4 = singleton sym "token1" 2

v5 :: Value
v5 = singleton sym "token1" 3 <> singleton sym "token2" 1

v4mv5 :: Value
v4mv5 = singleton sym "token1" (-1) <> singleton sym "token2" (-1)

sym :: CurrencySymbol
sym = "c0"

ctx :: Term s PScriptContext
ctx =
  pconstant
    (ScriptContext info purpose1)

ctx2' :: [TxInInfo] -> Term s PScriptContext
ctx2' is =
  pconstant
    (ScriptContext (info' is) purpose2)

info :: TxInfo
info =
  TxInfo
    { txInfoInputs = [inp1]
    , txInfoOutputs = [out1, out2]
    , txInfoFee = mempty
    , txInfoMint = mint
    , txInfoDCert = []
    , txInfoWdrl = []
    , txInfoValidRange = Interval.always
    , txInfoSignatories = signatories
    , txInfoData = [(datumhash1, datum1), (datumhash2, datum2)]
    , txInfoId = "b0"
    }

info' :: [TxInInfo] -> TxInfo
info' is = info {txInfoInputs = is}

ref1 :: TxOutRef
ref1 = TxOutRef "a1" 0

ref2 :: TxOutRef
ref2 = TxOutRef "a2" 0

out1 :: TxOut
out1 = TxOut (Address (ScriptCredential validator1) Nothing) v1 Nothing

out2 :: TxOut
out2 = TxOut (Address (ScriptCredential validator2) Nothing) v2 Nothing

purpose1 :: ScriptPurpose
purpose1 = Spending ref1

purpose2 :: ScriptPurpose
purpose2 = Spending ref2

inp1 :: TxInInfo
inp1 =
  TxInInfo
    { txInInfoOutRef = ref1
    , txInInfoResolved =
        TxOut
          { txOutAddress =
              Address (ScriptCredential validator1) Nothing
          , txOutValue = mempty
          , txOutDatumHash = Just datumhash1
          }
    }

inp2 :: TxInInfo
inp2 =
  TxInInfo
    { txInInfoOutRef = ref2
    , txInInfoResolved =
        TxOut
          { txOutAddress =
              Address (ScriptCredential validator2) Nothing
          , txOutValue = mempty
          , txOutDatumHash = Just datumhash2
          }
    }

mint :: Value
mint = singleton sym "sometoken" 1

signatories :: [PubKeyHash]
signatories = ["ab01fe235c", "123014", "abcdef"]

validator1 :: ValidatorHash
validator1 = "a1"

validator2 :: ValidatorHash
validator2 = "a2"

datum1 :: Datum
datum1 = Datum $ toBuiltinData ([1] :: [Integer])

datumhash1 :: DatumHash
datumhash1 = "d1"

datum2 :: Datum
datum2 = Datum $ toBuiltinData ([2] :: [Integer])

datumhash2 :: DatumHash
datumhash2 = "d2"

datumhash3 :: DatumHash
datumhash3 = "d3"
