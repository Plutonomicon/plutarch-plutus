module Examples.Api (tests) where

import Control.Monad.Trans.Cont (cont, runCont)
import Plutarch
import Plutarch.Api.V1 (
  PAddress (PAddress),
  PCredential,
  PCurrencySymbol,
  PMaybeData,
  PPubKeyHash,
  PScriptContext,
  PScriptPurpose (PSpending),
  PStakingCredential,
  PTxInInfo,
  PTxInfo,
  PValue,
 )
import Plutarch.Builtin (pasConstr, pforgetData)
import Plutarch.DataRepr (PLabeledType ((:=)))

-- import Plutarch.DataRepr (pindexDataList)
import qualified Plutarch.Monadic as P

import Plutus.V1.Ledger.Api (
  Address (Address),
  Credential (ScriptCredential),
  CurrencySymbol,
  DatumHash,
  PubKeyHash,
  ScriptContext (ScriptContext),
  ScriptPurpose (Spending),
  TxInInfo (TxInInfo, txInInfoOutRef, txInInfoResolved),
  TxInfo (
    TxInfo,
    txInfoDCert,
    txInfoData,
    txInfoFee,
    txInfoId,
    txInfoInputs,
    txInfoMint,
    txInfoOutputs,
    txInfoSignatories,
    txInfoValidRange,
    txInfoWdrl
  ),
  TxOut (TxOut, txOutAddress, txOutDatumHash, txOutValue),
  TxOutRef (TxOutRef),
  ValidatorHash,
  Value,
  toData,
 )

import Plutarch.Prelude
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
  let inp = pfield @"inputs" #$ pfield @"txInfo" # ctx
   in pmap # inputCredentialHash # pfromData inp

{- |
  Get the hash of the Credential in an input, treating
  PubKey & ValidatorHash identically.
-}
inputCredentialHash :: Term s (PAsData PTxInInfo :--> PData)
inputCredentialHash =
  phoistAcyclic $
    plam $ \inp ->
      let credential :: Term _ (PAsData PCredential)
          credential =
            (pfield @"credential")
              #$ (pfield @"address")
              #$ (pfield @"resolved" # inp)
       in phead #$ psndBuiltin #$ pasConstr # pforgetData credential

-- | Get first CurrencySymbol from Value
getSym :: Term s (PValue :--> PAsData PCurrencySymbol)
getSym =
  plam $ \v -> pfstBuiltin #$ phead # pto (pto v)

checkSignatory :: Term s (PPubKeyHash :--> PScriptContext :--> PUnit)
checkSignatory = plam $ \ph ctx' ->
  pletFields @["txInfo", "purpose"] ctx' $ \ctx -> P.do
    PSpending _ <- pmatch . pfromData $ ctx.purpose
    let signatories = pfield @"signatories" # ctx.txInfo
    pif
      (pelem # pdata ph # pfromData signatories)
      -- Success!
      (pconstant ())
      -- Signature not present.
      perror

-- | `checkSignatory` implemented using `runCont`
checkSignatoryCont :: Term s (PPubKeyHash :--> PScriptContext :--> PUnit)
checkSignatoryCont = plam $ \ph ctx' ->
  pletFields @["txInfo", "purpose"] ctx' $ \ctx -> (`runCont` id) $ do
    purpose <- cont (pmatch . pfromData $ ctx.purpose)
    pure $ case purpose of
      PSpending _ ->
        let signatories = pfield @"signatories" # ctx.txInfo
         in pif
              (pelem # pdata ph # pfromData signatories)
              -- Success!
              (pconstant ())
              -- Signature not present.
              perror
      _ ->
        ptraceError "checkSignatoryCont: not a spending tx"

-- | `checkSignatory` implemented using `runTermCont`
checkSignatoryTermCont :: Term s (PPubKeyHash :--> PScriptContext :--> PUnit)
checkSignatoryTermCont = plam $ \ph ctx' -> runTermContId $ do
  ctx <- tcont $ pletFields @["txInfo", "purpose"] ctx'
  PSpending _ <- tcont (pmatch . pfromData $ ctx.purpose)
  let signatories = pfield @"signatories" # ctx.txInfo
  pure $
    pif
      (pelem # pdata ph # pfromData signatories)
      -- Success!
      (pconstant ())
      -- Signature not present.
      perror

getFields :: Term s (PData :--> PBuiltinList PData)
getFields = phoistAcyclic $ plam $ \addr -> psndBuiltin #$ pasConstr # addr

getFields' :: Term s (PAddress :--> PDataRecord '["credential" ':= PCredential, "stakingCredential" ':= PMaybeData PStakingCredential])
getFields' = phoistAcyclic $
  plam $ \addr -> P.do
    PAddress addrFields <- pmatch addr
    addrFields

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
    , testCase "getting sym" $ do
        plift (pfromData $ getSym #$ pfromData $ getMint #$ getTxInfo # ctx) @?= sym
    , testCase "signatory validator" $ do
        () <$ traverse (\x -> succeeds $ checkSignatory # pconstant x # ctx) signatories
        fails $ checkSignatory # pconstant "41" # ctx
    , testCase "signatory validator with Cont" $ do
        () <$ traverse (\x -> succeeds $ checkSignatoryCont # pconstant x # ctx) signatories
        fails $ checkSignatory # pconstant "41" # ctx
    , testCase "signatory validator with TermCont" $ do
        () <$ traverse (\x -> succeeds $ checkSignatoryTermCont # pconstant x # ctx) signatories
        fails $ checkSignatory # pconstant "41" # ctx
    , testCase "getFields" $
        printTerm getFields @?= getFields_compiled
    , testCase "getFields'" $
        printTerm getFields' @?= getFields_compiled
    ]

ctx_compiled :: String
ctx_compiled = "(program 1.0.0 #d8799fd8799f9fd8799fd8799fd8799f41a0ff00ffd8799fd8799fd87a9f41a1ffd87a80ffa0d8799f41d0ffffffff80a0a141c0a149736f6d65746f6b656e018080d8799fd8799fd87980d87a80ffd8799fd87b80d87a80ffff9f45ab01fe235c4312301443abcdefff80d8799f41b0ffffd87a9fd8799fd8799f41a0ff00ffffff)"

getFields_compiled :: String
getFields_compiled = "(program 1.0.0 (\\i0 -> force (force sndPair) (unConstrData i1)))"
