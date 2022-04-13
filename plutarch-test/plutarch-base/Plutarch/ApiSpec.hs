-- NOTE: This module also contains ScriptContext mocks, which should ideally
-- moved to a module of its own after cleaning up to expose a easy to reason
-- about API.
module Plutarch.ApiSpec (
  spec,
  ctx,
  validContext0,
  validOutputs0,
  invalidContext1,
  d0Dat,
  d0DatValue,
  inp,
) where

import Test.Tasty.HUnit

import Control.Monad.Trans.Cont (cont, runCont)
import Plutus.V1.Ledger.Api
import qualified Plutus.V1.Ledger.Interval as Interval
import qualified Plutus.V1.Ledger.Value as Value

import Plutarch.Api.V1 (
  PCredential,
  PCurrencySymbol,
  PPubKeyHash,
  PScriptContext,
  PScriptPurpose (PMinting, PSpending),
  PTxInInfo,
  PTxInfo,
  PValue,
 )
import Plutarch.Builtin (pasConstr, pforgetData)
import Plutarch.Prelude
import Plutarch.Test
import Test.Hspec

spec :: Spec
spec = do
  describe "api" $ do
    describe "ctx" $ do
      pgoldenSpec $ do
        "term" @| ctx
        "get" @\ do
          "txInfo" @| pfromData (getTxInfo # ctx) @-> \p ->
            plift p @?= info
          "mint" @| pforgetData (getMint #$ getTxInfo # ctx) @-> \p ->
            plift p @?= toData mint
          "credentials" @| getCredentials ctx @-> \p ->
            plift p @?= [toData validator]
          "sym" @| pfromData (getSym #$ pfromData $ getMint #$ getTxInfo # ctx) @-> \p ->
            plift p @?= sym
        "ScriptPurpose" @\ do
          "literal" @| pconstant @PScriptPurpose (Minting dummyCurrency)
          "decode"
            @| pmatch (pconstant @PScriptPurpose (Minting dummyCurrency))
            $ \case
              PMinting c -> popaque c
              _ -> perror
    describe "example" $ do
      -- The checkSignatory family of functions implicitly use tracing due to
      -- monadic syntax, and as such we need two sets of tests here.
      -- See Plutarch.MonadicSpec for GHC9 only syntax.
      describe "signatory" . plutarchDevFlagDescribe . pgoldenSpec $ do
        let aSig :: PubKeyHash = "ab01fe235c"
        "cont" @\ do
          "succeeds" @| checkSignatoryCont # pconstant aSig # ctx @-> psucceeds
          "fails" @| checkSignatoryCont # pconstant "41" # ctx @-> pfails
        "termcont" @\ do
          "succeeds" @| checkSignatoryTermCont # pconstant aSig # ctx @-> psucceeds
          "fails" @| checkSignatoryTermCont # pconstant "41" # ctx @-> pfails
      describe "getFields" . pgoldenSpec $ do
        "0" @| getFields

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
   in pmap # inputCredentialHash # inp

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

-- | `checkSignatory` implemented using `runCont`
checkSignatoryCont :: forall s. Term s (PPubKeyHash :--> PScriptContext :--> PUnit)
checkSignatoryCont = plam $ \ph ctx' ->
  pletFields @["txInfo", "purpose"] ctx' $ \ctx -> (`runCont` id) $ do
    purpose <- cont (pmatch $ getField @"purpose" ctx)
    pure $ case purpose of
      PSpending _ ->
        let signatories :: Term s (PBuiltinList (PAsData PPubKeyHash))
            signatories = pfield @"signatories" # getField @"txInfo" ctx
         in pif
              (pelem # pdata ph # signatories)
              -- Success!
              (pconstant ())
              -- Signature not present.
              perror
      _ ->
        ptraceError "checkSignatoryCont: not a spending tx"

-- | `checkSignatory` implemented using `runTermCont`
checkSignatoryTermCont :: Term s (PPubKeyHash :--> PScriptContext :--> PUnit)
checkSignatoryTermCont = plam $ \ph ctx' -> unTermCont $ do
  ctx <- tcont $ pletFields @["txInfo", "purpose"] ctx'
  tcont (pmatch $ getField @"purpose" ctx) >>= \case
    PSpending _ -> do
      let signatories = pfield @"signatories" # getField @"txInfo" ctx
      pure $
        pif
          (pelem # pdata ph # pfromData signatories)
          -- Success!
          (pconstant ())
          -- Signature not present.
          perror
    _ ->
      pure $ ptraceError "checkSignatoryCont: not a spending tx"

getFields :: Term s (PData :--> PBuiltinList PData)
getFields = phoistAcyclic $ plam $ \addr -> psndBuiltin #$ pasConstr # addr

dummyCurrency :: CurrencySymbol
dummyCurrency = Value.currencySymbol "\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11"

------------------- Mocking a ScriptContext ----------------------------------------

validContext0 :: Term s PScriptContext
validContext0 = mkCtx validOutputs0 validDatums1

invalidContext1 :: Term s PScriptContext
invalidContext1 = mkCtx invalidOutputs1 validDatums1

mkCtx :: [TxOut] -> [(DatumHash, Datum)] -> Term s PScriptContext
mkCtx outs l = pconstant (ScriptContext (info' outs l) purpose)
  where
    info' :: [TxOut] -> [(DatumHash, Datum)] -> TxInfo
    info' outs dat =
      info
        { txInfoData = dat
        , txInfoOutputs = outs
        }

validOutputs0 :: [TxOut]
validOutputs0 =
  [ TxOut
      { txOutAddress =
          Address (ScriptCredential validator) Nothing
      , txOutValue = mempty
      , txOutDatumHash = Just datum
      }
  ]

invalidOutputs1 :: [TxOut]
invalidOutputs1 =
  [ TxOut
      { txOutAddress =
          Address (ScriptCredential validator) Nothing
      , txOutValue = mempty
      , txOutDatumHash = Just datum
      }
  , TxOut
      { txOutAddress =
          Address (ScriptCredential validator) Nothing
      , txOutValue = mempty
      , txOutDatumHash = Nothing
      }
  ]

validDatums1 :: [(DatumHash, Datum)]
validDatums1 =
  [("d0", d0Dat)]

-- | Mock datum that is a list of integers.
d0Dat :: Datum
d0Dat = Datum $ toBuiltinData d0DatValue

d0DatValue :: [Integer]
d0DatValue = [1 .. 10]
