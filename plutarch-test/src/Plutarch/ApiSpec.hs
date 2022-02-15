module Plutarch.ApiSpec (spec, ctx) where

import Test.Syd
import Test.Tasty.HUnit

import Control.Monad.Trans.Cont (cont, runCont)
import Plutus.V1.Ledger.Api
import qualified Plutus.V1.Ledger.Interval as Interval
import qualified Plutus.V1.Ledger.Value as Value

import Plutarch.Api.V1
import Plutarch.Builtin (pasConstr, pforgetData)
import Plutarch.Prelude
import Plutarch.Test

spec :: Spec
spec = do
  describe "api" $ do
    describe "ctx" $ do
      golden PrintTerm ctx
      describe "get" $ do
        describe "txInfo" $ do
          let p = pfromData $ getTxInfo # ctx
          golden All p
          it "works" $ plift p @?= info
        describe "mint" $ do
          let p = pforgetData $ getMint #$ getTxInfo # ctx
          golden All p
          it "works" $ plift p @?= toData mint
        describe "credentials" $ do
          let p = getCredentials ctx
          golden All p
          it "works" $ plift p @?= [toData validator]
        describe "sym" $ do
          let p = pfromData $ getSym #$ pfromData $ getMint #$ getTxInfo # ctx
          golden All p
          it "works" $ plift p @?= sym
    describe "example" $ do
      -- The checkSignatory family of functions implicitly use tracing due to
      -- monadic syntax, and as such we need two sets of tests here.
      -- See Plutarch.MonadicSpec for GHC9 only syntax.
      describe "signatory" . plutarchDevFlagDescribe $ do
        let aSig :: PubKeyHash = "ab01fe235c"
        describe "cont" $ do
          let p = checkSignatoryCont # pconstant aSig # ctx
              pe = checkSignatoryCont # pconstant "41" # ctx
          golden All p
          it "succeeds" $ psucceeds p
          it "fails" $ pfails pe
        describe "termcont" $ do
          let p = checkSignatoryTermCont # pconstant aSig # ctx
              pe = checkSignatoryTermCont # pconstant "41" # ctx
          golden All p
          it "succeeds" $ psucceeds p
          it "fails" $ pfails pe
      describe "getFields" $
        golden PrintTerm getFields

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
    purpose <- cont (pmatch $ hrecField @"purpose" ctx)
    pure $ case purpose of
      PSpending _ ->
        let signatories :: Term s (PBuiltinList (PAsData PPubKeyHash))
            signatories = pfield @"signatories" # hrecField @"txInfo" ctx
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
  tcont (pmatch $ hrecField @"purpose" ctx) >>= \case 
    PSpending _ -> do
      let signatories = pfield @"signatories" # hrecField @"txInfo" ctx
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
