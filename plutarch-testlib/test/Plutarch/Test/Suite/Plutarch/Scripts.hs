module Plutarch.Test.Suite.Plutarch.Scripts (tests) where

import Codec.Serialise (serialise)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy (toStrict)
import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Encoding
import Plutarch.Builtin.Crypto (pverifyEd25519Signature)
import Plutarch.Internal.Term (
  Config (Tracing),
  LogLevel (LogInfo),
  TracingMode (DetTracing),
  compile,
 )
import Plutarch.LedgerApi.V3 (
  PPubKeyHash,
  PScriptContext (pscriptContext'txInfo),
  PTxInfo (ptxInfo'signatories),
  scriptHash,
 )
import Plutarch.Prelude
import Plutarch.Script (Script)
import Plutarch.Test.Golden (goldenEval, goldenGroup, plutarchGolden)
import PlutusLedgerApi.V1 qualified as Plutus
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "scripts"
    [ plutarchGolden
        "Goldens"
        "scripts"
        [ goldenGroup
            "auth_validator"
            [ goldenEval "0" authValidatorTerm
            , goldenEval "hash" (pconstant @PString validatorHashEncoded)
            ]
        , goldenGroup
            "auth_policy"
            [ goldenEval "0" authPolicyTerm
            , goldenEval "hash" (pconstant @PString policySymEncoded)
            ]
        , goldenGroup
            "auth_stake_validator"
            [ goldenEval "0" authStakeValidatorTerm
            , goldenEval "hash" (pconstant @PString stakeValidatorHashEncoded)
            ]
        ]
    ]

base16 :: ByteString -> Text
base16 = Encoding.decodeUtf8 . Base16.encode

authorizedValidator ::
  (forall (s :: S). Term s PByteString) ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PScriptContext ->
  Term s POpaque
authorizedValidator authKey datumMessage redeemerSig _ctx =
  pif
    (pverifyEd25519Signature # authKey # datumMessage # redeemerSig)
    (popaque $ pcon PUnit)
    perror

authorizedPolicy ::
  forall (s :: S).
  (forall (s' :: S). Term s' (PAsData PPubKeyHash)) ->
  Term s PData ->
  Term s PScriptContext ->
  Term s POpaque
authorizedPolicy authHash _redeemer ctx' =
  pmatch ctx' $ \ctx ->
    pmatch (pscriptContext'txInfo ctx) $ \txinfo ->
      pif
        (pelem # authHash # pfromData (ptxInfo'signatories txinfo))
        (popaque $ pcon PUnit)
        perror

authorizedStakeValidator ::
  forall (s :: S).
  (forall (s' :: S). Term s' (PAsData PPubKeyHash)) ->
  Term s PData ->
  Term s PScriptContext ->
  Term s POpaque
authorizedStakeValidator authHash _redeemer ctx' =
  pmatch ctx' $ \ctx ->
    pmatch (pscriptContext'txInfo ctx) $ \txinfo ->
      pif
        (pelem # authHash # pfromData (ptxInfo'signatories txinfo))
        (popaque $ pcon PUnit)
        perror

adminPubKey :: ByteString
adminPubKey = "11661a8aca9b09bb93eefda295b5da2be3f944d1f4253ab29da17db580f50d02d26218e33fbba5e0cc1b0c0cadfb67a5f9a90157dcc19eecd7c9373b0415c888"

adminPubKeyHash :: Plutus.PubKeyHash
adminPubKeyHash = "cc1360b04bdd0825e0c6552abb2af9b4df75b71f0c7cca20256b1f4f"

authValidatorCompiled :: Script
authValidatorCompiled = compileD authValidatorTerm

authValidatorTerm :: forall (s :: S). Term s (PData :--> PData :--> PScriptContext :--> POpaque)
authValidatorTerm =
  plam $ \datum redeemer ctx ->
    authorizedValidator
      (pconstant adminPubKey)
      (pasByteStr # datum)
      (pasByteStr # redeemer)
      ctx

authValidatorHash :: Plutus.ScriptHash
authValidatorHash = scriptHash authValidatorCompiled

authPolicyCompiled :: Script
authPolicyCompiled = compileD authPolicyTerm

authPolicyTerm :: forall (s :: S). Term s (PData :--> PScriptContext :--> POpaque)
authPolicyTerm =
  plam $ \redeemer ctx ->
    authorizedPolicy
      (pconstant @(PAsData PPubKeyHash) adminPubKeyHash)
      redeemer
      ctx

authPolicySymbol :: Plutus.CurrencySymbol
authPolicySymbol =
  Plutus.CurrencySymbol $ Plutus.getScriptHash $ scriptHash authPolicyCompiled

authStakeValidatorCompiled :: Script
authStakeValidatorCompiled = compileD authStakeValidatorTerm

authStakeValidatorTerm :: forall (s :: S). Term s (PData :--> PScriptContext :--> POpaque)
authStakeValidatorTerm =
  plam $ \redeemer ctx ->
    authorizedStakeValidator
      (pconstant @(PAsData PPubKeyHash) adminPubKeyHash)
      redeemer
      ctx

authStakeValidatorHash :: Plutus.ScriptHash
authStakeValidatorHash = scriptHash authStakeValidatorCompiled

validatorHashEncoded :: Text
validatorHashEncoded = base16 . toStrict . serialise $ (coerce authValidatorHash :: Plutus.BuiltinByteString)

policySymEncoded :: Text
policySymEncoded = base16 . toStrict . serialise $ (coerce authPolicySymbol :: Plutus.BuiltinByteString)

stakeValidatorHashEncoded :: Text
stakeValidatorHashEncoded = base16 . toStrict . serialise $ (coerce authStakeValidatorHash :: Plutus.BuiltinByteString)

compileD :: forall (a :: S -> Type). (forall (s :: S). Term s a) -> Script
compileD t = either (error . Text.unpack) id $ compile (Tracing LogInfo DetTracing) t
