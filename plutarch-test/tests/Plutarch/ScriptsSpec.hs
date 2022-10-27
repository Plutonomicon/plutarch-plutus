module Plutarch.ScriptsSpec (
  authorizedValidator,
  authorizedPolicy,
  authorizedStakeValidator,
  authValidatorCompiled,
  validatorEncoded,
  validatorHashEncoded,
  authValidatorHash,
  authStakeValidatorCompiled,
  stakeValidatorEncoded,
  authStakeValidatorHash,
  stakeValidatorHashEncoded,
  authPolicyCompiled,
  policyEncoded,
  policySymEncoded,
  authPolicySymbol,
  spec,
) where

import Codec.Serialise (serialise)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Short (fromShort)
import Data.Coerce (coerce)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE

import PlutusLedgerApi.V1 qualified as Plutus

import Plutarch.Api.V1 (
  PMintingPolicy,
  PScriptContext,
  PStakeValidator,
  PValidator,
  scriptHash,
 )
import Plutarch.Api.V1.Crypto (PPubKeyHash)
import Plutarch.Builtin (pasByteStr)
import Plutarch.Crypto (pverifyEd25519Signature)
import Plutarch.Prelude
import Plutarch.Script (Script, serialiseScript)
import Plutarch.Test
import Plutarch.Test.Golden (compileD)
import Test.Hspec

spec :: Spec
spec = do
  describe "scripts" . pgoldenSpec $ do
    "auth_validator" @\ do
      "0" @| authValidatorTerm
      "hash" @| pconstant validatorHashEncoded
    "auth_policy" @\ do
      "0" @| authPolicyTerm
      "hash" @| pconstant policySymEncoded
    "auth_stake_validator" @\ do
      "0" @| authStakeValidatorTerm
      "hash" @| pconstant stakeValidatorHashEncoded

base16 :: ByteString -> Text
base16 = TE.decodeUtf8 . Base16.encode

type PSignature = PByteString
type PPubKey = PByteString
type PubKey = ByteString

{- |
  A parameterized Validator which may be unlocked
    by signing the Datum Message with the parameter PubKey.
-}
authorizedValidator ::
  ClosedTerm PPubKey ->
  Term s PByteString ->
  Term s PSignature ->
  Term s PScriptContext ->
  Term s POpaque
authorizedValidator authKey datumMessage redeemerSig _ctx =
  pif
    (pverifyEd25519Signature # authKey # datumMessage # redeemerSig)
    (popaque $ pcon PUnit)
    perror

{- |
  A parameterized MintingPolicy which allows minting if
   the parameter PubKeyHash signs the transaction.
-}
authorizedPolicy ::
  forall s.
  ClosedTerm (PAsData PPubKeyHash) ->
  Term s PData ->
  Term s PScriptContext ->
  Term s POpaque
authorizedPolicy authHash _redeemer ctx =
  let sigs :: Term s (PBuiltinList (PAsData PPubKeyHash))
      sigs = pfromData (pfield @"signatories" #$ pfield @"txInfo" # ctx)
   in pif
        (pelem # authHash # sigs)
        (popaque $ pcon PUnit)
        perror

{- |
  A parameterized StakeValidator which allows any StakeValidator action
  if the parameter PubKeyHash signs the transaction.
-}
authorizedStakeValidator ::
  forall s.
  ClosedTerm (PAsData PPubKeyHash) ->
  Term s PData ->
  Term s PScriptContext ->
  Term s POpaque
authorizedStakeValidator authHash _redeemer ctx =
  let sigs :: Term s (PBuiltinList (PAsData PPubKeyHash))
      sigs = pfromData (pfield @"signatories" #$ pfield @"txInfo" # ctx)
   in pif
        (pelem # authHash # sigs)
        (popaque $ pcon PUnit)
        perror

adminPubKey :: PubKey
adminPubKey = "11661a8aca9b09bb93eefda295b5da2be3f944d1f4253ab29da17db580f50d02d26218e33fbba5e0cc1b0c0cadfb67a5f9a90157dcc19eecd7c9373b0415c888"

adminPubKeyHash :: Plutus.PubKeyHash
adminPubKeyHash = "cc1360b04bdd0825e0c6552abb2af9b4df75b71f0c7cca20256b1f4f"

{- |
  We can compile a `Validator` using `compile` &
  `pwrapValidatorFromData`
-}
authValidatorCompiled :: Script
authValidatorCompiled = compileD authValidatorTerm

authValidatorTerm :: ClosedTerm PValidator
authValidatorTerm =
  plam $ \datum redeemer ctx ->
    authorizedValidator
      (pconstant adminPubKey)
      (pasByteStr # datum)
      (pasByteStr # redeemer)
      ctx

-- | `scriptHash` gets the Plutus `ScriptHash`
authValidatorHash :: Plutus.ScriptHash
authValidatorHash = scriptHash authValidatorCompiled

-- | Similarly, for a MintingPolicy
authPolicyCompiled :: Script
authPolicyCompiled = compileD authPolicyTerm

authPolicyTerm :: ClosedTerm PMintingPolicy
authPolicyTerm =
  plam $ \redeemer ctx ->
    authorizedPolicy
      (pconstantData adminPubKeyHash)
      redeemer
      ctx

-- | `mintingPolicySymbol` gets the Plutus `CurrencySymbol`
authPolicySymbol :: Plutus.CurrencySymbol
authPolicySymbol =
  Plutus.CurrencySymbol $ Plutus.getScriptHash $ scriptHash authPolicyCompiled

-- | ...And for a StakeValidator
authStakeValidatorCompiled :: Script
authStakeValidatorCompiled = compileD authStakeValidatorTerm

authStakeValidatorTerm :: ClosedTerm PStakeValidator
authStakeValidatorTerm =
  plam $ \redeemer ctx ->
    authorizedStakeValidator
      (pconstantData adminPubKeyHash)
      redeemer
      ctx

-- | `stakeValidatorHash` gets the Plutus `StakeValidatorHash`
authStakeValidatorHash :: Plutus.ScriptHash
authStakeValidatorHash = scriptHash authStakeValidatorCompiled

-- | `encodeSerialise` will get the hex-encoded serialisation of a script
validatorEncoded :: Text
validatorEncoded = base16 . fromShort . serialiseScript $ authValidatorCompiled

-- | Similarly, with a `MintingPolicy`
policyEncoded :: Text
policyEncoded = base16 . fromShort . serialiseScript $ authPolicyCompiled

-- | And with a `StakeValidator`
stakeValidatorEncoded :: Text
stakeValidatorEncoded = base16 . fromShort . serialiseScript $ authStakeValidatorCompiled

{- |
  We can also encode `ValidatorHash` the same way.

  NB:
  The serialisation from Codec.Serialise will prepend a 4-hexit prefix,
  tagging the type, so this will differ slightly from the encoding
  of the `Show` & `IsString` instances.
  Also note that this is not the addr1/CIP-0019 Address encoding of the script.
-}
validatorHashEncoded :: Text
validatorHashEncoded = base16 . toStrict . serialise $ (coerce authValidatorHash :: Plutus.BuiltinByteString)

-- | The same goes for `CurrencySymbol`
policySymEncoded :: Text
policySymEncoded = base16 . toStrict . serialise $ (coerce authPolicySymbol :: Plutus.BuiltinByteString)

-- | ... And `StakeValidatorHash`
stakeValidatorHashEncoded :: Text
stakeValidatorHashEncoded = base16 . toStrict . serialise $ (coerce authStakeValidatorHash :: Plutus.BuiltinByteString)
