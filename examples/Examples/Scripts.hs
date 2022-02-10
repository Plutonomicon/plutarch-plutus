module Examples.Scripts (
  authorizedValidator,
  authorizedPolicy,
  authorizedStakeValidator,
  authValidatorCompiled,
  authValidatorHash,
  authStakeValidatorCompiled,
  authStakeValidatorHash,
  authPolicyCompiled,
  authPolicySymbol,
  writeValidator,
  writePolicy,
  writeStakeValidator,
  tests,
) where

import Codec.Serialise (Serialise, serialise)
import qualified Data.ByteString.Lazy as BS
import Data.Text (Text)
import qualified Data.Text.IO as Text

import Data.Aeson.Extras (encodeByteString)
import qualified Plutus.V1.Ledger.Api as Plutus
import qualified Plutus.V1.Ledger.Crypto as Plutus

import Plutarch (ClosedTerm)
import Plutarch.Api.V1 (PScriptContext)
import Plutarch.Api.V1.Crypto (PPubKey, PPubKeyHash, PSignature)
import Plutarch.Api.V1.Scripts (
  MintingPolicy,
  StakeValidator,
  Validator,
  mintingPolicySymbol,
  mkMintingPolicy,
  mkStakeValidator,
  mkValidator,
  pwrapMintingPolicyFromData,
  pwrapStakeValidatorFromData,
  pwrapValidatorFromData,
  serialiseMintingPolicy,
  serialiseStakeValidator,
  serialiseValidator,
  stakeValidatorHash,
  validatorHash,
 )
import Plutarch.Prelude
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.Golden (DeleteOutputFile (OnPass), goldenVsFile)

import Utils

{- |
  A parameterized Validator which may be unlocked
    by signing the Datum Message with the parameter PubKey.
-}
authorizedValidator ::
  ClosedTerm PPubKey ->
  Term s PByteString ->
  Term s PSignature ->
  Term s PScriptContext ->
  Term s PUnit
authorizedValidator authKey datumMessage redeemerSig _ctx =
  pif
    (pverifySignature # pto authKey # datumMessage # pto redeemerSig)
    (pcon PUnit)
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
  Term s PUnit
authorizedPolicy authHash _redeemer ctx =
  let sigs :: Term s (PBuiltinList (PAsData PPubKeyHash))
      sigs = pfromData (pfield @"signatories" #$ pfield @"txInfo" # ctx)
   in pif
        (pelem # authHash # sigs)
        (pcon PUnit)
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
  Term s PUnit
authorizedStakeValidator authHash _redeemer ctx =
  let sigs :: Term s (PBuiltinList (PAsData PPubKeyHash))
      sigs = pfromData (pfield @"signatories" #$ pfield @"txInfo" # ctx)
   in pif
        (pelem # authHash # sigs)
        (pcon PUnit)
        perror

adminPubKey :: Plutus.PubKey
adminPubKey = "11661a8aca9b09bb93eefda295b5da2be3f944d1f4253ab29da17db580f50d02d26218e33fbba5e0cc1b0c0cadfb67a5f9a90157dcc19eecd7c9373b0415c888"

adminPubKeyHash :: Plutus.PubKeyHash
adminPubKeyHash = "cc1360b04bdd0825e0c6552abb2af9b4df75b71f0c7cca20256b1f4f"

{- |
  We can compile a `Validator` using `mkValidator` &
  `pwrapValidatorFromData`
-}
authValidatorCompiled :: Validator
authValidatorCompiled =
  mkValidator $
    pwrapValidatorFromData $
      authorizedValidator $ pconstant adminPubKey

-- | `validatorHash` gets the Plutus `ValidatorHash`
authValidatorHash :: Plutus.ValidatorHash
authValidatorHash = validatorHash authValidatorCompiled

-- | Similarly, for a MintingPolicy
authPolicyCompiled :: MintingPolicy
authPolicyCompiled =
  mkMintingPolicy $
    pwrapMintingPolicyFromData $
      authorizedPolicy $ pconstantData adminPubKeyHash

-- | `mintingPolicySymbol` gets the Plutus `CurrencySymbol`
authPolicySymbol :: Plutus.CurrencySymbol
authPolicySymbol =
  mintingPolicySymbol authPolicyCompiled

-- | ...And for a StakeValidator
authStakeValidatorCompiled :: StakeValidator
authStakeValidatorCompiled =
  mkStakeValidator $
    pwrapStakeValidatorFromData $
      authorizedStakeValidator $ pconstantData adminPubKeyHash

-- | `stakeValidatorHash` gets the Plutus `StakeValidatorHash`
authStakeValidatorHash :: Plutus.StakeValidatorHash
authStakeValidatorHash = stakeValidatorHash authStakeValidatorCompiled

-- | Serializing & hex-encoding a Validator to a file
writeValidator :: FilePath -> Validator -> IO ()
writeValidator path validator = do
  Text.writeFile path $
    encodeHex $
      serialiseValidator validator

-- | Serialising & hex-encoding a Validator to a file
writePolicy :: FilePath -> MintingPolicy -> IO ()
writePolicy path policy = do
  Text.writeFile path $
    encodeHex $
      serialiseMintingPolicy policy

-- | Serialising & hex-encoding a Validator to a file
writeStakeValidator :: FilePath -> StakeValidator -> IO ()
writeStakeValidator path stakeValidator = do
  Text.writeFile path $
    encodeHex $
      serialiseStakeValidator stakeValidator

{- |
  Convert a ByteString to a Base16 encoding, using
  `Data.Aeson.Extras.encodeByteString` from plutus-ledger-api.

  Note that there is an extra 4-hexit tag prepended to
  the output - due to the Serialise instance tags.
-}
encodeHex :: BS.ByteString -> Text
encodeHex = encodeByteString . BS.toStrict

{- |
  Serializing & writing a script hash to a file.

  NB: the `Serialise` instances will prepend a 4-hexit
  tag on the output repr.
  This is also different from the Cardano `addr1`
  (CIP-0019/bech32) format.
-}
writeHash :: (Serialise hash) => FilePath -> hash -> IO ()
writeHash path hash = do
  Text.writeFile path $
    encodeHex $
      serialise hash

tests :: HasTester => TestTree
tests =
  localOption (OnPass) $
    testGroup
      "Script compiling, serializing & hashing tests"
      [ goldenVsFile
          "authValidator serialize"
          validatorGolden
          validator
          (writeValidator validator authValidatorCompiled)
      , goldenVsFile
          "authPolicy serialize"
          policyGolden
          policy
          (writePolicy policy authPolicyCompiled)
      , goldenVsFile
          "authStakeValidator serialize"
          stakeGolden
          stake
          (writeStakeValidator stake authStakeValidatorCompiled)
      , goldenVsFile
          "authValidator hash"
          validatorHashGolden
          validatorHash
          (writeHash validatorHash authValidatorHash)
      , goldenVsFile
          "authPolicy hash"
          policyHashGolden
          policyHash
          (writeHash policyHash authPolicySymbol)
      , goldenVsFile
          "authStakeValidator hash"
          stakeHashGolden
          stakeHash
          (writeHash stakeHash authStakeValidatorHash)
      ]
  where
    validator = goldenPath "authValidator.plutus"
    validatorGolden = goldenSuffix validator

    policy = goldenPath "authPolicy.plutus"
    policyGolden = goldenSuffix policy

    stake = goldenPath "authStakeValidator.plutus"
    stakeGolden = goldenSuffix stake

    validatorHash = goldenPath "authValidator.hash"
    validatorHashGolden = goldenSuffix validatorHash

    policyHash = goldenPath "authPolicy.hash"
    policyHashGolden = goldenSuffix policyHash

    stakeHash = goldenPath "authStakeValidator.hash"
    stakeHashGolden = goldenSuffix stakeHash

    goldenPath = ("./examples/golden/" <>)
    goldenSuffix = (<> ".golden")
