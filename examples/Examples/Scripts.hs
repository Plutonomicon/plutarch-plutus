module Examples.Scripts (
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
  tests,
) where

import Data.Text (Text)

import Data.Aeson.Extras (encodeSerialise)
import qualified Plutus.V1.Ledger.Api as Plutus
import qualified Plutus.V1.Ledger.Crypto as Plutus

import Plutarch (ClosedTerm, POpaque, popaque)
import Plutarch.Api.V1 (
  PScriptContext,
  mintingPolicySymbol,
  mkMintingPolicy,
  mkStakeValidator,
  mkValidator,
  stakeValidatorHash,
  validatorHash,
 )
import Plutarch.Api.V1.Crypto (PPubKey, PPubKeyHash, PSignature (PSignature))
import Plutarch.Builtin (pasByteStr)
import Plutarch.Prelude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

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
  Term s POpaque
authorizedValidator authKey datumMessage redeemerSig _ctx =
  pif
    (pverifySignature # pto authKey # datumMessage # pto redeemerSig)
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

adminPubKey :: Plutus.PubKey
adminPubKey = "11661a8aca9b09bb93eefda295b5da2be3f944d1f4253ab29da17db580f50d02d26218e33fbba5e0cc1b0c0cadfb67a5f9a90157dcc19eecd7c9373b0415c888"

adminPubKeyHash :: Plutus.PubKeyHash
adminPubKeyHash = "cc1360b04bdd0825e0c6552abb2af9b4df75b71f0c7cca20256b1f4f"

{- |
  We can compile a `Validator` using `mkValidator` &
  `pwrapValidatorFromData`
-}
authValidatorCompiled :: Plutus.Validator
authValidatorCompiled =
  mkValidator $
    plam $ \datum redeemer ctx ->
      authorizedValidator
        (pconstant adminPubKey)
        (pasByteStr # datum)
        (pcon $ PSignature $ pasByteStr # redeemer)
        ctx

-- | `validatorHash` gets the Plutus `ValidatorHash`
authValidatorHash :: Plutus.ValidatorHash
authValidatorHash = validatorHash authValidatorCompiled

-- | Similarly, for a MintingPolicy
authPolicyCompiled :: Plutus.MintingPolicy
authPolicyCompiled =
  mkMintingPolicy $
    plam $ \redeemer ctx ->
      authorizedPolicy
        (pconstantData adminPubKeyHash)
        redeemer
        ctx

-- | `mintingPolicySymbol` gets the Plutus `CurrencySymbol`
authPolicySymbol :: Plutus.CurrencySymbol
authPolicySymbol =
  mintingPolicySymbol authPolicyCompiled

-- | ...And for a StakeValidator
authStakeValidatorCompiled :: Plutus.StakeValidator
authStakeValidatorCompiled =
  mkStakeValidator $
    plam $ \redeemer ctx ->
      authorizedStakeValidator
        (pconstantData adminPubKeyHash)
        redeemer
        ctx

-- | `stakeValidatorHash` gets the Plutus `StakeValidatorHash`
authStakeValidatorHash :: Plutus.StakeValidatorHash
authStakeValidatorHash = stakeValidatorHash authStakeValidatorCompiled

-- | `encodeSerialise` will get the hex-encoded serialisation of a script
validatorEncoded :: Text
validatorEncoded = encodeSerialise authValidatorCompiled

validatorEncoded' :: Text
validatorEncoded' = "585901000022253335734666e552214011661a8aca9b09bb93eefda295b5da2be3f944d1f4253ab29da17db580f50d02d26218e33fbba5e0cc1b0c0cadfb67a5f9a90157dcc19eecd7c9373b0415c88800375c0066eb8008526161"

-- | Similarly, with a `MintingPolicy`
policyEncoded :: Text
policyEncoded = encodeSerialise authPolicyCompiled

policyEncoded' :: Text
policyEncoded' = "588e010000323232323222533300333232323002233002002001230022330020020012253335573e002294054ccc018cdd798040008018a511300230070014c11e581ccc1360b04bdd0825e0c6552abb2af9b4df75b71f0c7cca20256b1f4f00375864600c64600c600c600c600c600c600c600c002600e002600a600c0022930b2b9a5744ae848c008dd5000aab9e01"

-- | And with a `StakeValidator`
stakeValidatorEncoded :: Text
stakeValidatorEncoded = encodeSerialise authStakeValidatorCompiled

stakeValidatorEncoded' :: Text
stakeValidatorEncoded' = "588e010000323232323222533300333232323002233002002001230022330020020012253335573e002294054ccc018cdd798040008018a511300230070014c11e581ccc1360b04bdd0825e0c6552abb2af9b4df75b71f0c7cca20256b1f4f00375864600c64600c600c600c600c600c600c600c002600e002600a600c0022930b2b9a5744ae848c008dd5000aab9e01"

{- |
  We can also encode `ValidatorHash` the same way.

  NB:
  The serialisation from Codec.Serialise will prepend a 4-hexit prefix,
  tagging the type, so this will differ slightly from the encoding
  of the `Show` & `IsString` instances.
  Also note that this is not the addr1/CIP-0019 Address encoding of the script.
-}
validatorHashEncoded :: Text
validatorHashEncoded = encodeSerialise authValidatorHash

validatorHashEncoded' :: Text
validatorHashEncoded' = "581cb8c68ee0b38d3c830ae47aec8154b1c35eeabaceaf2c00bea1f33865"

-- | The same goes for `CurrencySymbol`
policySymEncoded :: Text
policySymEncoded = encodeSerialise authPolicySymbol

policySymEncoded' :: Text
policySymEncoded' = encodeSerialise authPolicySymbol

-- | ... And `StakeValidatorHash`
stakeValidatorHashEncoded :: Text
stakeValidatorHashEncoded = encodeSerialise authStakeValidatorHash

stakeValidatorHashEncoded' :: Text
stakeValidatorHashEncoded' = "581cb9f49b1f51a0c1c285c9fde6b1da21e7094f7c19efb6eeace1ada858"

tests :: HasTester => TestTree
tests =
  testGroup
    "Script compiling, serializing & hashing tests"
    [ testCase "authValidator serialize" $
        validatorEncoded @?= validatorEncoded'
    , testCase "authPolicy serialize" $
        policyEncoded @?= policyEncoded'
    , testCase "stakeValidator serialize" $
        stakeValidatorEncoded @?= stakeValidatorEncoded'
    , testCase
        "authValidator hash encoding"
        $ validatorHashEncoded @?= validatorHashEncoded'
    , testCase
        "authPolicy hash encoding"
        $ policySymEncoded @?= policySymEncoded'
    , testCase
        "authStakeValidator hash encoding"
        $ stakeValidatorHashEncoded @?= stakeValidatorHashEncoded'
    ]
