module Examples.Scripts where

import Data.ByteString.Lazy (writeFile)

import Plutarch.Api.V1 (PScriptContext)
import Plutarch.Api.V1.Scripts ()
import Plutarch.Prelude
import Test.Tasty.Golden (goldenVsFile)

import Util

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
  let 
    pkBytes = pmatch authKey $ \(PPubKey bs) -> bs
    sigBytes = pmatch redeemerSig $ \(PSignature bs) -> bs
  in
  pif 
    (pverifySignature # pkBytes # datumMessage # sigBytes) 
    (pcon PUnit)
    perror

{- |
  A parameterized MintingPolicy which allows minting if 
   the parameter PubKeyHash signs the transaction.
-}
authorizedPolicy :: 
  ClosedTerm (PAsData PPubKeyHash) ->
  Term s PUnit ->
  Term s PScriptContext ->
  Term s PUnit
authorizedPolicy authHash redeemer ctx =
  let 
    sigs :: PBuiltinList (PAsData PPubKeyHash)
    sigs = pfromData $ pfield @"signatories" $ pfield @"txInfo" ctx
  in
  pif
    (pelem # authHash # sigs)
    redeemer
    perror

{- | 
  A parameterized StakeValidator which allows any StakeValidator action 
  if the parameter PubKeyHash signs the transaction.
-}
authorizedStakeValidator :: 
  ClosedTerm (PAsData PPubKeyHash) ->
  Term s PUnit -> 
  Term s PScriptContext -> 
  Term s PUnit
authorizedStakeValidator authHash redeemer ctx =
  let 
    sigs :: PBuiltinList (PAsData PPubKeyHash)
    sigs = pfromData $ pfield @"signatories" $ pfield @"txInfo" ctx
  in
  pif
    (pelem # authHash # sigs)
    redeemer
    perror


adminPubKey :: Plutus.PubKey
adminPubKey = ""

adminPubKeyHash :: Plutus.PubKeyHash
adminPubKeyHash = ""

{- |
  We can compile a `Validator` using `mkValidator` & 
  `pwrapValidatorFromData`

-}
authValidatorCompiled :: Validator
authValidatorCompiled = 
  mkValidator $ pwrapValidatorFromData $ 
    authorizedValidator $ pconstant adminPubKey


-- | Similarly, for a MintingPolicy
authPolicyCompiled :: MintingPolicy
authPolicyCompiled =
  mkMintingPolicy $ pwrapMintingPolicyFromData $ 
    authorizedPolicy $ pconstantData adminPubKeyHash

-- | ...And for a StakeValidator
authStakeValidatorCompiled :: StakeValidator
authStakeValidatorCompiled =
  mkStakeValidator $ pwrapMintingPolicyFromData $ 
    authorizedStakeValidator $ pconstantData adminPubKeyHash


-- | Serialising a validator to a file
writeValidator :: FilePath -> Validator -> IO ()
writeValidator path validator = do
  writeFile path $ serialiseValidator validator

-- | Serialising a MintingPolicy to a file
writePolicy :: FilePath -> MintingPolicy -> IO ()
writePolicy path policy = do
  writeFile path $ serialiseMintingPolicy policy

-- | Serialising a MintingPolicy to a file
writeStakeValidator :: FilePath -> StakeValidator -> IO ()
writeStakeValidator path stakeValidator = do
  writeFile path $ serialiseStakeValidator stakeValidator

tests :: HasTester => TestTree
tests = 
  testGroup
    "Script compiling, serializing & hashing tests"
    [ goldenVsFile "authValidator serialize"
        "./golden/authValidator.plutus.golden"
        "./golden/authValidator.plutus"
        (writeValidator "./golden/authValidator.plutus" authValidatorCompiled)
    , goldenVsFile "authPolicy serialize"
        "./golden/authPolicy.plutus.golden"
        "./golden/authPolicy.plutus"
        (writePolicy "./golden/authPolicy.plutus" authPolicyCompiled)
    , goldenVsFile "authStakeValidator serialize"
        "./golden/authStakeValidator.plutus.golden"
        "./golden/authStakeValidator.plutus"
        (writeStakeValidator "./golden/authStakeValidator.plutus" authStakeValidatorCompiled)
    ]




    

  
  
  





