{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}

module Plutarch.MonadicSpec (spec) where

import Test.Syd

import Plutus.V1.Ledger.Api

import Plutarch.Api.V1 (
  PAddress (PAddress),
  PCredential,
  PMaybeData,
  PPubKeyHash,
  PScriptContext,
  PScriptPurpose (PSpending),
  PStakingCredential,
 )
import qualified Plutarch.ApiSpec as ApiSpec
import qualified Plutarch.Monadic as P
import Plutarch.Prelude
import Plutarch.Test

spec :: Spec
spec = do
  describe "monadic" $ do
    describe "api.example" $ do
      -- The checkSignatory family of functions implicitly use tracing due to
      -- monadic syntax, and as such we need two sets of tests here.
      describe "signatory" . plutarchDevFlagDescribe . pgoldenSpec $ do
        let aSig :: PubKeyHash = "ab01fe235c"
        "do" @\ do
          "succeeds" @| checkSignatory # pconstant aSig # ApiSpec.ctx @-> psucceeds
          "fails" @| checkSignatory # pconstant "41" # ApiSpec.ctx @-> pfails
      describe "getFields" . pgoldenSpec $ do
        "0" @| getFields

checkSignatory :: Term s (PPubKeyHash :--> PScriptContext :--> PUnit)
checkSignatory = plam $ \ph ctx' ->
  pletFields @["txInfo", "purpose"] ctx' $ \ctx -> P.do
    PSpending _ <- pmatch $ ctx.purpose
    let signatories = pfield @"signatories" # ctx.txInfo
    pif
      (pelem # pdata ph # pfromData signatories)
      -- Success!
      (pconstant ())
      -- Signature not present.
      perror

getFields :: Term s (PAddress :--> PDataRecord '["credential" ':= PCredential, "stakingCredential" ':= PMaybeData PStakingCredential])
getFields = phoistAcyclic $
  plam $ \addr -> P.do
    PAddress addrFields <- pmatch addr
    addrFields
