module Plutarch.Extra.Api (
  pgetContinuingOutputs,
  pfindOwnInput,
  pfindDatum,
) where

import Plutarch.Api.V1 (
  PAddress,
  PDatum,
  PDatumHash,
  PScriptContext,
  PScriptPurpose (PSpending),
  PTuple,
  PTxInInfo,
  PTxInfo,
  PTxOut,
  PTxOutRef,
 )
import Plutarch.Prelude

import Plutarch.Extra.Monad (tmatch)

-- | Find the output txns corresponding to the input being validated.
pgetContinuingOutputs :: Term s (PScriptContext :--> PBuiltinList PTxOut)
pgetContinuingOutputs = phoistAcyclic $
  plam $ \sc -> unTermCont $ do
    let txinfo = pfield @"txInfo" # sc
    tmatch (pfindOwnInput # sc) >>= \case
      PJust te -> do
        let outs = pfield @"outputs" # txinfo
            resolved = pfield @"resolved" # te
            outAdr = pfield @"address" # resolved
        pure $ pfilter # (matches # outAdr) #$ pmap # plam pfromData # outs
      PNothing ->
        pure $ ptraceError "can't get any continuing outputs"
  where
    matches :: Term s (PAddress :--> PTxOut :--> PBool)
    matches = phoistAcyclic $
      plam $ \adr txOut ->
        adr #== pfield @"address" # txOut

{- | Find the input currently being validated.

  Tries to finds the transaction's input by looking for a `PTxInInfo` in the inputs
  coresponding to the `PTxOutRef` which the script purpose is spending
-}
pfindOwnInput :: Term s (PScriptContext :--> PMaybe PTxInInfo)
pfindOwnInput = phoistAcyclic $
  plam $ \sc -> unTermCont $ do
    ctx <- tcont $ pletFields @["txInfo", "purpose"] sc
    tmatch (getField @"purpose" ctx) >>= \case
      PSpending outRef' -> do
        let outRef = pfield @"_0" # outRef'
        pure $
          pfind # (matches # outRef)
            #$ pmap # plam pfromData
            #$ pfromData
            $ pfield @"inputs" # (getField @"txInfo" ctx)
      _ ->
        pure $ pcon PNothing
  where
    matches :: Term s (PTxOutRef :--> PTxInInfo :--> PBool)
    matches = phoistAcyclic $
      plam $ \outref txininfo ->
        pfield @"id" # outref
          #== pfield @"id" # (pfield @"outRef" # txininfo)

-- | Find the data corresponding to a data hash, if there is one
pfindDatum :: Term s (PDatumHash :--> PTxInfo :--> PMaybe PDatum)
pfindDatum = phoistAcyclic $
  plam $ \dh txinfo -> unTermCont $ do
    let txInfoData = pfield @"datums" # txinfo
        maybeEnt = pfind # (matches # dh) # txInfoData
    pure $
      pmatch maybeEnt $ \case
        PNothing -> pcon PNothing
        PJust x -> pcon $ PJust $ pdsnd # x
  where
    matches :: (PEq k, PIsData k) => Term s (k :--> PAsData (PTuple k v) :--> PBool)
    matches = phoistAcyclic $
      plam $ \a ab ->
        a #== pdfst # ab

pdfst :: PIsData k => Term s (PAsData (PTuple k v) :--> k)
pdfst = pfield @"_0"

pdsnd :: PIsData v => Term s (PAsData (PTuple k v) :--> v)
pdsnd = pfield @"_1"
