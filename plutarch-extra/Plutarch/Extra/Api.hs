module Plutarch.Extra.Api (
  pgetContinuingOutputs,
  pfindOwnInput,
  pfindDatum,
) where

import Plutarch.Api.V1 (
  PAddress,
  PDatum,
  PDatumHash,
  PTuple,
  PTxInInfo,
  PTxOut,
  PTxOutRef,
 )
import Plutarch.Prelude

{- | Find the output txns corresponding to the input being validated.

  Takes as arguments the inputs, outputs and the spending transaction referenced
  from `PScriptPurpose`.

  __Example:__

  @
  ctx <- tcont $ pletFields @["txInfo", "purpose"] sc
  pmatchC (getField @"purpose" ctx) >>= \case
    PSpending outRef' -> do
      let outRef = pfield @"_0" # outRef'
          inputs = pfield @"inputs" # (getField @"txInfo" ctx)
          outputs = pfield @"outputs" # (getField @"txInfo" ctx)
      pure $ pgetContinuingOutputs # inputs # outputs # outRef
    _ ->
      pure $ ptraceError "not a spending tx"
  @
-}
pgetContinuingOutputs :: Term s (PBuiltinList (PAsData PTxInInfo) :--> PBuiltinList (PAsData PTxOut) :--> PTxOutRef :--> PBuiltinList PTxOut)
pgetContinuingOutputs = phoistAcyclic $
  plam $ \inputs outputs outRef ->
    pmatch (pfindOwnInput # inputs # outRef) $ \case
      PJust tx -> do
        let resolved = pfield @"resolved" # tx
            outAddr = pfield @"address" # resolved
        pfilter # (matches # outAddr) #$ pmap # plam pfromData # outputs
      PNothing ->
        ptraceError "can't get any continuing outputs"
  where
    matches :: Term s (PAddress :--> PTxOut :--> PBool)
    matches = phoistAcyclic $
      plam $ \adr txOut ->
        adr #== pfield @"address" # txOut

{- | Find the input being spent in the current transaction.

  Takes as arguments the inputs, as well as the spending transaction referenced from `PScriptPurpose`.

  __Example:__

  @
  ctx <- tcont $ pletFields @["txInfo", "purpose"] sc
  pmatchC (getField @"purpose" ctx) >>= \case
    PSpending outRef' -> do
      let outRef = pfield @"_0" # outRef'
          inputs = pfield @"inputs" # (getField @"txInfo" ctx)
      pure $ pfindOwnInput # inputs # outRef
    _ ->
      pure $ ptraceError "not a spending tx"
  @
-}
pfindOwnInput :: Term s (PBuiltinList (PAsData PTxInInfo) :--> PTxOutRef :--> PMaybe PTxInInfo)
pfindOwnInput = phoistAcyclic $
  plam $ \inputs outRef ->
    pfind # (matches # outRef) #$ pmap # plam pfromData #$ inputs
  where
    matches :: Term s (PTxOutRef :--> PTxInInfo :--> PBool)
    matches = phoistAcyclic $
      plam $ \outref txininfo ->
        pfield @"id" # outref
          #== pfield @"id" # (pfield @"outRef" # txininfo)

{- | Lookup up the datum given the datum hash.

  Takes as argument the datum assoc list from a `PTxInfo`.

  __Example:__

  @
  pfindDatum # datumHash #$ pfield @"datums" # txinfo
  @
-}
pfindDatum :: Term s (PDatumHash :--> PBuiltinList (PAsData (PTuple PDatumHash PDatum)) :--> PMaybe PDatum)
pfindDatum = phoistAcyclic $
  plam $ \dh datums ->
    pmatch (pfind # (matches # dh) # datums) $ \case
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
