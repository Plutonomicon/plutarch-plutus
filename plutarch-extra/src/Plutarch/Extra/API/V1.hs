{-# LANGUAGE UndecidableInstances #-}

-- TODO the PValue instances should go to
-- Plutarc.Api.V1.Value but they
-- depend on quite a bit of
-- stuff in plutarch-extra
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Extra.API.V1 (
  PAssetClass (..),
  PMonoid,
  valueLTE,
  assetClass,
  assetClassValue,
  assetClassValueOf,
  valSub,
  getContinuingOutputs,
  findDatum,
  findOwnInput,
  convertValue,
  convertBackValue,
  mustPayToPubKey,
) where

import Plutarch.Prelude

import Plutarch.Api.V1 (
  PAddress (PAddress),
  PCredential (PPubKeyCredential),
  PCurrencySymbol,
  PDatum,
  PDatumHash,
  PMap (..),
  PPubKeyHash,
  PScriptContext (..),
  PScriptPurpose (PSpending),
  PTokenName,
  PTuple,
  PTxInInfo (..),
  PTxInfo (..),
  PTxOut (..),
  PTxOutRef (..),
  PValue (..),
 )
import Plutarch.Builtin (ppairDataBuiltin)
import Plutarch.DataRepr (
  PDataFields,
  PIsDataReprInstances (..),
 )
import Plutarch.List (pconvertLists)

import Plutarch.Extra.List (psort)
import Plutarch.Extra.Map (
  Map (Map),
  findWithDefault,
  mapFromBuiltin,
  mapJoin,
  mapLTE,
  mapSplit,
  mapSub,
  mapToBuiltin,
  mmap,
  numEq,
  unionWith,
 )
import Plutarch.Extra.Monadic (tcon, tlet, tletField, tmatch, tmatchField)

import qualified GHC.Generics as GHC
import Generics.SOP (Generic, I (I))

newtype PAssetClass (s :: S)
  = PAssetClass
      ( Term
          s
          ( PDataRecord
              '[ "currencySymbol" ':= PCurrencySymbol
               , "tokenName" ':= PTokenName
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving
    (PMatch, PIsData, PDataFields, PlutusType)
    via PIsDataReprInstances PAssetClass

type PAssetClass' = PPair PCurrencySymbol PTokenName
type PMonoid (a :: PType) = forall s. Monoid (Term s a)

instance PEq PValue where
  a' #== b' =
    phoistAcyclic
      ( plam $ \a b -> unTermCont $ do
          ma <- tlet $ convertValue # a
          mb <- tlet $ convertValue # b
          pure $ numEq # ma # mb
      )
      # a'
      # b'

instance Semigroup (Term s PValue) where
  a' <> b' =
    phoistAcyclic
      ( plam $ \a b ->
          convertBackValue #$ unionWith # plam (+) # (convertValue # a) # (convertValue # b)
      )
      # a'
      # b'

instance Monoid (Term s PValue) where
  mempty = pcon $ PValue $ pcon $ PMap $ pcon PNil

valueLTE :: Term s (PValue :--> PValue :--> PBool)
valueLTE = phoistAcyclic $
  plam $ \l r ->
    mapLTE # (convertValue # l) # (convertValue # r)

convertValue :: Term s (PValue :--> Map PAssetClass' PInteger)
convertValue = phoistAcyclic $
  plam $ \m -> unTermCont $ do
    PValue m1 <- tmatch m
    m2 <- tlet $ convertPMap # m1
    m3 <- tlet $ mmap # convertPMap # m2
    Map m4 <- tmatch $ mapJoin # m3
    pure $
      pcon $ Map $ psort # m4

-- TODO if we can verify that Values
-- from on chain are sorted
-- or can at least be required to be sorted
-- we can safely remove this sort
-- once we have a test net we should
-- look into this

convertBackValue :: Term s (Map PAssetClass' PInteger :--> PValue)
convertBackValue = phoistAcyclic $
  plam $ \m -> unTermCont $ do
    m1 <- tlet $ mapSplit # m
    m2 <- tlet $ mmap # convertBackPMap # m1
    m3 <- tlet $ convertBackPMap # m2
    tcon $ PValue m3

convertPMap :: (PIsData a, PIsData b) => Term s (PMap a b :--> Map a b)
convertPMap = phoistAcyclic $
  plam $ \pm -> unTermCont $ do
    PMap bmap <- tmatch pm
    pure $ mapFromBuiltin # bmap

convertBackPMap :: (PIsData a, PIsData b) => Term s (Map a b :--> PMap a b)
convertBackPMap = phoistAcyclic $
  plam $ \m -> unTermCont $ do
    m' <- tlet $ mapToBuiltin # m
    tcon $ PMap m'

assetClass :: Term s (PCurrencySymbol :--> PTokenName :--> PAssetClass)
assetClass = phoistAcyclic $
  plam $ \cs t ->
    pcon $
      PAssetClass $
        pdcons
          # pdata cs #$ pdcons
          # pdata t #$ pdnil

assetClassValue :: Term s (PAssetClass :--> PInteger :--> PValue)
assetClassValue = phoistAcyclic $
  plam $ \ac n -> unTermCont $ do
    cs <- tletField @"currencySymbol" ac
    tn <- tletField @"tokenName" ac
    tcon $ PValue $ pcon $ PMap $ psingleton
      #$ ppairDataBuiltin
        # pdata cs
        #$ pdata $ pcon $ PMap $ psingleton
          #$ ppairDataBuiltin
            # pdata tn
            # pdata n

assetClassValueOf :: Term s (PValue :--> PAssetClass :--> PInteger)
assetClassValueOf = phoistAcyclic $
  plam $ \v ac -> findWithDefault # 0 # (convertAC' # ac) # (convertValue # v)

valSub :: Term s (PValue :--> PValue :--> PValue)
valSub = phoistAcyclic $
  plam $ \l r -> do
    convertBackValue #$ mapSub # (convertValue # l) # (convertValue # r)

getContinuingOutputs :: Term s (PScriptContext :--> PBuiltinList PTxOut)
getContinuingOutputs = phoistAcyclic $
  plam $ \sc -> unTermCont $ do
    txinfo <- tletField @"txInfo" sc
    outs <- tletField @"outputs" txinfo
    pure $
      pmatch (findOwnInput # sc) $ \case
        PJust te -> unTermCont $ do
          resolved <- tletField @"resolved" te
          outAdr <- tletField @"address" resolved
          pure $ pfilter # (matches # outAdr) #$ pmap # plam pfromData # outs
        PNothing -> ptraceError "can't get any continuing outputs"
  where
    matches :: Term s (PAddress :--> PTxOut :--> PBool)
    matches = phoistAcyclic $
      plam $ \adr txOut -> unTermCont $ do
        outAdr <- tletField @"address" txOut
        pure $ adr #== outAdr

findDatum :: Term s (PDatumHash :--> PTxInfo :--> PMaybe PDatum)
findDatum = phoistAcyclic $
  plam $ \dh txinfo -> unTermCont $ do
    txInfoData <- tletField @"data" txinfo
    maybeEnt <- tlet $ pfind # (matches # dh) # txInfoData
    pure $
      pmatch maybeEnt $ \case
        PNothing -> pcon PNothing
        PJust x -> pcon $ PJust $ pfromData $ pfield @"_1" # x
  where
    matches :: Term s (PDatumHash :--> PAsData (PTuple PDatumHash PDatum) :--> PBool)
    matches = phoistAcyclic $
      plam $ \dh dataTupe -> unTermCont $ do
        tupe <- tlet $ pfromData dataTupe
        pure $
          dh #== pfromData (pfield @"_0" # tupe)

findOwnInput :: Term s (PScriptContext :--> PMaybe PTxInInfo)
findOwnInput = phoistAcyclic $
  plam $ \sc -> unTermCont $ do
    PScriptContext te <- tmatch sc
    pure $
      pmatch (pfromData $ pfield @"purpose" # te) $ \case
        PSpending outRef' -> unTermCont $ do
          outRef <- tletField @"_0" outRef'
          PTxInfo txinfo <- tmatchField @"txInfo" te
          is <- tlet $ pmap # plam pfromData #$ pfromData $ pfield @"inputs" # txinfo
          pure $
            pfind # (matches # outRef) # is
        _ -> pcon PNothing
  where
    matches :: Term s (PTxOutRef :--> PTxInInfo :--> PBool)
    matches = phoistAcyclic $
      plam $ \outref txininfo -> unTermCont $ do
        PTxOutRef outref' <- tmatch outref
        outRefId <- tletField @"id" outref'
        PTxInInfo txininfo' <- tmatch txininfo
        PTxOutRef inOutRef <- tmatchField @"outRef" txininfo'
        inOutRefId <- tletField @"id" inOutRef
        pure $
          outRefId #== inOutRefId

mustPayToPubKey :: Term s (PPubKeyHash :--> PValue :--> PScriptContext :--> PBool)
mustPayToPubKey = plam $ \pk vl ctx ->
  ptraceIfFalse "mustPayToPubKey" $
    unTermCont $ do
      PScriptContext te <- tmatch ctx
      PTxInfo txinfo <- tmatchField @"txInfo" te
      outputs <- tletField @"outputs" txinfo
      (outputs' :: Term s (PList PTxOut)) <- tlet $ pmap # plam pfromData #$ pconvertLists # outputs
      pure $
        pany # (outputPaysTo # vl # pk) # outputs'

outputPaysTo :: Term s (PValue :--> PPubKeyHash :--> PTxOut :--> PBool)
outputPaysTo = plam $ \vl pkh txout -> unTermCont $ do
  PTxOut txout' <- tmatch txout
  PAddress adr <- tmatchField @"address" txout'
  pure $
    pmatch (pfromData $ pfield @"credential" # adr) $ \case
      PPubKeyCredential cred -> unTermCont $ do
        pkhOut <- tletField @"_0" cred
        vl' <- tletField @"value" txout'
        pure $
          (valueLTE # vl' # vl) #&& (pkhOut #== pkh)
      _ -> pcon PFalse

convertAC' :: Term s (PAssetClass :--> PAssetClass')
convertAC' = phoistAcyclic $
  plam $ \ac -> unTermCont $ do
    cs <- tletField @"currencySymbol" ac
    tn <- tletField @"tokenName" ac
    tcon $ PPair cs tn
