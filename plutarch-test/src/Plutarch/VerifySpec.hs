{-# LANGUAGE UndecidableInstances #-}

module Plutarch.VerifySpec (spec) where

import Test.Syd

import Plutus.V1.Ledger.Api (
  Datum (Datum),
  DatumHash,
  ScriptContext (ScriptContext),
  ToData (toBuiltinData),
  TxInfo (txInfoData),
 )

import Plutarch
import Plutarch.Api.V1 (
  PDatum,
  PDatumHash,
  PMaybeData (PDJust),
  PScriptContext,
  PScriptPurpose (PSpending),
  PTuple,
  PTxInInfo,
  PTxOutRef,
  PValidator,
 )
import Plutarch.Bool (pif')
import Plutarch.Builtin (
  pasInt,
  pforgetData,
  ppairDataBuiltin,
 )
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeBuiltin)
import Plutarch.Verify (
  PTryFrom (ptryFrom),
  PTryFromRecur (ptryFromRecur),
 )
import qualified PlutusCore as PLC

import Plutarch.ApiSpec (info, purpose)
import Plutarch.Test

spec :: Spec
spec = do
  describe "verification_untrusted_data" . pgoldenSpec $ do
    "erroneous" @\ do
      "int /= bytestring"
        @| checkShallow
          @PInteger
          @PByteString
          (pconstant "foo")
        @-> pfails
      "(String, Integer) /= (String, String)"
        @| checkDeep
          @(PBuiltinPair (PAsData PInteger) (PAsData PByteString))
          @(PBuiltinPair (PAsData PByteString) (PAsData PByteString))
          (ppairDataBuiltin # (pdata $ pconstant "foo") # (pdata $ pconstant "bar"))
        @-> pfails
      "[String] /= [Integer]"
        @| checkDeep
          @(PBuiltinList (PAsData PByteString))
          @(PBuiltinList (PAsData PInteger))
          ((pcons # (pdata $ pconstant 3)) #$ (psingleton # (pdata $ pconstant 4)))
        @-> pfails
    "working" @\ do
      "int == int"
        @| checkShallow @PInteger @PInteger (pconstant 42)
        @-> psucceeds
      "(String, String) == (String, String)"
        @| checkDeep
          @(PBuiltinPair (PAsData PByteString) (PAsData PByteString))
          @(PBuiltinPair (PAsData PByteString) (PAsData PByteString))
          (ppairDataBuiltin # (pdata $ pconstant "foo") # (pdata $ pconstant "bar"))
        @-> psucceeds
      "(POpaque, POpaque) == (POpaque, POpaque)"
        @| checkShallow
          @(PBuiltinPair (PAsData POpaque) (PAsData POpaque))
          @(PBuiltinPair (PAsData POpaque) (PAsData POpaque))
          ( punsafeBuiltin PLC.MkPairData
              # (pdata $ pcon PUnit)
              # (pdata $ pcon PUnit)
          )
        @-> psucceeds
      "[String] == [String]"
        @| checkDeep
          @(PBuiltinList (PAsData PByteString))
          @(PBuiltinList (PAsData PByteString))
          ((pcons # (pdata $ pconstant "foo")) #$ (psingleton # (pdata $ pconstant "bar")))
        @-> psucceeds
    "example" @\ do
      let validContext = ctx validList1
          l1 :: Term _ (PAsData (PBuiltinList (PAsData PInteger)))
          l1 = toDatadList [1 .. 5]
          l2 :: Term _ (PAsData (PBuiltinList (PAsData PInteger)))
          l2 = toDatadList [6 .. 10]
      "concatenate two lists"
        @| validator # pforgetData l1 # pforgetData l2 # validContext @-> psucceeds

checkShallow ::
  forall (target :: PType) (actual :: PType).
  ( PTryFrom target
  , PIsData actual
  , PIsData target
  ) =>
  ClosedTerm actual ->
  ClosedTerm target
checkShallow = reprTargetActual ptryFrom

checkDeep ::
  forall (target :: PType) (actual :: PType).
  ( PTryFromRecur target
  , PIsData actual
  , PIsData target
  ) =>
  ClosedTerm actual ->
  ClosedTerm target
checkDeep = reprTargetActual (ptryFromRecur)

reprTargetActual ::
  forall (target :: PType) (actual :: PType).
  ( PIsData actual
  , PIsData target
  ) =>
  ClosedTerm (PData :--> PAsData target) ->
  ClosedTerm (actual) ->
  ClosedTerm (target)
reprTargetActual f x = pfromData $ f #$ pforgetData $ pdata x

-- example: untrusted redeemer

newtype PNatural (s :: S) = PMkNatural (Term s PInteger)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PNatural PInteger)

-- | partial
pmkNatural :: Term s (PInteger :--> PNatural)
pmkNatural = plam $ \i -> pif' # (i #< 0) # perror # (pcon $ PMkNatural i)

{- | total
 pmaybeMkNatural :: Term s (PInteger :--> PMaybe PNatural)
 pmaybeMkNatural = plam $ \i -> pif' # (i #< 0) # pcon PNothing # (pcon $ PJust $ pcon $ PMkNatural i)
-}
instance PTryFromRecur PNatural where
  ptryFromRecur = plam $ \opq -> unTermCont $ do
    let i :: Term _ PInteger
        i = pasInt # opq
    pure $ pdata $ pmkNatural # i

validator :: Term s PValidator
validator = phoistAcyclic $
  plam $ \dat red ctx -> unTermCont $ do
    --                      untrusted ---^---^   ^--- trusted
    let trustedRedeemer :: Term _ (PBuiltinList (PAsData PNatural))
        trustedRedeemer = pfromData $ ptryFromRecur # red
        trustedDatum :: Term _ (PBuiltinList (PAsData PNatural))
        trustedDatum = pfromData $ ptryFromRecur # dat
        -- make the Datm and Redeemer trusted

        ownHash :: Term _ PDatumHash
        ownHash = unTermCont $ do
          PJust ownInput <- tcont $ pmatch $ pfindOwnInput # ctx
          let maybeHash :: Term _ (PMaybeData PDatumHash)
              maybeHash = pfield @"datumHash" #$ pfield @"resolved" #$ (pfromData ownInput)
          PDJust datumHash <- tcont $ pmatch maybeHash
          pure $ pfield @"_0" # datumHash
        -- find own script adress matching DatumHash

        outputs :: Term _ (PBuiltinList (PAsData (PTuple PDatumHash PDatum)))
        outputs = pfield @"data" #$ pfield @"txInfo" # ctx
        -- find the list of the outputs

        matchingHashDatum :: Term _ (PBuiltinList PDatum)
        matchingHashDatum =
          precList
            ( \self x xs -> unTermCont $ do
                tup <- tcont $ pletFields @["_0", "_1"] x
                pure $
                  pif
                    (hrecField @"_0" tup #== ownHash)
                    (pcons # (hrecField @"_1" tup) # (self # xs))
                    (self # xs)
            )
            (const pnil)
            #$ outputs
        -- filter and map at the same time, as there is no efficient way
        -- to do that with tools available, I wrote it by hand

        singleOutput :: Term _ PDatum
        singleOutput =
          pif' # (pnull #$ ptail # matchingHashDatum)
            # (phead # matchingHashDatum)
            # perror
        -- make sure that after filtering the outputs, only one output
        -- remains

        resultList :: Term _ (PAsData (PBuiltinList (PAsData PNatural)))
        resultList = pdata $ pconcat # trustedDatum # trustedRedeemer
        -- the resulting list with trusted datum and trusted redeemer

        isValid :: Term _ PBool
        isValid = (pto singleOutput) #== pforgetData resultList
    -- the final check for validity
    pure $
      pif' # isValid # (popaque $ pcon PUnit) # perror

pfindOwnInput :: Term s (PScriptContext :--> PMaybe (PAsData PTxInInfo))
pfindOwnInput = phoistAcyclic $
  plam $ \ctx' -> unTermCont $ do
    ctx <- tcont $ pletFields @["txInfo", "purpose"] ctx'
    PSpending txoutRef <- tcont $ pmatch $ hrecField @"purpose" ctx
    let txInInfos :: Term _ (PBuiltinList (PAsData PTxInInfo))
        txInInfos = pfield @"inputs" #$ hrecField @"txInfo" ctx
        target :: Term _ PTxOutRef
        target = pfield @"_0" # txoutRef
        pred :: Term _ (PAsData PTxInInfo :--> PBool)
        pred = plam $ \actual ->
          (pfield @"id" # target) #== (pfield @"id" #$ pfield @"outRef" # pfromData actual)
    pure $ pfind # pred # txInInfos

{- |
    can be safely removed after
    https://github.com/Plutonomicon/plutarch/pull/274
    has been merged
-}
pfind :: (PIsListLike l a) => Term s ((a :--> PBool) :--> l a :--> PMaybe a)
pfind = phoistAcyclic $
  pfix #$ plam $ \self f xs ->
    pelimList
      ( \y ys ->
          pif
            (f # y)
            (pcon $ PJust y)
            (self # f # ys)
      )
      (pcon PNothing)
      xs

-- Mocking a transaction

ctx :: [(DatumHash, Datum)] -> Term s PScriptContext
ctx l = pconstant (ScriptContext (info' l) purpose)

info' :: [(DatumHash, Datum)] -> TxInfo
info' dat = info {txInfoData = dat}

validList1 :: [(DatumHash, Datum)]
validList1 =
  let dat :: Datum
      dat = Datum $ toBuiltinData [(1 :: Integer) .. 10]
   in [("d0", dat)]

toDatadList :: [Integer] -> Term s (PAsData (PBuiltinList (PAsData PInteger)))
toDatadList = pdata . (foldr go pnil)
  where
    go :: Integer -> Term _ (PBuiltinList (PAsData PInteger)) -> Term _ (PBuiltinList (PAsData PInteger))
    go i acc = pcons # (pdata $ pconstant i) # acc
