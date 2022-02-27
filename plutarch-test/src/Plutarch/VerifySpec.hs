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
import Plutarch.Builtin (
  pasInt,
  pforgetData,
  ppairDataBuiltin,
 )
import Plutarch.Prelude
import Plutarch.Verify (
  PTryFrom (ptryFrom),
  PTryUnwrapFrom (ptryUnwrapFrom),
  PDepth (PShallow, PDeep),
 )

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
          (pdata $ pconstant "foo")
        @-> pfails
      "(String, Integer) /= (String, String)"
        @| checkDeep
          @(PBuiltinPair (PAsData PInteger) (PAsData PByteString))
          @(PBuiltinPair (PAsData PByteString) (PAsData PByteString))
          (pdata $ ppairDataBuiltin # (pdata $ pconstant "foo") # (pdata $ pconstant "bar"))
        @-> pfails
      "[String] /= [Integer]"
        @| checkDeep
          @(PBuiltinList (PAsData PByteString))
          @(PBuiltinList (PAsData PInteger))
          (pdata $ (pcons # (pdata $ pconstant 3)) #$ (psingleton # (pdata $ pconstant 4)))
        @-> pfails
    "working" @\ do
      "int == int"
        @| checkShallow @PInteger @PInteger (pdata $ pconstant 42)
        @-> psucceeds
      "(String, String) == (String, String)"
        @| checkDeep
          @(PBuiltinPair (PAsData PByteString) (PAsData PByteString))
          @(PBuiltinPair (PAsData PByteString) (PAsData PByteString))
          (pdata $ ppairDataBuiltin # (pdata $ pconstant "foo") # (pdata $ pconstant "bar"))
        @-> psucceeds
      "[String] == [String]"
        @| checkDeep
          @(PBuiltinList (PAsData PByteString))
          @(PBuiltinList (PAsData PByteString))
          (pdata $ (pcons # (pdata $ pconstant "foo")) #$ (psingleton # (pdata $ pconstant "bar")))
        @-> psucceeds
      "A { test := String, test2 := Integer } == { test := String, test2 := Integer }"
        @| checkDeep 
          @(PDataRecord (("foo" ':= PInteger) ': ("bar" ':= PInteger) ': '[]))
          @(PDataRecord (("foo" ':= PInteger) ': ("bar" ':= PInteger) ': '[]))
          (pdata (pdcons @"foo" # (pdata $ pconstant 7)  #$ pdcons @"bar" # (pdata $ pconstant 42) # pdnil))
    "removing the data wrapper" @\ do 
      "erroneous" @\ do
        "(String, Integer) /= (String, String)"
          @| checkDeepUnwrap
            @(PAsData (PBuiltinPair (PAsData PByteString) (PAsData PByteString)))
            @(PBuiltinPair (PAsData PInteger) (PAsData PByteString))
            (pdata $ ppairDataBuiltin # (pdata $ pconstant "foo") # (pdata $ pconstant "bar"))
          @-> pfails 
        "[String] /= [Integer]"
          @| (checkDeepUnwrap 
            @(PAsData (PBuiltinList (PAsData PInteger)))
            @(PBuiltinList PByteString)
            (pdata $ (pcons # (pdata $ pconstant 3)) #$ (psingleton # (pdata $ pconstant 4))))
          @-> pfails
      "working" @\ do
        "(String, String) == (String, String)"
          @| (checkDeepUnwrap
            @(PAsData (PBuiltinPair (PAsData PByteString) (PAsData PByteString)))
            @(PBuiltinPair (PAsData PByteString) (PAsData PByteString))
            (pdata $ ppairDataBuiltin # (pdata $ pconstant "foo") # (pdata $ pconstant "bar")))
          @-> psucceeds
        "[String] == [String]"
          @| checkDeepUnwrap
            @(PAsData (PBuiltinList (PAsData PByteString)))
            @(PBuiltinList PByteString)
            (pdata $ (pcons # (pdata $ pconstant "foo")) #$ (psingleton # (pdata $ pconstant "bar"))) 
          @-> psucceeds 
      "partial checks" @\ do 
        -- this is way more expensive ...
        "check whole structure" 
          @| fullCheck @-> psucceeds
        -- ... than this
        "check structure partly"
          @| partialCheck @-> psucceeds
    "example" @\ do
      let validContext = ctx validList1
          invalidContext = ctx invalidList1
          l1 :: Term _ (PAsData (PBuiltinList (PAsData PInteger)))
          l1 = toDatadList [1 .. 5]
          l2 :: Term _ (PAsData (PBuiltinList (PAsData PInteger)))
          l2 = toDatadList [6 .. 10]
          l3 :: Term _ (PAsData (PBuiltinList (PAsData PInteger)))
          l3 = toDatadList [6..9]
          l4 :: Term _ (PAsData (PBuiltinList (PAsData PInteger)))
          l4 = toDatadList [6,8,8,9,10]
      "concatenate two lists, legal"
        @| validator # pforgetData l1 # pforgetData l2 # validContext @-> psucceeds
      "concatenate tow lists, illegal (list too short)"
        @| validator # pforgetData l1 # pforgetData l3 # validContext @-> pfails
      "concatenate tow lists, illegal (wrong elements in list)"
        @| validator # pforgetData l1 # pforgetData l4 # validContext @-> pfails
      "concatenate tow lists, illegal (more than one output)"
        @| validator # pforgetData l1 # pforgetData l2 # invalidContext @-> pfails

-- rec :: Term _ (PDataRecord (("foo" ':= PInteger) ': ("bar" ':= PInteger) ': '[]))
-- rec = 

checkDeepUnwrap :: 
  forall (actual :: PType) (target :: PType) b.
    ( PTryUnwrapFrom 'PDeep target
    , PAsData b ~ actual
    ) => 
    ClosedTerm actual -> 
    ClosedTerm target
checkDeepUnwrap t = ptryUnwrapFrom @'PDeep #$ pforgetData t


checkShallow ::
  forall (target :: PType) (actual :: PType) .
  ( PTryFrom 'PShallow PData (PAsData target)
  ) =>
  ClosedTerm (PAsData actual) ->
  ClosedTerm (PAsData target)
checkShallow  t = ptryFrom @'PShallow #$ pforgetData t

checkDeep ::
  forall (target :: PType) (actual :: PType).
  ( PTryFrom 'PDeep PData (PAsData target)
  , PIsData actual
  , PIsData target
  ) =>
  ClosedTerm (PAsData actual) ->
  ClosedTerm (PAsData target)
checkDeep t = ptryFrom @'PDeep #$ pforgetData t 


sampleStructure :: Term _ (PAsData (PBuiltinList (PAsData (PBuiltinList (PAsData (PBuiltinList (PAsData PInteger)))))))
sampleStructure = pdata $ psingleton #$ pdata $ psingleton #$ toDatadList [1..100]

-- | PData serves as the base case for recursing into the structure
partialCheck :: Term _ (PAsData (PBuiltinList (PAsData (PBuiltinList PData))))
partialCheck = let dat :: Term _ PData 
                   dat = pforgetData sampleStructure
                in ptryFrom @'PDeep #$ dat

fullCheck :: Term _ (PAsData (PBuiltinList (PAsData (PBuiltinList (PAsData (PBuiltinList (PAsData PInteger)))))))
fullCheck = ptryFrom @'PDeep #$ pforgetData sampleStructure


------------------- Example: untrusted Redeemer ------------------------------------


newtype PNatural (s :: S) = PMkNatural (Term s PInteger)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PNatural PInteger)

-- | partial
pmkNatural :: Term s (PInteger :--> PNatural)
pmkNatural = plam $ \i -> pif  (i #< 0) (ptraceError "could not make natural") (pcon $ PMkNatural i)

instance PTryFrom a PData (PAsData PNatural) where
  ptryFrom = plam $ \opq -> unTermCont $ do
    let i :: Term _ PInteger
        i = pasInt # opq
    pure $ pdata $ pmkNatural # i

validator :: Term s PValidator
validator = phoistAcyclic $
  plam $ \dat red ctx -> unTermCont $ do
    --                      untrusted ---^---^   ^--- trusted
    let trustedRedeemer :: Term _ (PBuiltinList (PAsData PNatural))
        trustedRedeemer = pfromData $ ptryFrom @'PDeep # red
        trustedDatum :: Term _ (PBuiltinList (PAsData PNatural))
        trustedDatum = pfromData $ ptryFrom @'PDeep # dat
        -- make the Datum and Redeemer trusted

        ownHash :: Term _ PDatumHash
        ownHash = unTermCont $ do
          PJust ownInput <- tcont $ pmatch $ pfindOwnInput # ctx
          let maybeHash :: Term _ (PMaybeData PDatumHash)
              maybeHash = pfield @"datumHash" #$ pfield @"resolved" #$ (pfromData ownInput)
          PDJust datumHash <- tcont $ pmatch maybeHash
          pure $ pfield @"_0" # datumHash
        -- find own script address matching DatumHash

        outputs :: Term _ (PBuiltinList (PAsData (PTuple PDatumHash PDatum)))
        outputs = pfield @"data" #$ pfield @"txInfo" # ctx
        -- find the list of the outputs

        matchingHashDatum :: Term _ (PBuiltinList PDatum)
        matchingHashDatum =
          precList
            ( \self x xs ->  pletFields @["_0", "_1"] x $ 
              \tup ->
                ptrace "iteration" $
                  pif
                    (hrecField @"_0" tup #== ownHash)
                    (ptrace "appended something" pcons # (hrecField @"_1" tup) # (self # xs))
                    (ptrace "called without appending" self # xs)
            )
            (const pnil)
            #$ outputs
        -- filter and map at the same time, as there is no efficient way
        -- to do that with tools available, I wrote it by hand

        singleOutput :: Term _ PDatum
        singleOutput = plet matchingHashDatum $ \dat ->
          pif (pnull #$ ptail # dat)
            (phead # dat)
            (ptraceError "not a single output")
        -- make sure that after filtering the outputs, only one output
        -- remains

        resultList :: Term _ (PAsData (PBuiltinList (PAsData PNatural)))
        resultList = pdata $ pconcat # trustedDatum # trustedRedeemer
        -- the resulting list with trusted datum and trusted redeemer

        isValid :: Term _ PBool
        isValid = (pto singleOutput) #== pforgetData resultList
    -- the final check for validity
    pure $
      pif isValid (popaque $ pcon PUnit) (ptraceError "not valid")

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

------------------- Mocking a transaction ------------------------------------------


ctx :: [(DatumHash, Datum)] -> Term s PScriptContext
ctx l = pconstant (ScriptContext (info' l) purpose)

info' :: [(DatumHash, Datum)] -> TxInfo
info' dat = info {txInfoData = dat}

validList1 :: [(DatumHash, Datum)]
validList1 =
  let dat :: Datum
      dat = Datum $ toBuiltinData [(1 :: Integer) .. 10]
   in [("d0", dat)] 

invalidList1 :: [(DatumHash, Datum)]
invalidList1 =
  let dat :: Datum
      dat = Datum $ toBuiltinData [(1 :: Integer) .. 10]
   in [("d0", dat), ("d0", Datum $ toBuiltinData @Integer 3)] 


------------------- Helpers --------------------------------------------------------


toDatadList :: [Integer] -> Term s (PAsData (PBuiltinList (PAsData PInteger)))
toDatadList = pdata . (foldr go pnil)
  where
    go :: Integer -> Term _ (PBuiltinList (PAsData PInteger)) -> Term _ (PBuiltinList (PAsData PInteger))
    go i acc = pcons # (pdata $ pconstant i) # acc
