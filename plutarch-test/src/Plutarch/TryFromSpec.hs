{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.TryFromSpec (spec) where

import Test.Syd

import qualified GHC.Generics as GHC

import Generics.SOP (Generic, I (I))

import Plutus.V1.Ledger.Api (
  Address (Address),
  Credential (ScriptCredential),
  Datum (Datum),
  DatumHash,
  ScriptContext (ScriptContext),
  ToData (toBuiltinData),
  TxInfo (txInfoData, txInfoOutputs),
  TxOut (TxOut, txOutAddress, txOutDatumHash, txOutValue),
 )

import PlutusTx (
  Data (B, Constr, I),
 )

import PlutusTx.AssocMap (Map)
import qualified PlutusTx.AssocMap as PlutusMap

import Plutarch.Unsafe (
  punsafeCoerce,
 )

import Plutarch
import Plutarch.Api.V1 (
  PAddress,
  PCurrencySymbol,
  PDatum,
  PDatumHash,
  PMap,
  PMaybeData (PDJust),
  PScriptContext,
  PScriptPurpose (PSpending),
  PTokenName,
  PTuple,
  PTxInInfo,
  PTxInfo,
  PTxOut,
  PTxOutRef,
  PValidator,
  PValue,
 )
import Plutarch.Builtin (
  PBuiltinMap,
  pforgetData,
  ppairDataBuiltin,
 )
import Plutarch.Prelude
import Plutarch.TryFrom (
  Flip (Flip, unFlip),
  PTryFrom (PTryFromExcess, ptryFrom),
 )

import Plutarch.ApiSpec (info, purpose)
import qualified Plutarch.ApiSpec as Api
import Plutarch.DataRepr (PIsDataReprInstances (PIsDataReprInstances))
import Plutarch.Test
import Plutus.V1.Ledger.Value (Value)
import qualified Plutus.V1.Ledger.Value as Value

spec :: Spec
spec = do
  describe "verification_untrusted_data" . plutarchDevFlagDescribe . pgoldenSpec $ do
    "erroneous" @\ do
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
      "A { test := Integer, test2 := Integer } /= { test := String, test2 := Integer }"
        @| checkDeep
          @(PDataRecord (("foo" ':= PInteger) ': ("bar" ':= PInteger) ': '[]))
          @(PDataRecord (("foo" ':= PByteString) ': ("bar" ':= PInteger) ': '[]))
          (pdata (pdcons @"foo" # (pdata $ pconstant "baz") #$ pdcons @"bar" # (pdata $ pconstant 42) # pdnil))
        @-> pfails
      "Map Int String /= Map Int Int"
        @| mapTestFails @-> pfails
      "PDataSum constr 2"
        @| checkDeep
          @(PDataSum '[ '["i1" ':= PInteger, "b2" ':= PByteString]])
          @(PDataSum '[ '["i1" ':= PInteger, "b2" ':= PByteString], '["i3" ':= PInteger, "b4" ':= PByteString]])
          (punsafeCoerce $ pconstant $ Constr 1 [PlutusTx.I 5, B "foo"])
          @-> pfails
      "PDataSum wrong record type"
        @| checkDeep
          @(PDataSum '[ '["i1" ':= PInteger, "b2" ':= PByteString], '["i3" ':= PByteString, "b4" ':= PByteString]])
          @(PDataSum '[ '["i1" ':= PInteger, "b2" ':= PByteString], '["i3" ':= PInteger, "b4" ':= PByteString]])
          (punsafeCoerce $ pconstant $ Constr 2 [PlutusTx.I 5, B "foo"])
          @-> pfails
    "working" @\ do
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
      "A { test := Integer, test2 := Integer } == { test := Integer, test2 := Integer }"
        @| checkDeep
          @(PDataRecord (("foo" ':= PInteger) ': ("bar" ':= PInteger) ': '[]))
          @(PDataRecord (("foo" ':= PInteger) ': ("bar" ':= PInteger) ': '[]))
          (pdata (pdcons @"foo" # (pdata $ pconstant 7) #$ pdcons @"bar" # (pdata $ pconstant 42) # pdnil))
        @-> psucceeds
      "A { test := Integer, test2 := Integer } == [Integer]"
        @| checkDeep
          @(PDataRecord (("foo" ':= PInteger) ': ("bar" ':= PInteger) ': '[]))
          @(PBuiltinList (PAsData PInteger))
          (pdata (pcons # (pdata $ pconstant 7) #$ pcons # (pdata $ pconstant 42) # pnil))
        @-> psucceeds
      "A { test := String, test2 := Integer } == { test := String, test2 := Integer }"
        @| checkDeep
          @(PDataRecord (("foo" ':= PByteString) ': ("bar" ':= PInteger) ': '[]))
          @(PDataRecord (("foo" ':= PByteString) ': ("bar" ':= PInteger) ': '[]))
          (pdata (pdcons @"foo" # (pdata $ pconstant "baz") #$ pdcons @"bar" # (pdata $ pconstant 42) # pdnil))
        @-> psucceeds
      "Map Int String == Map Int String"
        @| mapTestSucceeds @-> psucceeds
      "PDataSum constr 0"
        @| checkDeep
          @(PDataSum '[ '["i1" ':= PInteger, "b2" ':= PByteString], '["i3" ':= PInteger, "b4" ':= PByteString]])
          @(PDataSum '[ '["i1" ':= PInteger, "b2" ':= PByteString], '["i3" ':= PInteger, "b4" ':= PByteString]])
          (punsafeCoerce $ pconstant $ Constr 0 [PlutusTx.I 5, B "foo"])
        @-> psucceeds
      "PDataSum constr 1"
        @| checkDeep
          @(PDataSum '[ '["i1" ':= PInteger, "b2" ':= PByteString], '["i3" ':= PInteger, "b4" ':= PByteString]])
          @(PDataSum '[ '["i1" ':= PInteger, "b2" ':= PByteString], '["i3" ':= PInteger, "b4" ':= PByteString]])
          (punsafeCoerce $ pconstant $ Constr 1 [PlutusTx.I 5, B "foo"])
        @-> psucceeds
    "recovering a record partially vs completely" @\ do
      "partially"
        @| checkDeep
          @(PDataRecord '["foo" ':= PInteger, "bar" ':= PData])
          @(PDataRecord '["foo" ':= PInteger, "bar" ':= PByteString])
          (pdata $ pdcons @"foo" # (pdata $ pconstant 3) #$ pdcons @"bar" # (pdata $ pconstant "baz") # pdnil)
          @-> psucceeds
      "completely"
        @| checkDeep
          @(PDataRecord '["foo" ':= PInteger, "bar" ':= PByteString])
          @(PDataRecord '["foo" ':= PInteger, "bar" ':= PByteString])
          (pdata (pdcons @"foo" # (pdata $ pconstant 3) #$ pdcons @"bar" # (pdata $ pconstant "baz") # pdnil))
          @-> psucceeds
    "removing the data wrapper" @\ do
      "erroneous" @\ do
        "(String, Integer) /= (String, String)"
          @| checkDeepUnwrap
            @(PBuiltinPair (PAsData PByteString) (PAsData PByteString))
            @(PBuiltinPair (PAsData PInteger) (PAsData PByteString))
            (pdata $ ppairDataBuiltin # (pdata $ pconstant 42) # (pdata $ pconstant "bar"))
          @-> pfails
        "[String] /= [Integer]"
          @| ( checkDeepUnwrap
                @(PBuiltinList (PAsData PInteger))
                @(PBuiltinList (PAsData PByteString))
                (pdata $ (pcons # (pdata $ pconstant "foo")) #$ (psingleton # (pdata $ pconstant "baz")))
             )
          @-> pfails
      "working" @\ do
        "(String, String) == (String, String)"
          @| ( checkDeepUnwrap
                @(PBuiltinPair (PAsData PByteString) (PAsData PByteString))
                @(PBuiltinPair (PAsData PByteString) (PAsData PByteString))
                (pdata $ ppairDataBuiltin # (pdata $ pconstant "foo") # (pdata $ pconstant "bar"))
             )
          @-> psucceeds
        "[String] == [String]"
          @| checkDeepUnwrap
            @(PBuiltinList (PAsData PByteString))
            @(PBuiltinList (PAsData PByteString))
            (pdata $ (pcons # (pdata $ pconstant "foo")) #$ (psingleton # (pdata $ pconstant "bar")))
          @-> psucceeds
      "partial checks" @\ do
        -- this is way more expensive ...
        "check whole structure"
          @| fullCheck @-> psucceeds
        -- ... than this
        "check structure partly"
          @| partialCheck @-> psucceeds
    "checking PValue and PMap for validity" @\ do
      "PMap" @\ do
        let ms0 :: Term _ (PBuiltinMap PInteger PUnit)
            ms0 =
              pcons
                # (ppairDataBuiltin # (pdata $ pconstant 1) # (pdata $ pcon PUnit)) #$ pcons
                # (ppairDataBuiltin # (pdata $ pconstant 2) # (pdata $ pcon PUnit)) #$ pcons
                # (ppairDataBuiltin # (pdata $ pconstant 42) # (pdata $ pcon PUnit))
                # pnil
            mf1 :: Term _ (PBuiltinMap PInteger PUnit)
            mf1 =
              pcons
                # (ppairDataBuiltin # (pdata $ pconstant 1) # (pdata $ pcon PUnit)) #$ pcons
                # (ppairDataBuiltin # (pdata $ pconstant 1) # (pdata $ pcon PUnit)) #$ pcons
                # (ppairDataBuiltin # (pdata $ pconstant 42) # (pdata $ pcon PUnit))
                # pnil
            mf2 :: Term _ (PBuiltinMap PInteger PUnit)
            mf2 =
              pcons
                # (ppairDataBuiltin # (pdata $ pconstant 1) # (pdata $ pcon PUnit)) #$ pcons
                # (ppairDataBuiltin # (pdata $ pconstant 2) # (pdata $ pcon PUnit)) #$ pcons
                # (ppairDataBuiltin # (pdata $ pconstant 3) # (pdata $ pcon PUnit)) #$ pcons
                # (ppairDataBuiltin # (pdata $ pconstant 2) # (pdata $ pcon PUnit))
                # pnil
        "valid0"
          @| (unTermCont $ fst <$> TermCont (ptryFrom @_ @(PMap PInteger PUnit) ms0)) @-> psucceeds
        "invalid1"
          @| (unTermCont $ fst <$> TermCont (ptryFrom @_ @(PMap PInteger PUnit) mf1)) @-> pfails
        "invalid2"
          @| (unTermCont $ fst <$> TermCont (ptryFrom @_ @(PMap PInteger PUnit) mf2)) @-> pfails
      "PValue" @\ do
        let legalValue0 :: Value
            legalValue0 = Value.singleton "c0" "someToken" 1
            illegalValue1 :: Map Value.CurrencySymbol (Map Value.TokenName Integer)
            illegalValue1 = PlutusMap.fromList [("c0", PlutusMap.fromList [("someToken", 1), ("someOtherToken", 0)])]
        "valid0"
          @| (unTermCont $ fst <$> TermCont (ptryFrom @(PMap PCurrencySymbol (PMap PTokenName PInteger)) @PValue $ punsafeCoerce $ pconstant $ legalValue0)) @-> psucceeds
        "invalid1"
          @| (unTermCont $ fst <$> TermCont (ptryFrom @(PMap PCurrencySymbol (PMap PTokenName PInteger)) @PValue $ pconstant $ illegalValue1)) @-> pfails
    "example" @\ do
      let validContext0 = ctx validOutputs0 validList1
          invalidContext1 = ctx invalidOutputs1 validList1
          l1 :: Term _ (PAsData (PBuiltinList (PAsData PInteger)))
          l1 = toDatadList [1 .. 5]
          l2 :: Term _ (PAsData (PBuiltinList (PAsData PInteger)))
          l2 = toDatadList [6 .. 10]
          l3 :: Term _ (PAsData (PBuiltinList (PAsData PInteger)))
          l3 = toDatadList [6 .. 9]
          l4 :: Term _ (PAsData (PBuiltinList (PAsData PInteger)))
          l4 = toDatadList [6, 8, 8, 9, 10]
      "concatenate two lists, legal"
        @| validator # pforgetData l1 # pforgetData l2 # validContext0 @-> psucceeds
      "concatenate two lists, illegal (list too short)"
        @| validator # pforgetData l1 # pforgetData l3 # validContext0 @-> pfails
      "concatenate two lists, illegal (wrong elements in list)"
        @| validator # pforgetData l1 # pforgetData l4 # validContext0 @-> pfails
      "concatenate two lists, illegal (more than one output)"
        @| validator # pforgetData l1 # pforgetData l2 # invalidContext1 @-> pfails

------------------- Checking deeply, shallowly and unwrapping ----------------------

checkDeep ::
  forall (target :: PType) (actual :: PType).
  ( PTryFrom PData (PAsData target)
  , PIsData actual
  , PIsData target
  ) =>
  ClosedTerm (PAsData actual) ->
  ClosedTerm (PAsData target)
checkDeep t = unTermCont $ fst <$> TermCont (ptryFrom $ pforgetData t)

checkDeepUnwrap ::
  forall (target :: PType) (actual :: PType).
  ( PTryFrom PData (PAsData target)
  , PIsData actual
  , PIsData target
  , PTryFromExcess PData (PAsData target) ~ Flip Term target
  ) =>
  ClosedTerm (PAsData actual) ->
  ClosedTerm (PAsData target)
checkDeepUnwrap t = unTermCont $ fst <$> TermCont (ptryFrom @PData @(PAsData target) $ pforgetData t)

sampleStructure :: Term _ (PAsData (PBuiltinList (PAsData (PBuiltinList (PAsData (PBuiltinList (PAsData PInteger)))))))
sampleStructure = pdata $ psingleton #$ pdata $ psingleton #$ toDatadList [1 .. 100]

-- | PData serves as the base case for recursing into the structure
partialCheck :: Term _ (PAsData (PBuiltinList (PAsData (PBuiltinList PData))))
partialCheck =
  let dat :: Term _ PData
      dat = pforgetData sampleStructure
   in unTermCont $ fst <$> TermCont (ptryFrom dat)

fullCheck :: Term _ (PAsData (PBuiltinList (PAsData (PBuiltinList (PAsData (PBuiltinList (PAsData PInteger)))))))
fullCheck = unTermCont $ fst <$> TermCont (ptryFrom $ pforgetData sampleStructure)

------------------- Example: untrusted Redeemer ------------------------------------

newtype PNatural (s :: S) = PMkNatural (Term s PInteger)
  deriving (PlutusType, PIsData, PEq, POrd) via (DerivePNewtype PNatural PInteger)

-- | partial
pmkNatural :: Term s (PInteger :--> PNatural)
pmkNatural = plam $ \i -> pif (i #< 0) (ptraceError "could not make natural") (pcon $ PMkNatural i)

instance PTryFrom PData (PAsData PNatural) where
  type PTryFromExcess PData (PAsData PNatural) = Flip Term PNatural
  ptryFrom opq = runTermCont $ do
    tup <- TermCont $ ptryFrom @PData @(PAsData PInteger) opq
    ver <- tcont $ plet $ pmkNatural # unFlip (snd tup)
    pure $ (punsafeCoerce (fst tup), Flip ver)

validator :: Term s PValidator
validator = phoistAcyclic $
  plam $ \dat red ctx -> unTermCont $ do
    trustedRedeemer <- (unFlip . snd) <$> (TermCont $ ptryFrom @PData @(PAsData (PBuiltinList (PAsData PNatural))) red)
    let trustedDatum :: Term _ (PBuiltinList (PAsData PNatural))
        trustedDatum = pfromData $ punsafeCoerce dat
    -- make the Datum and Redeemer trusted

    txInfo :: (Term _ PTxInfo) <- tcont $ plet $ pfield @"txInfo" # ctx

    PJust ownInput <- tcont $ pmatch $ pfindOwnInput # ctx
    resolved <- tcont $ pletFields @["address", "datumHash"] $ pfield @"resolved" # ownInput

    let ownAddress :: Term _ PAddress
        ownAddress = resolved.address
        -- find own script address matching DatumHash

        ownHash :: Term _ PDatumHash
        ownHash = unTermCont $ do
          PDJust dhash <- tcont $ pmatch resolved.datumHash
          pure $ pfield @"_0" # dhash

        data' :: Term _ (PBuiltinList (PAsData (PTuple PDatumHash PDatum)))
        data' = pfield @"data" # txInfo

        outputs :: Term _ (PBuiltinList (PAsData PTxOut))
        outputs = pfield @"outputs" # txInfo
        -- find the list of the outputs

        matchingHashDatum :: Term _ (PBuiltinList PDatum)
        matchingHashDatum =
          precList
            ( \self x xs -> pletFields @["_0", "_1"] x $
                \tup ->
                  ptrace "iteration" $
                    pif
                      (tup._0 #== ownHash)
                      (ptrace "appended something" pcons # (tup._1) # (self # xs))
                      (ptrace "called without appending" self # xs)
            )
            (const pnil)
            #$ data'
        -- filter and map at the same time, as there is no efficient way
        -- to do that with tools available, I wrote it by hand

        singleOutput :: Term _ PBool
        singleOutput = pnull #$ ptail #$ pfilter # pred # outputs
          where
            pred :: Term _ (PAsData PTxOut :--> PBool)
            pred = plam $ \out -> unTermCont $ do
              pure $ pfield @"address" # out #== (pdata $ ownAddress)

        -- make sure that after filtering the outputs, only one output
        -- remains

        resultList :: Term _ (PAsData (PBuiltinList (PAsData PNatural)))
        resultList = pdata $ pconcat # trustedDatum # trustedRedeemer
        -- the resulting list with trusted datum and trusted redeemer

        isValid :: Term _ PBool
        isValid = pif singleOutput (pto (phead # matchingHashDatum) #== pforgetData resultList) (pcon PFalse)
    -- the final check for validity
    pure $
      pif isValid (popaque $ pcon PUnit) (ptraceError "not valid")

pfindOwnInput :: Term s (PScriptContext :--> PMaybe (PAsData PTxInInfo))
pfindOwnInput = phoistAcyclic $
  plam $ \ctx' -> unTermCont $ do
    ctx <- tcont $ pletFields @["txInfo", "purpose"] ctx'
    PSpending txoutRef <- tcont $ pmatch $ ctx.purpose
    let txInInfos :: Term _ (PBuiltinList (PAsData PTxInInfo))
        txInInfos = pfield @"inputs" #$ ctx.txInfo
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

------------------- Mocking a ScriptContext ----------------------------------------

ctx :: [TxOut] -> [(DatumHash, Datum)] -> Term s PScriptContext
ctx outs l = pconstant (ScriptContext (info' outs l) purpose)

info' :: [TxOut] -> [(DatumHash, Datum)] -> TxInfo
info' outs dat =
  info
    { txInfoData = dat
    , txInfoOutputs = outs
    }

validOutputs0 :: [TxOut]
validOutputs0 =
  [ TxOut
      { txOutAddress =
          Address (ScriptCredential Api.validator) Nothing
      , txOutValue = mempty
      , txOutDatumHash = Just Api.datum
      }
  ]

invalidOutputs1 :: [TxOut]
invalidOutputs1 =
  [ TxOut
      { txOutAddress =
          Address (ScriptCredential Api.validator) Nothing
      , txOutValue = mempty
      , txOutDatumHash = Just Api.datum
      }
  , TxOut
      { txOutAddress =
          Address (ScriptCredential Api.validator) Nothing
      , txOutValue = mempty
      , txOutDatumHash = Nothing
      }
  ]

validList1 :: [(DatumHash, Datum)]
validList1 =
  let dat :: Datum
      dat = Datum $ toBuiltinData [(1 :: Integer) .. 10]
   in [("d0", dat)]

------------------- Helpers --------------------------------------------------------

toDatadList :: [Integer] -> Term s (PAsData (PBuiltinList (PAsData PInteger)))
toDatadList = pdata . (foldr go pnil)
  where
    go :: Integer -> Term _ (PBuiltinList (PAsData PInteger)) -> Term _ (PBuiltinList (PAsData PInteger))
    go i acc = pcons # (pdata $ pconstant i) # acc

------------------- Special cases for maps -----------------------------------------

mapTestSucceeds :: ClosedTerm (PAsData (PBuiltinMap PByteString PInteger))
mapTestSucceeds = unTermCont $ do
  (val, _) <- TermCont $ ptryFrom @PData $ pforgetData sampleMap
  pure val

mapTestFails :: ClosedTerm (PAsData (PBuiltinMap PInteger PInteger))
mapTestFails = unTermCont $ do
  (val, _) <- TermCont $ ptryFrom @PData $ pforgetData sampleMap
  pure val

sampleMap :: Term _ (PAsData (PBuiltinMap PByteString PInteger))
sampleMap =
  pdata $
    pcons
      # (ppairDataBuiltin # (pdata $ pconstant "foo") # (pdata $ pconstant 42)) #$ pcons
      # (ppairDataBuiltin # (pdata $ pconstant "bar") # (pdata $ pconstant 41))
      # pnil

{-
------------------- Sample type with PIsDataRepr -----------------------------------

sampleAB :: Term s (PAsData PAB)
sampleAB = pdata $ pcon $ PA (pdcons @"_0" # (pdata $ pconstant 4) #$ pdcons # (pdata $ pconstant "foo") # pdnil)

sampleABdata :: Term s PData
sampleABdata = pforgetData sampleAB

recoverAB :: Term s (PAsData PAB)
recoverAB = unTermCont $ do
  ver <- fst <$> TermCont (ptryFrom sampleABdata)

data PAB (s::S)
  = PA  (Term s (PDataRecord '["_0" ':= PInteger, "_1" ':= PByteString] ))
  | PB  (Term s (PDataRecord '["_0" ':= PBuiltinList (PAsData PInteger), "_1" ':= PByteString] ))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PIsDataRepr)
  deriving (PlutusType, PIsData)
  via PIsDataReprInstances PAB
-}
