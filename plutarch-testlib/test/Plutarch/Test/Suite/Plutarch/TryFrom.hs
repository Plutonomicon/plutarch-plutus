{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Test.Suite.Plutarch.TryFrom (tests) where

import Plutarch.Builtin (
  pforgetData,
  ppairDataBuiltin,
 )
import Plutarch.Prelude
import Plutarch.Reducible (Reduce)
import Plutarch.Test.Golden (goldenEval, goldenEvalFail, goldenGroup, plutarchGolden)
import Plutarch.TryFrom (
  PTryFromExcess,
  ptryFrom',
 )
import Plutarch.Unsafe (
  punsafeCoerce,
  punsafeDowncast,
 )
import PlutusTx (
  Data (B, Constr, I),
 )
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "TryFrom"
    [ plutarchGolden
        "Goldens"
        "data-verif"
        [ goldenGroup
            "erroneous"
            [ goldenEvalFail
                "(String, Integer) /= (String, String)"
                ( checkDeep
                    @(PBuiltinPair (PAsData PInteger) (PAsData PByteString))
                    @(PBuiltinPair (PAsData PByteString) (PAsData PByteString))
                    (pdata $ ppairDataBuiltin # pdata (pconstant "foo") # pdata (pconstant "bar"))
                )
            , goldenEvalFail
                "[String] /= [Integer]"
                ( checkDeep
                    @(PBuiltinList (PAsData PByteString))
                    @(PBuiltinList (PAsData PInteger))
                    (pdata $ (pcons # pdata (pconstant 3)) #$ (psingleton # pdata (pconstant 4)))
                )
            , goldenEvalFail
                "A { test := Integer, test2 := Integer } /= { test := String, test2 := Integer }"
                ( checkDeep
                    @(PDataRecord (("foo" ':= PInteger) ': ("bar" ':= PInteger) ': '[]))
                    @(PDataRecord (("foo" ':= PByteString) ': ("bar" ':= PInteger) ': '[]))
                    (pdata (pdcons @"foo" # pdata (pconstant "baz") #$ pdcons @"bar" # pdata (pconstant 42) # pdnil))
                )
            , goldenEvalFail
                "PDataSum constr 2"
                ( checkDeep
                    @(PDataSum '[ '["i1" ':= PInteger, "b2" ':= PByteString]])
                    @(PDataSum '[ '["i1" ':= PInteger, "b2" ':= PByteString], '["i3" ':= PInteger, "b4" ':= PByteString]])
                    (punsafeCoerce $ pconstant $ Constr 1 [PlutusTx.I 5, B "foo"])
                )
            , goldenEvalFail
                "PDataSum wrong record type"
                ( checkDeep
                    @(PDataSum '[ '["i1" ':= PInteger, "b2" ':= PByteString], '["i3" ':= PByteString, "b4" ':= PByteString]])
                    @(PDataSum '[ '["i1" ':= PInteger, "b2" ':= PByteString], '["i3" ':= PInteger, "b4" ':= PByteString]])
                    (punsafeCoerce $ pconstant $ Constr 2 [PlutusTx.I 5, B "foo"])
                )
            , goldenEvalFail
                "[ByteString] (with length == 2) /= PRational"
                ( checkDeep
                    @PRational
                    @(PBuiltinList (PAsData PByteString))
                    (pdata $ pcons # pdata (phexByteStr "41") #$ pcons # pdata (phexByteStr "2b") # pnil)
                )
            , goldenEvalFail
                "[Integer] (with length == 0) /= PRational"
                ( checkDeep
                    @PRational
                    @(PBuiltinList (PAsData PInteger))
                    (pdata pnil)
                )
            , goldenEvalFail
                "[Integer] (with length == 3) /= PRational"
                ( checkDeep
                    @PRational
                    @(PBuiltinList (PAsData PInteger))
                    (pdata $ pcons # pconstantData 42 #$ pcons # pconstantData 7 #$ pcons # pconstantData 0 # pnil)
                )
            , goldenEvalFail
                "[Integer] (with length == 2, with 0 denominator) /= PRational"
                ( checkDeep
                    @PRational
                    @(PBuiltinList (PAsData PInteger))
                    (pdata $ pcons # pconstantData 42 #$ pcons # pconstantData 0 # pnil)
                )
            ]
        , goldenGroup
            "working"
            [ goldenEval
                "(String, String) == (String, String)"
                ( checkDeep
                    @(PBuiltinPair (PAsData PByteString) (PAsData PByteString))
                    @(PBuiltinPair (PAsData PByteString) (PAsData PByteString))
                    (pdata $ ppairDataBuiltin # pdata (pconstant "foo") # pdata (pconstant "bar"))
                )
            , goldenEval
                "[String] == [String]"
                ( checkDeep
                    @(PBuiltinList (PAsData PByteString))
                    @(PBuiltinList (PAsData PByteString))
                    (pdata $ (pcons # pdata (pconstant "foo")) #$ (psingleton # pdata (pconstant "bar")))
                )
            , goldenEval
                "[Integer] (with length == 2) == PRational"
                ( unTermCont
                    ( do
                        let numr = pconstantData 42
                        let denm = pconstantData 31
                        (drat, nz) <-
                          checkDeep' @PRational @(PBuiltinList (PAsData PInteger))
                            (pdata $ pcons # numr #$ pcons # denm # pnil)
                        pguardC "non-zero should be as expected" $ pto nz #== pfromData denm
                        pguardC "drat should be as expected" $ pfromData drat #== pcon (PRational (pfromData numr) nz)
                        pure $ pconstant ()
                    )
                )
            , goldenEval
                "A { test := Integer, test2 := Integer } == { test := Integer, test2 := Integer }"
                ( checkDeep
                    @(PDataRecord (("foo" ':= PInteger) ': ("bar" ':= PInteger) ': '[]))
                    @(PDataRecord (("foo" ':= PInteger) ': ("bar" ':= PInteger) ': '[]))
                    (pdata (pdcons @"foo" # pdata (pconstant 7) #$ pdcons @"bar" # pdata (pconstant 42) # pdnil))
                )
            , goldenEval
                "A { test := Integer, test2 := Integer } == [Integer]"
                ( checkDeep
                    @(PDataRecord (("foo" ':= PInteger) ': ("bar" ':= PInteger) ': '[]))
                    @(PBuiltinList (PAsData PInteger))
                    (pdata (pcons # pdata (pconstant 7) #$ pcons # pdata (pconstant 42) # pnil))
                )
            , goldenEval
                "A { test := String, test2 := Integer } == { test := String, test2 := Integer }"
                ( checkDeep
                    @(PDataRecord (("foo" ':= PByteString) ': ("bar" ':= PInteger) ': '[]))
                    @(PDataRecord (("foo" ':= PByteString) ': ("bar" ':= PInteger) ': '[]))
                    (pdata (pdcons @"foo" # pdata (pconstant "baz") #$ pdcons @"bar" # pdata (pconstant 42) # pdnil))
                )
            , goldenEval
                "PDataSum constr 0"
                ( checkDeep
                    @(PDataSum '[ '["i1" ':= PInteger, "b2" ':= PByteString], '["i3" ':= PInteger, "b4" ':= PByteString]])
                    @(PDataSum '[ '["i1" ':= PInteger, "b2" ':= PByteString], '["i3" ':= PInteger, "b4" ':= PByteString]])
                    (punsafeCoerce $ pconstant $ Constr 0 [PlutusTx.I 5, B "foo"])
                )
            , goldenEval
                "PDataSum constr 1"
                ( checkDeep
                    @(PDataSum '[ '["i1" ':= PInteger, "b2" ':= PByteString], '["i3" ':= PInteger, "b4" ':= PByteString]])
                    @(PDataSum '[ '["i1" ':= PInteger, "b2" ':= PByteString], '["i3" ':= PInteger, "b4" ':= PByteString]])
                    (punsafeCoerce $ pconstant $ Constr 1 [PlutusTx.I 5, B "foo"])
                )
            , goldenEval
                "recover PWrapInt"
                ( pconstant 42
                    #== unTermCont (snd <$> tcont (ptryFrom @(PAsData PWrapInt) (pforgetData $ pdata $ pconstant @PInteger 42)))
                )
            ]
        , goldenGroup
            "recovering a record partially vs completely"
            [ goldenEval
                "partially"
                ( checkDeep
                    @(PDataRecord '["foo" ':= PInteger, "bar" ':= PData])
                    @(PDataRecord '["foo" ':= PInteger, "bar" ':= PByteString])
                    (pdata $ pdcons @"foo" # pdata (pconstant 3) #$ pdcons @"bar" # pdata (pconstant "baz") # pdnil)
                )
            , goldenEval
                "completely"
                ( checkDeep
                    @(PDataRecord '["foo" ':= PInteger, "bar" ':= PByteString])
                    @(PDataRecord '["foo" ':= PInteger, "bar" ':= PByteString])
                    (pdata (pdcons @"foo" # pdata (pconstant 3) #$ pdcons @"bar" # pdata (pconstant "baz") # pdnil))
                )
            ]
        , goldenGroup
            "removing the data wrapper"
            [ goldenGroup
                "erroneous"
                [ goldenEvalFail
                    "(String, Integer) /= (String, String)"
                    ( checkDeepUnwrap
                        @(PBuiltinPair (PAsData PByteString) (PAsData PByteString))
                        @(PBuiltinPair (PAsData PInteger) (PAsData PByteString))
                        (pdata $ ppairDataBuiltin # pdata (pconstant 42) # pdata (pconstant "bar"))
                    )
                , goldenEvalFail
                    "[String] /= [Integer]"
                    ( checkDeepUnwrap
                        @(PBuiltinList (PAsData PInteger))
                        @(PBuiltinList (PAsData PByteString))
                        (pdata $ (pcons # pdata (pconstant "foo")) #$ (psingleton # pdata (pconstant "baz")))
                    )
                ]
            , goldenGroup
                "working"
                [ goldenEval
                    "(String, String) == (String, String)"
                    ( checkDeepUnwrap
                        @(PBuiltinPair (PAsData PByteString) (PAsData PByteString))
                        @(PBuiltinPair (PAsData PByteString) (PAsData PByteString))
                        (pdata $ ppairDataBuiltin # pdata (pconstant "foo") # pdata (pconstant "bar"))
                    )
                , goldenEval
                    "[String] == [String]"
                    ( checkDeepUnwrap
                        @(PBuiltinList (PAsData PByteString))
                        @(PBuiltinList (PAsData PByteString))
                        (pdata $ (pcons # pdata (pconstant "foo")) #$ (psingleton # pdata (pconstant "bar")))
                    )
                ]
            , goldenGroup
                "partial checks"
                [ -- this is way more expensive ...
                  goldenEval "check whole structure" fullCheck
                , -- ... than this
                  goldenEval "check structure partly" partialCheck
                ]
            , goldenGroup
                "recovering a nested record"
                [ goldenEval
                    "succeeds"
                    ( checkDeep
                        @(PDataRecord '["_0" ':= PDataRecord '["_1" ':= PInteger]])
                        @(PDataRecord '["_0" ':= PDataRecord '["_1" ':= PInteger]])
                        (pdata $ pdcons # pdata (pdcons # pdata (pconstant 42) # pdnil) # pdnil)
                    )
                , goldenEvalFail
                    "fails"
                    ( checkDeep
                        @(PDataRecord '["_0" ':= PDataRecord '["_1" ':= PByteString]])
                        @(PDataRecord '["_0" ':= PDataRecord '["_1" ':= PInteger]])
                        (pdata $ pdcons # pdata (pdcons # pdata (pconstant 42) # pdnil) # pdnil)
                    )
                , goldenEval
                    "sample usage contains the right value"
                    ( pconstant 42
                        #== theField
                    )
                ]
            ]
        , goldenGroup
            "example2"
            [ goldenEval "recovering a record succeeds" recoverAB
            ]
        ]
    ]

------------------- Checking deeply, shallowly and unwrapping ----------------------

checkDeep ::
  forall (target :: PType) (actual :: PType).
  PTryFrom PData (PAsData target) =>
  ClosedTerm (PAsData actual) ->
  ClosedTerm (PAsData target)
checkDeep t = unTermCont $ fst <$> checkDeep' t

checkDeep' ::
  forall (target :: PType) (actual :: PType) (s :: S).
  PTryFrom PData (PAsData target) =>
  ClosedTerm (PAsData actual) ->
  TermCont s (Term s (PAsData target), Reduce (PTryFromExcess PData (PAsData target) s))
checkDeep' t = TermCont (ptryFrom @(PAsData target) $ pforgetData t)

checkDeepUnwrap ::
  forall (target :: PType) (actual :: PType) (s :: S).
  PTryFrom PData (PAsData target) =>
  Term s (PAsData actual) ->
  Term s (PAsData target)
checkDeepUnwrap t = unTermCont $ fst <$> TermCont (ptryFrom @(PAsData target) $ pforgetData t)

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
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PPartialOrd, POrd)
instance DerivePlutusType PNatural where type DPTStrat _ = PlutusTypeNewtype

-- | partial
pmkNatural :: Term s (PInteger :--> PNatural)
pmkNatural = plam $ \i -> pif (i #< 0) (ptraceInfoError "could not make natural") (pcon $ PMkNatural i)

newtype Flip f b a = Flip (f a b)
  deriving stock (Generic)

instance PTryFrom PData (PAsData PNatural) where
  type PTryFromExcess PData (PAsData PNatural) = Flip Term PNatural
  ptryFrom' opq = runTermCont $ do
    (ter, exc) <- TermCont $ ptryFrom @(PAsData PInteger) opq
    ver <- tcont $ plet $ pmkNatural # exc
    pure (punsafeDowncast ter, ver)

------------- Helpers --------------------------------------------------------

toDatadList :: [Integer] -> Term s (PAsData (PBuiltinList (PAsData PInteger)))
toDatadList = pdata . foldr go pnil
  where
    go :: Integer -> Term _ (PBuiltinList (PAsData PInteger)) -> Term _ (PBuiltinList (PAsData PInteger))
    go i acc = pcons # pdata (pconstant i) # acc

------------------- Sample type with PIsDataRepr -----------------------------------

sampleAB :: Term s (PAsData PAB)
sampleAB = pdata $ pcon $ PA (pdcons @"_0" # pdata (pconstant 4) #$ pdcons # pdata (pconstant "foo") # pdnil)

sampleABdata :: Term s PData
sampleABdata = pforgetData sampleAB

recoverAB :: Term s (PAsData PAB)
recoverAB = unTermCont $ fst <$> tcont (ptryFrom sampleABdata)

data PAB (s :: S)
  = PA (Term s (PDataRecord '["_0" ':= PInteger, "_1" ':= PByteString]))
  | PB (Term s (PDataRecord '["_0" ':= PBuiltinList (PAsData PInteger), "_1" ':= PByteString]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)
instance DerivePlutusType PAB where type DPTStrat _ = PlutusTypeData
instance PTryFrom PData (PAsData PAB)

------------------- Sample usage with recovered record type ------------------------

untrustedRecord :: Term s PData
untrustedRecord =
  let rec_ :: Term s (PAsData (PDataRecord '["_0" ':= PDataRecord '["_1" ':= PInteger]]))
      rec_ = pdata $ pdcons # pdata (pdcons # pdata (pconstant 42) # pdnil) # pdnil
   in pforgetData rec_

theField :: Term s PInteger
theField = unTermCont $ do
  (_, exc) <- tcont (ptryFrom @(PAsData (PDataRecord '["_0" ':= PDataRecord '["_1" ':= PInteger]])) untrustedRecord)
  pure $ snd . getField @"_1" . snd . snd . getField @"_0" . snd $ exc

------------------- Sample usage DerivePNewType ------------------------------------

newtype PWrapInt (s :: S) = PWrapInt (Term s PInteger)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq, PPartialOrd, POrd)
instance DerivePlutusType PWrapInt where type DPTStrat _ = PlutusTypeNewtype
instance PTryFrom PData (PAsData PWrapInt)
