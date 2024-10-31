{-# LANGUAGE OverloadedRecordDot #-}

module Plutarch.Test.Suite.Plutarch.Field (tests) where

import Plutarch.Builtin (ppairDataBuiltin)
import Plutarch.Internal.Term (punsafeBuiltin, punsafeCoerce)
import Plutarch.LedgerApi.V1 (PAddress (PAddress))
import Plutarch.Prelude
import Plutarch.Test.Golden (goldenEval, goldenEvalEqual, goldenGroup, plutarchGolden)
import Plutarch.Test.SpecTypes (PTriplet)
import PlutusCore qualified as PLC
import PlutusLedgerApi.V1.Address (Address (Address))
import PlutusLedgerApi.V1.Credential (Credential (PubKeyCredential))
import PlutusTx qualified
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Field"
    [ testGroup
        "Goldens"
        [ plutarchGolden
            "Trips"
            "field.trips"
            [ goldenGroup
                "lam"
                [ goldenEval "tripSum" tripSum
                , goldenEval "getY" getY
                , goldenEval "tripYZ" tripYZ
                ]
            , goldenEvalEqual "tripSum.A" (tripSum # tripA) (pconstant 1000)
            , goldenEvalEqual "tripSum.B" (tripSum # tripB) (pconstant 100)
            , goldenEvalEqual "tripSum.C" (tripSum # tripC) (pconstant 10)
            , goldenEvalEqual "tripYZ=tripZY" tripYZ tripZY
            ]
        , plutarchGolden
            "rangeFields"
            "field.rangeFields"
            [ goldenEval "lam" rangeFields
            , goldenEvalEqual "app" (rangeFields # someFields) (pconstant 11)
            ]
        , plutarchGolden
            "dropFields"
            "field.dropFields"
            [ goldenEval "lam" dropFields
            , goldenEvalEqual "app" (dropFields # someFields) (pconstant 17)
            ]
        , plutarchGolden
            "pletFields"
            "field.pletFields"
            [ goldenGroup
                "letSomeFields"
                [ goldenEval "lam" letSomeFields
                , goldenEvalEqual "order" letSomeFields letSomeFields'
                , goldenEvalEqual "app" (letSomeFields # someFields) (pconstant 14)
                ]
            , goldenGroup
                "nFields"
                [ goldenEval "lam" nFields
                , goldenEvalEqual "app" (nFields # someFields) (pconstant 1)
                ]
            ]
        , plutarchGolden
            "other"
            "field.other"
            [ goldenEvalEqual "by" by (pconstant 10)
            , goldenEvalEqual "dotPlus" dotPlus (pconstant 19010)
            ]
        , plutarchGolden
            "data"
            "field.data"
            [ goldenGroup
                "pmatch-pfield"
                [ goldenGroup
                    "pmatch"
                    [ goldenEval
                        "newtype"
                        ( let addr = pconstant $ Address (PubKeyCredential "ab") Nothing
                           in pmatch addr $ \(PAddress addrFields) ->
                                pletFields @'["credential", "stakingCredential"] addrFields $ \y ->
                                  ppairDataBuiltin # getField @"credential" y # getField @"stakingCredential" y
                        )
                    ]
                , goldenGroup
                    "pfield"
                    [ goldenEval
                        "newtype"
                        ( let addr = pconstant $ Address (PubKeyCredential "ab") Nothing
                           in pletFields @'["credential", "stakingCredential"] addr $ \y ->
                                ppairDataBuiltin # getField @"credential" y # getField @"stakingCredential" y
                        )
                    ]
                ]
            , goldenGroup
                "pfield-pletFields"
                [ goldenGroup
                    "pfield"
                    [ goldenEval
                        "single"
                        ( let addr = pconstant $ Address (PubKeyCredential "ab") Nothing
                           in pfromData $ pfield @"credential" # addr
                        )
                    ]
                , goldenGroup
                    "pletFields"
                    [ goldenEval
                        "single"
                        ( let addr = pconstant $ Address (PubKeyCredential "ab") Nothing
                           in pletFields @'["credential"] addr $ \y ->
                                pfromData $ getField @"credential" y
                        )
                    ]
                ]
            ]
        ]
    ]

--------------------------------------------------------------------------------

mkTrip ::
  forall a s. PIsData a => Term s a -> Term s a -> Term s a -> Term s (PTriplet a)
mkTrip x y z =
  punsafeBuiltin PLC.ConstrData
    # (0 :: Term _ PInteger)
    # ( ( pcons
            # pdata x
            #$ pcons
            # pdata y
            #$ pcons
            # pdata z
            # pnil
        ) ::
          Term _ (PBuiltinList (PAsData a))
      )

-- | An example term
tripA :: Term s (PTriplet PInteger)
tripA = mkTrip 150 750 100

-- | Another
tripB :: Term s (PTriplet PInteger)
tripB = mkTrip 50 10 40

-- | Another
tripC :: Term s (PTriplet PInteger)
tripC = mkTrip 1 8 1

-- | Nested PTriplet
tripTrip :: Term s (PTriplet (PTriplet PInteger))
tripTrip = mkTrip tripA tripB tripC

{- |
  'pletFields' generates efficient bindings for the specified fields,
  as a 'HRec' of fields.

  The fields in the 'HRec' can them be accessed with
  RecordDotSyntax.
-}
tripSum :: Term s (PTriplet PInteger :--> PInteger)
tripSum =
  plam $ \x -> pletFields @["x", "y", "z"] x $
    \fs ->
      pfromData fs.x
        + pfromData fs.y
        + pfromData fs.z

{- |
   A subset of fields can be specified.
-}
tripYZ :: Term s (PTriplet PInteger :--> PInteger)
tripYZ =
  plam $ \x -> pletFields @["y", "z"] x $
    \fs ->
      pfromData fs.y + pfromData fs.z

{- |
  The ordering of fields specified is irrelevant,
  this is equivalent to 'tripYZ'.
-}
tripZY :: Term s (PTriplet PInteger :--> PInteger)
tripZY =
  plam $ \x -> pletFields @["z", "y"] x $
    \fs ->
      pfromData fs.y + pfromData fs.z

{- |
  When accessing only a single field, we can use 'pfield'.

  This should be used carefully - if more than one field is needed,
  'pletFields' is more efficient.
-}
by :: Term s PInteger
by = pfield @"y" # tripB

getY :: Term s (PTriplet PInteger :--> PAsData PInteger)
getY = pfield @"y"

{- |
  Due to the instance @(PDataFields a) -> PDataFields (PAsData a)@,

  we can conveniently chain 'pletAllFields' & 'pfield' within
  nested structures:
-}
dotPlus :: Term s PInteger
dotPlus =
  pletFields @["x", "y", "z"] tripTrip $ \ts ->
    pletFields @["x", "y", "z"] ts.x $ \a ->
      pletFields @["x", "y", "z"] ts.y $ \b ->
        pletFields @["x", "y", "z"] ts.z $ \c ->
          pfromData a.x * pfromData b.x
            + pfromData a.y * pfromData b.y
            + pfromData a.z * pfromData b.z
            + pfromData c.x
            + pfromData c.y
            + pfromData c.z

type SomeFields =
  '[ "_0" ':= PInteger
   , "_1" ':= PInteger
   , "_2" ':= PInteger
   , "_3" ':= PInteger
   , "_4" ':= PInteger
   , "_5" ':= PInteger
   , "_6" ':= PInteger
   , "_7" ':= PInteger
   , "_8" ':= PInteger
   , "_9" ':= PInteger
   ]

someFields :: Term s (PDataRecord SomeFields)
someFields =
  punsafeCoerce $
    pconstant $
      fmap (PlutusTx.toData @Integer) ([0, 1, 2, 3, 4, 5, 6, 7, 8, 9] :: [Integer])

{- |
  We can also bind over a 'PDataRecord' directly.
-}
nFields :: Term s (PDataRecord SomeFields :--> PInteger)
nFields =
  plam $ \r -> pletFields @["_0", "_1"] r $ \fs ->
    pfromData fs._0
      + pfromData fs._1

dropFields :: Term s (PDataRecord SomeFields :--> PInteger)
dropFields =
  plam $ \r -> pletFields @["_8", "_9"] r $ \fs ->
    pfromData fs._8
      + pfromData fs._9

rangeFields :: Term s (PDataRecord SomeFields :--> PInteger)
rangeFields =
  plam $ \r -> pletFields @["_5", "_6"] r $ \fs ->
    pfromData fs._5
      + pfromData fs._6

{- |
  'pletFields' makes it convenient to pick out
  any amount of desired fields, efficiently.
-}
letSomeFields :: Term s (PDataRecord SomeFields :--> PInteger)
letSomeFields =
  plam $ \r -> pletFields @["_3", "_4", "_7"] r $ \fs ->
    pfromData fs._3
      + pfromData fs._4
      + pfromData fs._7

{- |
  Ordering of fields is irrelevant
-}
letSomeFields' :: Term s (PDataRecord SomeFields :--> PInteger)
letSomeFields' =
  plam $ \r -> pletFields @["_7", "_3", "_4"] r $ \fs ->
    pfromData fs._3
      + pfromData fs._4
      + pfromData fs._7
