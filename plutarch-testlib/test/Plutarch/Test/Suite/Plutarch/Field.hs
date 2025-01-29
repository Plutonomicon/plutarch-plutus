{-# LANGUAGE OverloadedRecordDot #-}

-- TODO: Remove this module once old DataRepr stuff is fully gone

module Plutarch.Test.Suite.Plutarch.Field (tests) where

import Plutarch.Prelude
import Plutarch.Test.Golden (goldenEval, goldenGroup, plutarchGolden)
import Plutarch.Test.SpecTypes (PTriplet)
import Plutarch.Unsafe (punsafeBuiltin, punsafeCoerce)
import PlutusCore qualified as PLC
import PlutusTx (toData)
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
            , goldenEval "tripSum.A" (tripSum # tripA)
            , goldenEval "tripSum.B" (tripSum # tripB)
            , goldenEval "tripSum.C" (tripSum # tripC)
            , goldenEval "tripYZ=tripZY" tripYZ
            ]
        , plutarchGolden
            "rangeFields"
            "field.rangeFields"
            [ goldenEval "lam" rangeFields
            , goldenEval "app" (rangeFields # someFields)
            ]
        , plutarchGolden
            "dropFields"
            "field.dropFields"
            [ goldenEval "lam" dropFields
            , goldenEval "app" (dropFields # someFields)
            ]
        , plutarchGolden
            "pletFields"
            "field.pletFields"
            [ goldenGroup
                "letSomeFields"
                [ goldenEval "lam" letSomeFields
                , goldenEval "order" letSomeFields
                , goldenEval "app" (letSomeFields # someFields)
                ]
            , goldenGroup
                "nFields"
                [ goldenEval "lam" nFields
                , goldenEval "app" (nFields # someFields)
                ]
            ]
        , plutarchGolden
            "other"
            "field.other"
            [ goldenEval "by" by
            , goldenEval "dotPlus" dotPlus
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
    pconstant @(PBuiltinList PData) $
      fmap toData ([0, 1, 2, 3, 4, 5, 6, 7, 8, 9] :: [Integer])

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
