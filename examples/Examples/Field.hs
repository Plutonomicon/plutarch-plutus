{-# LANGUAGE UndecidableInstances #-}

module Examples.Field (
  -- * Examples
  Triplet (..),
  type SomeFields,
  mkTrip,
  tripA,
  tripB,
  tripC,
  tripTrip,
  tripSum,
  tripYZ,
  tripZY,
  by,
  dotPlus,
  tripSum',
  nFields,
  dropFields,
  dropFields',
  getY,
  rangeFields,
  letSomeFields,
  letSomeFields',

  -- * Testing
  tests,
) where

--------------------------------------------------------------------------------

import Plutarch
import Plutarch.Builtin (PAsData, PBuiltinList, PIsData (..))
import Plutarch.DataRepr (
  DataReprHandlers (DRHCons, DRHNil),
  PDataFields,
  PDataRecord,
  PIsDataRepr (pmatchRepr, type PIsDataReprRepr),
  PIsDataReprInstances (PIsDataReprInstances),
  PLabeledType ((:=)),
  pfield,
  pletAllFields,
  pletDropFields,
  pletFields,
  pletNFields,
  pletRangeFields,
  pmatchDataRepr,
 )
import Plutarch.Integer (PInteger)
import Plutarch.Lift (pconstant, plift)
import Plutarch.List (PListLike (pcons, pnil))

import qualified PlutusCore as PLC
import qualified PlutusTx

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Utils

--------------------------------------------------------------------------------

{- |
  We can defined a data-type using PDataRecord, with labeled fields.

  With an appropriate instance of 'PIsDataRepr', we can automatically
  derive 'PDataFields'.
-}
newtype Triplet (a :: PType) (s :: S)
  = Triplet
      ( Term
          s
          ( PDataRecord
              '[ "x" ':= a
               , "y" ':= a
               , "z" ':= a
               ]
          )
      )
  deriving
    (PMatch, PIsData, PDataFields)
    via (PIsDataReprInstances (Triplet a))

-- | The usual PIsDataRepr instance for a Record type...
instance PIsDataRepr (Triplet a) where
  type
    PIsDataReprRepr (Triplet a) =
      '[ '[ "x" ':= a
          , "y" ':= a
          , "z" ':= a
          ]
       ]
  pmatchRepr dat f =
    pmatchDataRepr dat $ DRHCons (f . Triplet) DRHNil

mkTrip ::
  forall a s. (PIsData a) => Term s a -> Term s a -> Term s a -> Term s (Triplet a)
mkTrip x y z =
  punsafeBuiltin PLC.ConstrData # (0 :: Term _ PInteger)
    # ( ( pcons # (pdata x)
            #$ pcons # (pdata y)
            #$ pcons # (pdata z)
              # pnil
        ) ::
          Term _ (PBuiltinList (PAsData a))
      )

-- | An example term
tripA :: Term s (Triplet PInteger)
tripA = mkTrip 150 750 100

-- | Another
tripB :: Term s (Triplet PInteger)
tripB = mkTrip 50 10 40

-- | Another
tripC :: Term s (Triplet PInteger)
tripC = mkTrip 1 8 1

-- | Nested triplet
tripTrip :: Term s (Triplet (Triplet PInteger))
tripTrip = mkTrip tripA tripB tripC

{- |
  We can bind all fields to a 'HRec' at once with 'pletAllFields'.

  The fields in the 'HRec' can them be accessed with
  RecordDotSyntax.
-}
tripSum :: Term s ((Triplet PInteger) :--> PInteger)
tripSum =
  plam $ \x -> pletAllFields x $
    \fs ->
      pfromData fs.x
        + pfromData fs.y
        + pfromData fs.z

{- |
  We can also bind the first N fields with 'pletNFields',
  this saves some code in binding the tails for additional fields.

  The fields in the 'HRec' can them be accessed with
  RecordDotSyntax.
-}
tripSum' :: Term s ((Triplet PInteger) :--> PInteger)
tripSum' =
  plam $ \x -> pletNFields @2 x $
    \fs ->
      pfromData fs.x
        + pfromData fs.y

{- |
  'pletFields' can also be used to bind the relevant fields,
  without knowing their range.
-}
tripYZ :: Term s ((Triplet PInteger) :--> PInteger)
tripYZ =
  plam $ \x -> pletFields @["y", "z"] x $
    \fs ->
      pfromData fs.y + pfromData fs.z

{- |
  The ordering of fields specified is irrelevant,
  this is equivalent to 'tripYZ'.
-}
tripZY :: Term s ((Triplet PInteger) :--> PInteger)
tripZY =
  plam $ \x -> pletFields @["z", "y"] x $
    \fs ->
      pfromData fs.y + pfromData fs.z

{- |
  When accessing only a single field, we can use
  'pfield', which can end up generating smaller code when used frequently
  in a script.
-}
by :: Term s PInteger
by = pfromData $ pfield @"y" # tripB

getY :: Term s (Triplet PInteger :--> PAsData PInteger)
getY = pfield @"y"

{- |
  Due to the instance @(PDataFields a) -> PDataFields (PAsData a)@,

  we can conveniently chain 'pletAllFields' & 'pfield' within
  nested structures:
-}
dotPlus :: Term s PInteger
dotPlus =
  pletAllFields tripTrip $ \ts ->
    pletAllFields (ts.x) $ \a ->
      pletAllFields (ts.y) $ \b ->
        pletAllFields (ts.z) $ \c ->
          (pfromData a.x * pfromData b.x)
            + (pfromData a.y * pfromData b.y)
            + (pfromData a.z * pfromData b.z)
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
      fmap (PlutusTx.toData @Integer) [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

{- |
  We can also bind over a 'PDataRecord' directly.

  'pletNFields' will more efficiently bind the first N fields
  of a PRecord.
-}
nFields :: Term s (PDataRecord SomeFields :--> PInteger)
nFields =
  plam $ \r -> pletNFields @2 r $ \fs ->
    pfromData fs._0
      + pfromData fs._1

{- |
  Or equivalently, with 'pletFields'
-}
nFieldsLet :: Term s (PDataRecord SomeFields :--> PInteger)
nFieldsLet =
  plam $ \r -> pletFields @["_0", "_1"] r $ \fs ->
    pfromData fs._0
      + pfromData fs._1

{- |
  'pletDropFields' will bind fields, dropping the first N.
-}
dropFields :: Term s (PDataRecord SomeFields :--> PInteger)
dropFields =
  plam $ \r -> pletDropFields @8 r $ \fs ->
    pfromData fs._8
      + pfromData fs._9

{- |
  Or equivalently, with 'pletFields'
-}
dropFieldsLet :: Term s (PDataRecord SomeFields :--> PInteger)
dropFieldsLet =
  plam $ \r -> pletFields @["_8", "_9"] r $ \fs ->
    pfromData fs._8
      + pfromData fs._9

-- | Without using 'pletDropFields', the code generated is a little less efficient
dropFields' :: Term s (PDataRecord SomeFields :--> PInteger)
dropFields' =
  plam $ \r -> pletAllFields r $ \fs ->
    pfromData fs._8
      + pfromData fs._9

-- | 'pletRangeFields' will bind fields in a specific range
rangeFields :: Term s (PDataRecord SomeFields :--> PInteger)
rangeFields =
  plam $ \r -> pletRangeFields @5 @6 r $ \fs ->
    pfromData fs._5
      + pfromData fs._6

{- |
  Or equivalently, with 'pletFields'
-}
rangeFieldsLet :: Term s (PDataRecord SomeFields :--> PInteger)
rangeFieldsLet =
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

---------- Tests

tests :: HasTester => TestTree
tests =
  testGroup
    "Field examples"
    [ testCase "tripSum compilation" $
        tripSum `equal'` tripSumComp
    , testCase "nFields compilation" $
        nFields `equal'` nFieldsComp
    , testCase "nFieldsLet = nFields" $
        nFieldsLet `equal` nFields
    , testCase "dropFields compilation" $
        dropFields `equal'` dropFieldsComp
    , testCase "dropFieldsLet = dropFields" $
        dropFieldsLet `equal` dropFields
    , testCase "dropFields' compilation" $
        dropFields' `equal'` dropFields'Comp
    , testCase "rangeFields compilation" $
        rangeFields `equal'` rangeFieldsComp
    , testCase "rangeFieldsLet = rangeFields" $
        rangeFieldsLet `equal` rangeFields
    , testCase "letSomeFields compilation" $
        letSomeFields `equal'` letSomeFieldsComp
    , testCase "letSomeFields = letSomeFields'" $
        letSomeFields `equal` letSomeFields'
    , testCase "nFields someFields = 1" $
        plift (nFields # someFields)
          @?= 1
    , testCase "dropFields someFields = 17" $
        plift (dropFields # someFields)
          @?= 17
    , testCase "rangeFields someFields = 11" $
        plift (rangeFields # someFields)
          @?= 11
    , testCase "letSomeFields someFields = 14" $
        plift (letSomeFields # someFields)
          @?= 14
    , testCase "getY compilation" $
        getY `equal'` getYComp
    , testCase "tripYZ compilation" $
        tripYZ `equal'` tripYZComp
    , testCase "tripYZ = tripZY" $
        tripZY `equal` tripYZ
    , testCase "tripSum # tripA = 1000" $
        plift (tripSum # tripA)
          @?= 1000
    , testCase "tripSum # tripB = 100" $
        plift (tripSum # tripB)
          @?= 100
    , testCase "tripSum # tripC = 10" $
        plift (tripSum # tripC)
          @?= 10
    , testCase "by = 10" $
        plift by @?= 10
    , testCase "dotPlus = 19010" $
        plift dotPlus @?= 19010
    ]

tripSumComp :: String
tripSumComp =
  "(program 1.0.0 (\\i0 -> (\\i0 -> (\\i0 -> addInteger (addInteger (unIData (force headList i2)) (unIData (force headList i1))) (unIData (force headList (force tailList i1)))) (force tailList i1)) ((\\i0 -> force (force sndPair) (unConstrData i1)) i1)))"

nFieldsComp :: String
nFieldsComp = "(program 1.0.0 (\\i0 -> addInteger (unIData (force headList i1)) (unIData (force headList (force tailList i1)))))"

dropFieldsComp :: String
dropFieldsComp = "(program 1.0.0 (\\i0 -> (\\i0 -> addInteger (unIData (force headList i1)) (unIData (force headList (force tailList i1)))) ((\\i0 -> force tailList (force tailList (force tailList (force tailList (force tailList (force tailList (force tailList (force tailList i1)))))))) i1)))"

dropFields'Comp :: String
dropFields'Comp = "(program 1.0.0 (\\i0 -> (\\i0 -> (\\i0 -> (\\i0 -> (\\i0 -> (\\i0 -> (\\i0 -> (\\i0 -> (\\i0 -> addInteger (unIData (force headList i1)) (unIData (force headList (force tailList i1)))) (force tailList i1)) (force tailList i1)) (force tailList i1)) (force tailList i1)) (force tailList i1)) (force tailList i1)) (force tailList i1)) (force tailList i1)))"

rangeFieldsComp :: String
rangeFieldsComp = "(program 1.0.0 (\\i0 -> (\\i0 -> addInteger (unIData (force headList i1)) (unIData (force headList (force tailList i1)))) ((\\i0 -> force tailList (force tailList (force tailList (force tailList (force tailList i1))))) i1)))"

getYComp :: String
getYComp = "(program 1.0.0 (\\i0 -> force headList (force tailList ((\\i0 -> force (force sndPair) (unConstrData i1)) i1))))"

tripYZComp :: String
tripYZComp = "(program 1.0.0 (\\i0 -> (\\i0 -> addInteger (unIData (force headList i1)) (unIData (force headList (force tailList i1)))) (force tailList ((\\i0 -> force (force sndPair) (unConstrData i1)) i1))))"

letSomeFieldsComp :: String
letSomeFieldsComp = "(program 1.0.0 (\\i0 -> (\\i0 -> (\\i0 -> (\\i0 -> (\\i0 -> addInteger (addInteger (unIData (force headList i4)) (unIData (force headList i3))) (unIData (force headList (force tailList i1)))) (force tailList i1)) (force tailList i1)) (force tailList i1)) ((\\i0 -> force tailList (force tailList (force tailList i1))) i1)))"
