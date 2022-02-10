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
  nFields,
  dropFields,
  getY,
  rangeFields,
  letSomeFields,
  letSomeFields',

  -- * Testing
  tests,
) where

--------------------------------------------------------------------------------

import qualified GHC.Generics as GHC
import Generics.SOP (Generic)
import Plutarch
import Plutarch.Builtin (PAsData, PBuiltinList, PIsData (..))
import Plutarch.DataRepr (
  PDataFields,
  PDataRecord,
  PIsDataRepr,
  PIsDataReprInstances (PIsDataReprInstances),
  PLabeledType ((:=)),
  pfield,
  pletFields,
 )
import Plutarch.Integer (PInteger)
import Plutarch.Lift (pconstant, plift)
import Plutarch.List (PListLike (pcons, pnil))
import Plutarch.Unsafe (punsafeBuiltin, punsafeCoerce)

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
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PMatch, PIsData, PDataFields)
    via (PIsDataReprInstances (Triplet a))

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
  'pletFields' generates efficient bindings for the specified fields,
  as a 'HRec' of fields.

  The fields in the 'HRec' can them be accessed with
  RecordDotSyntax.
-}
tripSum :: Term s ((Triplet PInteger) :--> PInteger)
tripSum =
  plam $ \x -> pletFields @["x", "y", "z"] x $
    \fs ->
      pfromData fs.x
        + pfromData fs.y
        + pfromData fs.z

{- |
   A subset of fields can be specified.
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
  When accessing only a single field, we can use 'pfield'.

  This should be used carefully - if more than one field is needed,
  'pletFields' is more efficient.
-}
by :: Term s PInteger
by = pfield @"y" # tripB

getY :: Term s (Triplet PInteger :--> PAsData PInteger)
getY = pfield @"y"

{- |
  Due to the instance @(PDataFields a) -> PDataFields (PAsData a)@,

  we can conveniently chain 'pletAllFields' & 'pfield' within
  nested structures:
-}
dotPlus :: Term s PInteger
dotPlus =
  pletFields @["x", "y", "z"] tripTrip $ \ts ->
    pletFields @["x", "y", "z"] (ts.x) $ \a ->
      pletFields @["x", "y", "z"] (ts.y) $ \b ->
        pletFields @["x", "y", "z"] (ts.z) $ \c ->
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

---------- Tests

tests :: HasTester => TestTree
tests =
  testGroup
    "Field examples"
    [ testCase "tripSum compilation" $
        printTerm tripSum @?= tripSumComp
    , testCase "nFields compilation" $
        printTerm nFields @?= nFieldsComp
    , testCase "dropFields compilation" $
        printTerm dropFields @?= dropFieldsComp
    , testCase "rangeFields compilation" $
        printTerm rangeFields @?= rangeFieldsComp
    , testCase "letSomeFields compilation" $
        printTerm letSomeFields @?= letSomeFieldsComp
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
        printTerm getY @?= getYComp
    , testCase "tripYZ compilation" $
        printTerm tripYZ @?= tripYZComp
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
  "(program 1.0.0 ((\\i0 -> (\\i0 -> \\i0 -> (\\i0 -> (\\i0 -> addInteger (addInteger (unIData (i4 i2)) (unIData (i4 i1))) (unIData (i4 (i5 i1)))) (i4 i1)) (force (force sndPair) (unConstrData i1))) (force headList)) (force tailList)))"

nFieldsComp :: String
nFieldsComp = "(program 1.0.0 ((\\i0 -> \\i0 -> addInteger (unIData (i2 i1)) (unIData (i2 (force tailList i1)))) (force headList)))"

dropFieldsComp :: String
dropFieldsComp = "(program 1.0.0 ((\\i0 -> (\\i0 -> \\i0 -> (\\i0 -> addInteger (unIData (i3 i1)) (unIData (i3 (i4 i1)))) (i3 (i3 (i3 (i3 (i3 (i3 (i3 (i3 i1))))))))) (force headList)) (force tailList)))"

rangeFieldsComp :: String
rangeFieldsComp = "(program 1.0.0 ((\\i0 -> (\\i0 -> \\i0 -> (\\i0 -> addInteger (unIData (i3 i1)) (unIData (i3 (i4 i1)))) (i3 (i3 (i3 (i3 (i3 i1)))))) (force headList)) (force tailList)))"

getYComp :: String
getYComp = "(program 1.0.0 (\\i0 -> force headList (force tailList (force (force sndPair) (unConstrData i1)))))"

tripYZComp :: String
tripYZComp = "(program 1.0.0 ((\\i0 -> (\\i0 -> \\i0 -> (\\i0 -> addInteger (unIData (i3 i1)) (unIData (i3 (i4 i1)))) (i3 (force (force sndPair) (unConstrData i1)))) (force headList)) (force tailList)))"

letSomeFieldsComp :: String
letSomeFieldsComp = "(program 1.0.0 ((\\i0 -> (\\i0 -> (\\i0 -> \\i0 -> (\\i0 -> (\\i0 -> addInteger (addInteger (unIData (i4 i2)) (unIData (i4 i1))) (unIData (i4 (i5 (i6 i1))))) (i5 i1)) (i4 (i3 i1))) (force headList)) (\\i0 -> i2 (i2 i1))) (force tailList)))"
