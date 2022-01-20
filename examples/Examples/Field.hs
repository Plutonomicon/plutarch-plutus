{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Examples.Field 
  ( -- * Examples
    Triplet (..)
  , mkTrip
  , tripA, tripB, tripC
  , tripTrip, tripSum, by, dotPlus, tripSum', someFields
    -- * Testing
  , tests
  ) where

--------------------------------------------------------------------------------

import Plutarch.Integer (PInteger)
import Plutarch.Lift (plift)
import Plutarch
import Plutarch.DataRepr 
  (PDataRecord, PLabeled (..), PIsDataRepr (..), PIsDataReprInstances (..), DataReprHandlers (..), pmatchDataRepr)
import Plutarch.Builtin (PAsData, PBuiltinList, PIsData (..))
import Plutarch.Field (DerivePDataFields, PDataFields, pletFields, pfield, pletNFields)
import Plutarch.List (PListLike (..))

import qualified PlutusCore as PLC

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Utils

--------------------------------------------------------------------------------

{- | 
  We can defined a data-type using PDataRecord, with labeled fields.
  
  With an appropriate instance of 'PIsDataRepr', we can automatically
  derive 'PDataFields'. 

-}
newtype Triplet (a :: PType) (s :: S) = 
   Triplet 
    (Term s (PDataRecord 
      ' [ "x" ':= a
        , "y" ':= a
        , "z" ':= a
        ]))
  deriving 
    (PMatch, PIsData) 
    via (PIsDataReprInstances (Triplet a) ()) 
  deriving
    (PDataFields) via (DerivePDataFields (Triplet a))

-- | The usual PIsDataRepr instance for a Record type...
instance PIsDataRepr (Triplet a) where
  type PIsDataReprRepr (Triplet a) = 
    '[ 
      '[ "x" ':= a
       , "y" ':= a
       , "z" ':= a
       ] 
    ]
  pmatchRepr dat f = 
    pmatchDataRepr dat $ DRHCons (f . Triplet) DRHNil

mkTrip :: 
  forall a s. (PIsData a) => Term s a -> Term s a -> Term s a -> Term s (Triplet a)
mkTrip x y z = 
  punsafeBuiltin PLC.ConstrData # (0 :: Term _ PInteger) #
  ((pcons # (pdata x) 
    #$ pcons # (pdata y) 
      #$ pcons # (pdata z)
        # pnil) :: Term _ (PBuiltinList (PAsData a)))

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
  We can bind all fields to a 'HRec' at once with 'pletFields'.

  The fields in the 'HRec' can them be accessed with
  RecordDotSyntax.

-}
tripSum :: Term s ((Triplet PInteger) :--> PInteger)
tripSum = 
  plam $ \x -> pletFields x $ 
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
  When accessing only a single field, we can use 
  'pfield', which can end up generating smaller code when used frequently
  in a script.
-}
by :: Term s PInteger
by = pfromData $ pfield @"y" # tripB

{- | 
  Due to the instance @(PDataFields a) -> PDataFields (PAsData a)@,

  we can conveniently chain 'pletFields' & 'pfield' within 
  nested structures:
-}
dotPlus :: Term s PInteger
dotPlus = 
  pletFields tripTrip $ \ts ->
    pletFields (ts.x) $ \a ->
      pletFields (ts.y) $ \b ->
        pletFields (ts.z) $ \c ->
          (pfromData a.x * pfromData b.x) 
          + (pfromData a.y * pfromData b.y) 
          + (pfromData a.z * pfromData b.z)
          + pfromData c.x + pfromData c.y + pfromData c.z


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

{- | 
  We can also bind over a 'PDataRecord'.

-}
someFields :: Term s (PDataRecord SomeFields :--> PInteger)
someFields = 
  plam $ \r -> pletNFields @5 r $ \fs ->
    pfromData fs._3 
    + pfromData fs._4

---------- Tests

tests :: HasTester => TestTree
tests = 
 testGroup "Field examples"
  [ testCase "tripSum compilation" $ 
      tripSum `equal'` tripSumComp
  , testCase "someFields compilation" $ 
      someFields `equal'` someFieldsComp
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

someFieldsComp :: String
someFieldsComp = "(program 1.0.0 (\\i0 -> (\\i0 -> (\\i0 -> (\\i0 -> addInteger (unIData (force headList i1)) (unIData (force headList (force tailList i1)))) (force tailList i1)) (force tailList i1)) (force tailList i1)))"
