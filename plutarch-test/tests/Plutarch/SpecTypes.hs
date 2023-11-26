{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.SpecTypes (Triplet (..), PTriplet (..)) where

import PlutusTx qualified

import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
 )
import Plutarch.Lift (
  PConstantDecl (PConstanted),
  PUnsafeLiftDecl (PLifted),
 )
import Plutarch.Prelude
import Test.Tasty.QuickCheck (Arbitrary, arbitrary)

data Triplet a = Triplet a a a
  deriving stock (Show, Eq, Ord)

{- |
  We can defined a data-type using PDataRecord, with labeled fields.

  With an appropriate instance of 'PIsDataRepr', we can automatically
  derive 'PDataFields'.
-}
newtype PTriplet (a :: PType) (s :: S)
  = PTriplet
      ( Term
          s
          ( PDataRecord
              '[ "x" ':= a
               , "y" ':= a
               , "z" ':= a
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PPartialOrd, POrd, PDataFields)

instance DerivePlutusType (PTriplet a) where type DPTStrat _ = PlutusTypeData

PlutusTx.makeIsDataIndexed ''Triplet [('Triplet, 0)]

instance PLiftData a => PUnsafeLiftDecl (PTriplet a) where
  type PLifted (PTriplet a) = Triplet (PLifted a)

deriving via
  (DerivePConstantViaData (Triplet a) (PTriplet (PConstanted a)))
  instance
    PConstantData a => PConstantDecl (Triplet a)

instance Arbitrary a => Arbitrary (Triplet a) where
  arbitrary = Triplet <$> arbitrary <*> arbitrary <*> arbitrary
