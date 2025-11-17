{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Dummy types used in tests
module Plutarch.Test.SpecTypes (Triplet (..), PTriplet (..)) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Plutarch.Prelude
import PlutusTx qualified
import Prettyprinter (Pretty (pretty), (<+>))
import Test.Tasty.QuickCheck (Arbitrary, arbitrary)

{- | Tuple of three elements of the same type

@since 1.0.0
-}
data Triplet a = Triplet a a a
  deriving stock (Show, Eq, Ord)

instance Pretty a => Pretty (Triplet a) where
  pretty (Triplet x y z) = pretty x <+> pretty y <+> pretty z

{- |
  We can defined a data-type using PDataRecord, with labeled fields.

  With an appropriate instance of 'PIsDataRepr', we can automatically
  derive 'PDataFields'.

@since 1.0.0
-}
newtype PTriplet (a :: S -> Type) (s :: S)
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
  deriving anyclass (PlutusType, PIsData, PEq, POrd, PDataFields)

-- | @since 1.0.0
instance DerivePlutusType (PTriplet a) where type DPTStrat _ = PlutusTypeData

PlutusTx.makeIsDataIndexed ''Triplet [('Triplet, 0)]

-- | @since 1.0.0
instance Arbitrary a => Arbitrary (Triplet a) where
  arbitrary = Triplet <$> arbitrary <*> arbitrary <*> arbitrary

-- | @since 1.0.0
deriving via
  DeriveDataPLiftable (PTriplet a) (Triplet (AsHaskell a))
  instance
    (PlutusTx.ToData (AsHaskell a), PlutusTx.FromData (AsHaskell a)) => PLiftable (PTriplet a)
