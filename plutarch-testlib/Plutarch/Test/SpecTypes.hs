{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Dummy types used in tests
module Plutarch.Test.SpecTypes (Triplet (..), PTriplet (..)) where

import Plutarch.DataRepr (PDataFields)
import Plutarch.Internal.Lift (DeriveDataPLiftable)
import Plutarch.Prelude
import PlutusTx qualified
import Prettyprinter (Pretty (pretty), (<+>))
import Test.Tasty.QuickCheck (Arbitrary, arbitrary)

{- | Tuple of three elements of the same type

@since WIP
-}
data Triplet a = Triplet a a a
  deriving stock (Show, Eq, Ord)

instance Pretty a => Pretty (Triplet a) where
  pretty (Triplet x y z) = pretty x <+> pretty y <+> pretty z

{- |
  We can defined a data-type using PDataRecord, with labeled fields.

  With an appropriate instance of 'PIsDataRepr', we can automatically
  derive 'PDataFields'.

@since WIP
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

-- | @since WIP
instance DerivePlutusType (PTriplet a) where type DPTStrat _ = PlutusTypeData

PlutusTx.makeIsDataIndexed ''Triplet [('Triplet, 0)]

-- | @since WIP
instance Arbitrary a => Arbitrary (Triplet a) where
  arbitrary = Triplet <$> arbitrary <*> arbitrary <*> arbitrary

-- | @since WIP
deriving via
  DeriveDataPLiftable (PTriplet a) (Triplet (AsHaskell a))
  instance
    (PlutusTx.ToData (AsHaskell a), PlutusTx.FromData (AsHaskell a)) => PLiftable (PTriplet a)
