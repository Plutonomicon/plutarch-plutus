{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Dummy types used in tests
module Plutarch.Test.SpecTypes (Triplet (..), PTriplet (..)) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
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

@since 1.0.0
-}
data PTriplet (a :: S -> Type) (s :: S) = PTriplet
  { ptriplet'a :: Term s (PAsData a)
  , ptriplet'b :: Term s (PAsData a)
  , ptriplet'c :: Term s (PAsData a)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq)
  deriving (PlutusType) via (DeriveAsDataStruct (PTriplet a))

PlutusTx.makeIsDataIndexed ''Triplet [('Triplet, 0)]

-- | @since 1.0.0
instance Arbitrary a => Arbitrary (Triplet a) where
  arbitrary = Triplet <$> arbitrary <*> arbitrary <*> arbitrary

-- | @since 1.0.0
deriving via
  DeriveDataPLiftable (PTriplet a) (Triplet (AsHaskell a))
  instance
    (PlutusTx.ToData (AsHaskell a), PlutusTx.FromData (AsHaskell a)) => PLiftable (PTriplet a)
