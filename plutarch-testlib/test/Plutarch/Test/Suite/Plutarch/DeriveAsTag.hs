{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Plutarch.Test.Suite.Plutarch.DeriveAsTag (tests)
where

import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Plutarch.Internal.Lift (
  PLiftable,
  PLifted (PLifted),
 )
import Plutarch.Internal.PlutusType (PlutusType)
import Plutarch.Prelude (S)
import Plutarch.Repr.Tag (DeriveAsTag (DeriveAsTag))
import Plutarch.Test.Laws (AsTagTest (tagNum), checkPLiftableLawsForDeriveTags)
import Prettyprinter (Pretty (pretty))
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))
import Test.QuickCheck.Gen (elements)
import Test.Tasty (TestTree, testGroup)

data PFoo (s :: S) = A | B | C | D | E
  deriving stock (GHC.Generic, Show, Eq)
  deriving
    (PlutusType, PLiftable)
    via DeriveAsTag PFoo

instance Pretty (PFoo s) where
  pretty = \case
    A -> "A"
    B -> "B"
    C -> "C"
    D -> "D"
    E -> "E"

instance SOP.Generic (PFoo s)

instance Arbitrary (PFoo s) where
  {-# INLINEABLE arbitrary #-}
  arbitrary = elements [A, B, C, D, E]

instance AsTagTest (PFoo s) where
  tagNum A = 0
  tagNum B = 1
  tagNum C = 2
  tagNum D = 3
  tagNum E = 4

tests :: TestTree
tests =
  testGroup
    "DeriveAsTag"
    [checkPLiftableLawsForDeriveTags @PFoo]
