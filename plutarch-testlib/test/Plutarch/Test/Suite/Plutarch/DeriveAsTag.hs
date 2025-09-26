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
import Plutarch.Prelude (PEq, PlutusType, S)
import Plutarch.Repr.Tag (DeriveAsTag (DeriveAsTag))
import Plutarch.Test.Laws (checkPLiftableLaws, checkPLiftableLawsForDeriveTags, checkPlutusTypeLaws)
import Prettyprinter (Pretty (pretty))
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))
import Test.QuickCheck.Gen (elements)
import Test.Tasty (TestTree, testGroup)

-- | @since 1.0.2
data PFoo (s :: S) = A | B | C | D | E
  deriving stock (GHC.Generic, Show, Eq)
  deriving anyclass (PEq)
  deriving
    (PlutusType, PLiftable)
    via DeriveAsTag PFoo

-- | @since 1.0.2
instance Pretty (PFoo s) where
  pretty = pretty . show

-- | @since 1.0.2
instance SOP.Generic (PFoo s)

-- | @since 1.0.2
instance Arbitrary (PFoo s) where
  {-# INLINEABLE arbitrary #-}
  arbitrary = elements [A, B, C, D, E]

-- | @since 1.0.2
tests :: TestTree
tests =
  testGroup
    "DeriveAsTag"
    ( mconcat
        [ checkPLiftableLawsForDeriveTags @PFoo
        , checkPLiftableLaws @PFoo
        , checkPlutusTypeLaws @PFoo
        ]
    )
