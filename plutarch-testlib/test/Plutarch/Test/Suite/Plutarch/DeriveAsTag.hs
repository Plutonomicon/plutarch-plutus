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
import Plutarch.Prelude (PBool (PFalse, PTrue), PEq, PlutusType, S, pcon, pmatch)
import Plutarch.Repr.Tag (DeriveAsTag (DeriveAsTag))
import Plutarch.Test.Golden (goldenEval, plutarchGolden)
import Plutarch.Test.Laws (checkPLiftableLaws, checkPLiftableLawsForDeriveTags, checkPlutusTypeLaws)
import Prettyprinter (Pretty (pretty))
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))
import Test.QuickCheck.Gen (elements)
import Test.Tasty (TestTree, testGroup)

data PFoo1 (s :: S) = A1
  deriving stock (GHC.Generic, Show, Eq)
  deriving anyclass (SOP.Generic, PEq)
  deriving
    (PlutusType, PLiftable)
    via DeriveAsTag PFoo1

data PFoo2 (s :: S) = A2 | B2
  deriving stock (GHC.Generic, Show, Eq)
  deriving anyclass (SOP.Generic, PEq)
  deriving
    (PlutusType, PLiftable)
    via DeriveAsTag PFoo2

data PFoo3 (s :: S) = A3 | B3 | C3
  deriving stock (GHC.Generic, Show, Eq)
  deriving anyclass (SOP.Generic, PEq)
  deriving
    (PlutusType, PLiftable)
    via DeriveAsTag PFoo3

data PFoo4 (s :: S) = A4 | B4 | C4 | D4
  deriving stock (GHC.Generic, Show, Eq)
  deriving anyclass (SOP.Generic, PEq)
  deriving
    (PlutusType, PLiftable)
    via DeriveAsTag PFoo4

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
    [ testGroup "Laws" . mconcat $
        [ checkPLiftableLawsForDeriveTags @PFoo
        , checkPLiftableLaws @PFoo
        , checkPlutusTypeLaws @PFoo
        ]
    , plutarchGolden
        "Tag encoding and decoding"
        "derive-as-tag"
        [ goldenEval "pcon 1" (pcon A1)
        , goldenEval "pmatch 1" (pmatch (pcon A1) (\x -> if x == A1 then pcon PTrue else pcon PFalse))
        , goldenEval "pcon 2" (pcon B2)
        , goldenEval "pmatch 2" (pmatch (pcon B2) (\x -> if x == B2 then pcon PTrue else pcon PFalse))
        , goldenEval "pcon 3" (pcon C3)
        , goldenEval "pmatch 3" (pmatch (pcon C3) (\x -> if x == C3 then pcon PTrue else pcon PFalse))
        , goldenEval "pcon 4" (pcon C4)
        , goldenEval "pmatch 4" (pmatch (pcon C4) (\x -> if x == C4 then pcon PTrue else pcon PFalse))
        , goldenEval "pcon 5" (pcon C)
        , goldenEval "pmatch 5" (pmatch (pcon D) (\x -> if x == D then pcon PTrue else pcon PFalse))
        ]
    ]
