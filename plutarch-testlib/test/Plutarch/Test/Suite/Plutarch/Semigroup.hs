-- Needed because ByteString isn't Pretty
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Test.Suite.Plutarch.Semigroup (tests) where

import Data.ByteString (ByteString)
import Plutarch.BitString (PBitString)
import Plutarch.Prelude
import Plutarch.Test.Laws (checkPMonoidLaws, checkPSemigroupLaws)
import Prettyprinter (Pretty (pretty), viaShow)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "PSemigroup and PMonoid"
    [ testGroup
        "PByteString"
        [ checkPSemigroupLaws @PByteString
        , checkPMonoidLaws @PByteString
        ]
    , testGroup
        "PBitString"
        [ checkPSemigroupLaws @PBitString
        , checkPMonoidLaws @PBitString
        ]
    , testGroup
        "PAnd PByteString"
        [ checkPSemigroupLaws @(PAnd PByteString)
        , checkPMonoidLaws @(PAnd PByteString)
        ]
    , testGroup
        "POr PByteString"
        [ checkPSemigroupLaws @(POr PByteString)
        , checkPMonoidLaws @(POr PByteString)
        ]
    , testGroup
        "PXor PByteString"
        [ checkPSemigroupLaws @(PXor PByteString)
        , checkPMonoidLaws @(PXor PByteString)
        ]
    ]

-- Orphan Pretty for ByteString, sigh
instance Pretty ByteString where
  {-# INLINEABLE pretty #-}
  pretty = viaShow
