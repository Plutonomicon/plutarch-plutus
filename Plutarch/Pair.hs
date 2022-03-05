module Plutarch.Pair (PPair (..)) where

import qualified GHC.Generics as GHC
import Generics.SOP (Generic, I (I))
import Plutarch.Bool (PEq)
import Plutarch.Internal.Other (PType, PlutusType, S, Term)

{- |
  Plutus encoding of Pairs.

  Note: This is represented differently than 'BuiltinPair'. It is scott-encoded.
-}
data PPair (a :: PType) (b :: PType) (s :: S)
  = PPair (Term s a) (Term s b)
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PlutusType, PEq)
