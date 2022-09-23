module Plutarch.Pair (PPair (..)) where

import GHC.Generics (Generic)
import Plutarch.Bool (PEq)
import Plutarch.Internal (PType, S, Term)
import Plutarch.Internal.PlutusType (DPTStrat, DerivePlutusType, PlutusType)
import Plutarch.Internal.ScottEncoding (PlutusTypeScott)
import Plutarch.Show (PShow)

{- |
  Plutus encoding of Pairs.

  Note: This is represented differently than 'BuiltinPair'. It is scott-encoded.
-}
data PPair (a :: PType) (b :: PType) (s :: S)
  = PPair (Term s a) (Term s b)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq, PShow)

instance DerivePlutusType (PPair a b) where type DPTStrat _ = PlutusTypeScott
