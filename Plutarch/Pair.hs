module Plutarch.Pair (PPair (..)) where

import GHC.Generics (Generic)
import Plutarch.Internal (DPTStrat, DerivePlutusType, PEq, PShow, PType, PlutusType, PlutusTypeScott, S, Term)

{- |
  Plutus encoding of Pairs.

  Note: This is represented differently than 'BuiltinPair'. It is scott-encoded.
-}
data PPair (a :: PType) (b :: PType) (s :: S)
  = PPair (Term s a) (Term s b)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq, PShow)

instance DerivePlutusType (PPair a b) where type DPTStrat _ = PlutusTypeScott
