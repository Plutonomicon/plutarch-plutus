module Plutarch.Either (PEither (PLeft, PRight)) where

import GHC.Generics (Generic)
import Plutarch (
  DPTStrat,
  DerivePlutusType,
  PType,
  PlutusType,
  PlutusTypeScott,
  S,
  Term,
 )
import Plutarch.Bool (PEq)
import Plutarch.Show (PShow)

data PEither (a :: PType) (b :: PType) (s :: S)
  = PLeft (Term s a)
  | PRight (Term s b)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq, PShow)
instance DerivePlutusType (PEither a b) where type DPTStrat _ = PlutusTypeScott
