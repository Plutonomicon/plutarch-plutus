module Plutarch.Either (PEither (PLeft, PRight)) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Plutarch.Internal.Eq (PEq)
import Plutarch.Internal.PlutusType (
  DerivePlutusType (DPTStrat),
  PlutusType,
 )
import Plutarch.Internal.ScottEncoding (PlutusTypeScott)
import Plutarch.Internal.Term (S, Term)
import Plutarch.Show (PShow)

data PEither (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PLeft (Term s a)
  | PRight (Term s b)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq, PShow)

instance DerivePlutusType (PEither a b) where
  type DPTStrat _ = PlutusTypeScott
