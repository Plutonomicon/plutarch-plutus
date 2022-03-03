module Plutarch.Maybe (PMaybe (..)) where

import qualified GHC.Generics as GHC
import Generics.SOP (Generic, I (I))
import Plutarch (
  PType,
  PlutusType,
  S,
  Term,
 )
import Plutarch.Bool (PEq)

-- | Plutus Maybe type, with Scott-encoded repr
data PMaybe (a :: PType) (s :: S)
  = PJust (Term s a)
  | PNothing
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PlutusType, PEq)
