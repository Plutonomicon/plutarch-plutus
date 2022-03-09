module Plutarch.Maybe (PMaybe (..)) where

import qualified GHC.Generics as GHC
import Generics.SOP (Generic, HasDatatypeInfo, I (I))
import Plutarch (
  PType,
  PlutusType,
  S,
  Term,
 )
import Plutarch.Bool (PEq)
import Plutarch.Show (PShow)

-- | Plutus Maybe type, with Scott-encoded repr
data PMaybe (a :: PType) (s :: S)
  = PJust (Term s a)
  | PNothing
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, HasDatatypeInfo, PlutusType, PEq, PShow)
