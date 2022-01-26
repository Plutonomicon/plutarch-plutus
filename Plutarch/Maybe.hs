module Plutarch.Maybe (PMaybe (..)) where

import qualified GHC.Generics as GHC
import Generics.SOP
import Plutarch (
  PType,
  PlutusType,
  S,
  Term,
  gpcon,
  gpmatch,
  pcon',
  pmatch',
 )

-- | Plutus Maybe type, with Scott-encoded repr
data PMaybe (a :: PType) (s :: S)
  = PJust (Term s a)
  | PNothing
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)

instance PlutusType (PMaybe a) where
  pcon' x = gpcon @(PMaybe a) $ from x
  pmatch' x f' = gpmatch @(PMaybe a) x (f' . to)
