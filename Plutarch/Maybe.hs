module Plutarch.Maybe (
  PMaybe (PJust, PNothing),
  pfromMaybe,
) where

import qualified GHC.Generics as GHC
import Generics.SOP (Generic, I (I))
import Plutarch (
  PType,
  PlutusType,
  S,
  Term,
  perror,
  phoistAcyclic,
  plam,
  pmatch,
  type (:-->),
 )

-- | Plutus Maybe type, with Scott-encoded repr
data PMaybe (a :: PType) (s :: S)
  = PJust (Term s a)
  | PNothing
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PlutusType)

pfromMaybe :: Term s (PMaybe a :--> a)
pfromMaybe = phoistAcyclic $
  plam $ \maybe -> pmatch maybe $ \case
    PNothing -> perror
    PJust a -> a
