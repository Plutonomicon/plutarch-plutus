module Plutarch.Maybe (
  PMaybe (PJust, PNothing),
  pfromJust,
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
import Plutarch.Bool (PEq)

-- | Plutus Maybe type, with Scott-encoded repr
data PMaybe (a :: PType) (s :: S)
  = PJust (Term s a)
  | PNothing
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PlutusType, PEq)

{- |
 fallible unwrapping from @PMaybe@
-}
pfromJust :: Term s (PMaybe a :--> a)
pfromJust = phoistAcyclic $
  plam $ \maybe -> pmatch maybe $ \case
    PNothing -> perror
    PJust a -> a
