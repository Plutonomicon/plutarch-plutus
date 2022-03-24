module Plutarch.Maybe (
  PMaybe (PJust, PNothing),
  pfromJust,
  pmaybe,
) where

import qualified GHC.Generics as GHC
import Generics.SOP (Generic, HasDatatypeInfo, I (I))
import Plutarch (
  PType,
  PlutusType,
  S,
  Term,
  perror,
  phoistAcyclic,
  plam,
  pmatch,
  (#),
  type (:-->),
 )
import Plutarch.Bool (PEq)
import Plutarch.Show (PShow)

-- | Plutus Maybe type, with Scott-encoded repr
data PMaybe (a :: PType) (s :: S)
  = PJust (Term s a)
  | PNothing
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, HasDatatypeInfo, PlutusType, PEq, PShow)

{- |
 fallible unwrapping from @PMaybe@
-}
pfromJust :: Term s (PMaybe a :--> a)
pfromJust = phoistAcyclic $
  plam $ \maybe -> pmatch maybe $ \case
    PNothing -> perror
    PJust a -> a

-- | 'PMaybe' destructor equivalent to 'maybe'
pmaybe :: Term (s :: S) (b :--> (a :--> b) :--> PMaybe a :--> b)
pmaybe = phoistAcyclic $
  plam $ \nothing just maybe -> pmatch maybe $ \case
    PNothing -> nothing
    PJust a -> just # a
