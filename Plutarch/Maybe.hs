module Plutarch.Maybe (PMaybe (..)) where

import qualified GHC.Generics as GHC
import Generics.SOP (Generic, I (I))
import Plutarch (
  PType,
  PlutusType,
  S,
  Term,
  pcon,
  phoistAcyclic,
  plam,
  pmatch,
  (#),
 )
import Plutarch.Bool (PBool (PFalse, PTrue), PEq ((#==)))

-- | Plutus Maybe type, with Scott-encoded repr
data PMaybe (a :: PType) (s :: S)
  = PJust (Term s a)
  | PNothing
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PlutusType)

instance PEq a => PEq (PMaybe a) where
  ma' #== mb' =
    phoistAcyclic
      ( plam $ \ma mb ->
          pmatch ma $ \case
            PNothing -> pmatch mb $ \case
              PNothing -> pcon PTrue
              PJust _ -> pcon PFalse
            PJust a -> pmatch mb $ \case
              PNothing -> pcon PFalse
              PJust b -> a #== b
      )
      # ma'
      # mb'
