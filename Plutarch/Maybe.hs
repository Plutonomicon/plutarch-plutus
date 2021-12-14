module Plutarch.Maybe (PMaybe (..)) where

import Plutarch (PlutusType (PInner, pcon', pmatch'))
import Plutarch.Prelude

data PMaybe (a :: k -> Type) (s :: k) = PJust (Term s a) | PNothing

-- Why do we need this?
plam2 :: (Term s a -> Term s b -> Term s c) -> Term s (a :--> b :--> c)
plam2 = plam

-- Why do we need this?
plam1 :: (Term s a -> Term s b) -> Term s (a :--> b)
plam1 = plam

instance PlutusType (PMaybe a) where
  type PInner (PMaybe a) b = (a :--> b) :--> PDelayed b :--> b
  pcon' (PJust x) = plam2 $ \f _ -> f £ x
  pcon' PNothing = plam2 $ \_ g -> pforce g
  pmatch' x f = x £ (plam1 $ \inner -> f (PJust inner)) £ (pdelay $ f PNothing)
