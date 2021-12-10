module Plutarch.Pair (PPair(..)) where

import Plutarch.Prelude
import Plutarch (PlutusType(PInner, pCon', pMatch'))

data PPair a b = PPair (Term a) (Term b)

instance PlutusType (PPair a b) where
  type PInner (PPair a b) c = (a :--> b :--> c) :--> c
  pCon' (PPair x y) = pLam $ \f -> f £ x £ y
  pMatch' :: Term (PInner (PPair a b) c) -> (PPair a b -> Term c) -> Term c
  pMatch' p f = p £$ pLam2 $ \x y -> f (PPair x y)
