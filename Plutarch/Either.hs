module Plutarch.Either (PEither) where

import Plutarch.Prelude
import Plutarch (PlutusType(PInner, pCon', pMatch'))

data PEither a b = PLeft (Term a) | PRight (Term b)

instance PlutusType (PEither a b) where
  type PInner (PEither a b) c = (a :--> c) :--> (b :--> c) :--> c
  pCon' (PLeft x) = pLam2 $ \(f, _) -> f £ x
  pCon' (PRight y) = pLam2 $ \(_, g) -> g £ y
  pMatch' :: Term (PInner (PEither a b) c) -> (PEither a b -> Term c) -> Term c
  pMatch' p f = p £ (pLam $ \x -> f . PLeft $ x) £ (pLam $ \y -> f . PRight $ y)
