module Plutarch.Pair (PPair, pPair) where

import Plutarch.Prelude

data PPair a b = PPair (Term a) (Term b)

pPair :: Term a -> Term b -> Term (PPair a b)
pPair (x :: Term a) (y :: Term b) = pCoerce $ ((pLam $ \f -> f £ x £ y) :: forall c. Term ((a :--> (b :--> c)) :--> c))

--pPairElim :: Term (PPair a b) -> (PPair a b -> Term b) -> Term b
--pPairElim =
