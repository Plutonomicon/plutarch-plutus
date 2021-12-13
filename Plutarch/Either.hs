module Plutarch.Either (PEither(..)) where

import Plutarch.Prelude
import Plutarch (PlutusType(PInner, pcon', pmatch'))

data PEither (a :: k -> Type) (b :: k -> Type) (s :: k) = PLeft (Term s a) | PRight (Term s b)

instance PlutusType (PEither a b) where
  type PInner (PEither a b) c = (a :--> c) :--> (b :--> c) :--> c
  pcon' (PLeft x) = plam2 $ \f _ -> f £ x
  pcon' (PRight y) = plam2 $ \_ g -> g £ y
  pmatch' p f = p £ (plam $ \x -> f . PLeft $ x) £ (plam $ \y -> f . PRight $ y)
