module Plutarch.Either (PEither(..)) where

import Plutarch.Prelude
import Plutarch (PlutusType(PInner, pcon', pmatch'))

data PEither (a :: k -> Type) (b :: k -> Type) (s :: k) = PLeft (Term s a) | PRight (Term s b)

-- Why do we need this?
plam2 :: (Term s a -> Term s b -> Term s c) -> Term s (a :--> b :--> c)
plam2 = plam

instance PlutusType (PEither a b) where
  type PInner (PEither a b) c = (a :--> c) :--> (b :--> c) :--> c
  pcon' :: forall s. PEither a b s -> forall c. Term s ((a :--> c) :--> (b :--> c) :--> c)
  pcon' (PLeft x) = plam2 $ \f _ -> f £ x
  pcon' (PRight y) = plam $ \_ g -> g £ y
  pmatch' p f = p £ (plam $ \x -> f . PLeft $ x) £ (plam $ \y -> f . PRight $ y)
