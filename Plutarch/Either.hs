module Plutarch.Either (PEither (..)) where

import Plutarch (PlutusType (PInner, pcon', pmatch'))
import Plutarch.Prelude

data PEither (a :: PType) (b :: PType) (s :: S) = PLeft (Term s a) | PRight (Term s b)

instance PlutusType (PEither a b) where
  type PInner (PEither a b) c = (a :--> c) :--> (b :--> c) :--> c
  pcon' (PLeft x) = plam $ \f (_ :: Term _ _) -> f # x
  pcon' (PRight y) = plam $ \_ g -> g # y
  pmatch' p f = p # plam (f . PLeft) # plam (f . PRight)
