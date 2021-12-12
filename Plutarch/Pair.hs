{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Pair (PPair(..)) where
  
import Plutarch.Prelude
import Plutarch (PlutusType(PInner, pCon', pMatch'))

data PPair (a :: k -> Type) (b :: k -> Type) (s :: k) = PPair (Term s a) (Term s b)

instance PlutusType (PPair a b) where
  type PInner (PPair a b) c = (a :--> b :--> c) :--> c
  pCon' (PPair x y) = pLam $ \f -> f £ x £ y
  pMatch' p f = p £$ pLam2 $ \x y -> f (PPair x y)
