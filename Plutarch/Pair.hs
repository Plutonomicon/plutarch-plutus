{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Pair (PPair(..)) where
  
import Plutarch.Prelude
import Plutarch (PlutusType(PInner, pcon', pmatch'))

data PPair (a :: k -> Type) (b :: k -> Type) (s :: k) = PPair (Term s a) (Term s b)

instance PlutusType (PPair a b) where
  type PInner (PPair a b) c = (a :--> b :--> c) :--> c
  pcon' (PPair x y) = plam $ \f -> f £ x £ y
  pmatch' p f = p £$ plam2 $ \x y -> f (PPair x y)
