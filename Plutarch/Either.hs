{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Plutarch.Either (PEither (..)) where

import qualified GHC.Generics as GHC
import Generics.SOP
import Plutarch
import Plutarch.Internal

data PEither (a :: PType) (b :: PType) (s :: S)
  = PLeft (Term s a)
  | PRight (Term s b)
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)

instance PlutusType (PEither a b) where
  -- type PInner (PEither a b) c = (a :--> c) :--> (b :--> c) :--> c
  pcon' :: forall s. PEither a b s -> forall c. Term s (PInner (PEither a b) c)
  pcon' x =
    -- plam' $ \c1 -> plam' $ \c2 -> f (c1 :* c2 :* Nil) val
    -- plamL $ \cs -> pconEither cs x
    gpcon @(PEither a b) @s @_ @(Code (PEither a b s)) $ from x

  -- pcon' (PLeft x) = plam' $ \c1 -> plam' $ \_ -> c1 # x
  -- pcon' (PRight y) = plam' $ \_ -> plam' $ \c2 -> c2 # y
  pmatch' p f = p # plam (f . PLeft) # plam (f . PRight)

-- | Specialized
_pconEither :: forall s a b c. NP (Term s) '[a :--> c, b :--> c] -> PEither a b s -> Term s c
_pconEither (c1 :* c2 :* Nil) = \case
  PLeft x -> c1 # x
  PRight y -> c2 # y
