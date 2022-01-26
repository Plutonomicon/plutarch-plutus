module Plutarch.Either (PEither (..)) where

import qualified GHC.Generics as GHC
import Generics.SOP
import Plutarch

data PEither (a :: PType) (b :: PType) (s :: S)
  = PLeft (Term s a)
  | PRight (Term s b)
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)

instance PlutusType (PEither a b) where
  pcon' x = gpcon @(PEither a b) $ from x
  pmatch' x f = gpmatch @(PEither a b) x (f . to)

{-
-- | Specialized
_pconEither :: forall s a b c. NP (Term s) '[a :--> c, b :--> c] -> PEither a b s -> Term s c
_pconEither (c1 :* c2 :* Nil) = \case
  PLeft x -> c1 # x
  PRight y -> c2 # y

-- TODO: generalize this further
_pmatchEither :: forall s c a b. (forall b'. Term s (PInner (PEither a b) b')) -> (PEither a b s -> Term s c) -> Term s c
_pmatchEither scott f =
  scott # plam (f . PLeft) # plam (f . PRight)

_pmatchEither' :: forall s c a b. (forall b'. Term s (PInner (PEither a b) b')) -> (PEither a b s -> Term s c) -> Term s c
_pmatchEither' scott f =
  scott `appL` (plam (f . PLeft) :* plam (f . PRight) :* Nil)

_pmatchEither'' :: forall s c a b . Term s (PInner (PEither a b) c) -> (PEither a b s -> Term s c) -> Term s c
_pmatchEither'' scott f =
  -- scott `appL` (plam (f . PLeft) :* plam (f . PRight) :* Nil)
  gpmatch @a @s @c scott (f . to)
-}
