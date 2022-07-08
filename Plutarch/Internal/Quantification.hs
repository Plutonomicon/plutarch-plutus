module Plutarch.Internal.Quantification (PForall (PForall), PSome (PSome), PFix (PFix)) where

import Plutarch.Internal (PType, Term)

type PForall :: (a -> PType) -> PType
newtype PForall (b :: a -> PType) s = PForall (forall (x :: a). Term s (b x))

type PSome :: (a -> PType) -> PType
data PSome (b :: a -> PType) s = forall (x :: a). PSome (Term s (b x))

type PFix :: (PType -> PType) -> PType
newtype PFix f s = PFix (Term s (f (PFix f)))
