module Plutarch.Internal.Quantification (PForall (PForall), PSome (PSome), PFix (PFix)) where

import Data.Kind (Type)
import Plutarch.Internal.Term (S, Term)

type PForall :: (a -> (S -> Type)) -> (S -> Type)
newtype PForall (b :: a -> (S -> Type)) s = PForall (forall (x :: a). Term s (b x))

type PSome :: (a -> (S -> Type)) -> (S -> Type)
data PSome (b :: a -> (S -> Type)) s = forall (x :: a). PSome (Term s (b x))

type PFix :: ((S -> Type) -> (S -> Type)) -> (S -> Type)
newtype PFix f s = PFix (Term s (f (PFix f)))
