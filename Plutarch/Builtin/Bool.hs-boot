module Plutarch.Builtin.Bool (PBool (PTrue, PFalse), pif', ptrue, pfalse) where

import Data.Kind (Type)
import Plutarch.Internal.Term (S, Term, (:-->))

type role PBool phantom
type PBool :: S -> Type
data PBool (s :: S) = PTrue | PFalse

{- | Strict if-then-else. Emits slightly less code than the lazy version.

@since WIP
-}
pif' ::
  forall (a :: S -> Type) (s :: S).
  Term s (PBool :--> a :--> a :--> a)
ptrue :: Term s PBool
pfalse :: Term s PBool
