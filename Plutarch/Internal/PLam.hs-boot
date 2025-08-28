{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{- JUSTIFICATION: Some of the function closely related builtin type requires plam
Ideally we move all those utilities outside of builtin, but I'll do this to reduce amount of moving around required.
-}

module Plutarch.Internal.PLam (plam) where

import Data.Kind (Constraint, Type)
import GHC.Stack (HasCallStack)
import Plutarch.Internal.Term (S, Term, type (:-->))

type PLamN :: Type -> (S -> Type) -> S -> Constraint
class PLamN a b s | a -> b, s b -> a where
  plam ::
    forall (c :: S -> Type).
    HasCallStack =>
    (Term s c -> a) ->
    Term s (c :--> b)

instance {-# OVERLAPPABLE #-} a' ~ Term s a => PLamN a' a s
instance (a' ~ Term s a, PLamN b' b s) => PLamN (a' -> b') (a :--> b) s
