module Plutarch.Primitive.Function (
  pid,
  pcompose,
) where

import Data.Kind (Type)
import Plutarch.Backend.Term (S, Term, papp, plam', (:-->))

-- | @since wip
pid ::
  forall (a :: S -> Type) (s :: S).
  Term s (a :--> a)
pid = plam' id

-- | @since wip
pcompose ::
  forall (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (s :: S).
  Term s (b :--> c) ->
  Term s (a :--> b) ->
  Term s (a :--> c)
pcompose f g = plam' $ \x -> papp f (papp g x)
