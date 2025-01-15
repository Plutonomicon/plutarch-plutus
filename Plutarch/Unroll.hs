module Plutarch.Unroll (punrollBound, punrollUnbound, punrollUnboundWhole) where

import Plutarch.Builtin.Data
import Plutarch.Builtin.Integer

import Plutarch.Internal.Fix
import Plutarch.Internal.Lift
import Plutarch.Internal.ListLike
import Plutarch.Internal.Numeric ()
import Plutarch.Internal.PLam
import Plutarch.Internal.Term

import Plutarch.Pretty

{- |
Unrolling a recursive function involves explicitly laying out some or all of the recursive steps, rather than relying on recursion via a fixed-point combinator.
In UPLC, the typical `pfix` implementation uses a Y-combinator under the hood. Each recursive step incurs additional evaluation costs (CPU and memory) due to
the execution of the Y-combinator. By eliminating these costs in each recursive step, unrolled functions reduce execution overhead. However, since each recursive
step is explicitly laid out, unrolled functions consume more script size.

There are various unrolling strategies available. It is important to carefully study the implications of each strategy, as they may impose different requirements,
such as hard limit on recursion depth.

@since WIP
-}
foo :: Integer -> (Term s (a :--> b) -> Term s (a :--> b)) -> Term s (a :--> b)
foo 0 _ = perror
foo d f = f (foo (d - 1) f)

{- |
The first argument specifies the unrolling depth.
The second argument defines the fallback behavior when the recursion depth exceeds the provided unrolling depth.

The fixed-point implementation provided requires a Haskell-level value @c@ and a Plutarch function of type `Term s (a :--> b)`. The functional for the recursion is passed as a Haskell function.
The inclusion of the additional, arbitrary Haskell value (typed @c@) enables further optimization by allowing pre-computation of constant values that depend only on the recursion depth.

@since WIP
-}
punrollBound ::
  forall a b c s.
  Integer ->
  Term s (a :--> b) ->
  ((c -> Term s (a :--> b)) -> c -> Term s (a :--> b)) ->
  c ->
  Term s (a :--> b)
punrollBound 0 def _ _ = def
punrollBound d def f c = f (punrollBound (d - 1) def f) c

{- |
Unroll given amount of steps, and for rest, uses `pfix` to support unbound recursion.

@since WIP
-}
punrollUnbound :: forall a b s. Integer -> (Term s (a :--> b) -> Term s (a :--> b)) -> Term s (a :--> b)
punrollUnbound 0 f = pfix #$ plam f
punrollUnbound d f = f (punrollUnbound (d - 1) f)

{- |
Uses `pfix` to recurse unrolled function itself. Unlike @punrollUnbound@, this function uses unrolled instructions
within `pfix` recursions.

This should perform better than @punrollUnbound@ when a function requires a large recursion depth.

@since WIP
-}
punrollUnboundWhole :: forall a b s. Integer -> (Term s (a :--> b) -> Term s (a :--> b)) -> Term s (a :--> b)
punrollUnboundWhole d f = pfix #$ plam $ \r -> punrollBound d r (\g () -> f (g ())) ()
