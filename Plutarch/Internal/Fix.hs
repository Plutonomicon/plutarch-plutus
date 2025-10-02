module Plutarch.Internal.Fix (pfixHoisted, pfix, pfixInline) where

import Data.Kind (Type)
import Plutarch.Builtin.Opaque (POpaque)
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.Term (
  S,
  Term,
  phoistAcyclic,
  plam',
  punsafeCoerce,
  (#),
  (:-->),
 )

{- | Fixpoint recursion, used to encode recursive functions.

= Note

This fixpoint combinator is /hoisted/, which allows for the smallest code
size. However, in terms of execution units, 'pfixHoisted' is the least
efficient.

'pfixHoisted' used to be the default fixpoint combinator in Plutarch. If you
used 'pfix' before, and want to maintain identical behaviour, use this
function.

= Example

Additional examples can be found in @examples/Recursion.hs@.

> iterateN' ::
>  Term s (PInteger :--> (a :--> a) :--> a :--> a) ->
>  Term s PInteger ->
>  Term s (a :--> a) ->
>  Term s a
> iterateN' self n f x =
>    pif (n #== 0)
>        x
>        (self # n - 1 #$ f x)
>
> iterateN :: Term s (PInteger :--> (a :--> a) :--> a :--> a)
> iterateN = pfixHoisted #$ plam iterateN'

@since 1.12.0
-}
pfixHoisted :: Term s (((a :--> b) :--> a :--> b) :--> a :--> b)
pfixHoisted = phoistAcyclic $
  punsafeCoerce $
    plam' $ \f ->
      plam' (\(x :: Term s POpaque) -> f # plam' (\(v :: Term s POpaque) -> punsafeCoerce x # x # v))
        # punsafeCoerce (plam' $ \(x :: Term s POpaque) -> f # plam' (\(v :: Term s POpaque) -> punsafeCoerce x # x # v))

{- | As 'pfixHoisted', but not hoisted. This is more efficient in terms of
execution units, but takes up more script space.

@since 1.12.0
-}
pfix ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  (Term s (a :--> b) -> Term s (a :--> b)) ->
  Term s (a :--> b)
pfix f =
  plam (\r -> punsafeCoerce r # r)
    # plam (\r -> f (punsafeCoerce r # r))

{- | As 'pfix', but we perform some additional inlining into the function
argument. This allows for even more speed, but at the cost of larger scripts.

@since 1.12.0
-}
pfixInline ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  (Term s (a :--> b) -> Term s (a :--> b)) ->
  Term s (a :--> b)
pfixInline f =
  plam (\r -> f (punsafeCoerce r # r))
    # plam (\r -> f (punsafeCoerce r # r))
