module Plutarch.Internal.Fix (pfix) where

import Plutarch.Builtin.Opaque
import Plutarch.Internal.Term

{- |
  Fixpoint recursion. Used to encode recursive functions.

  Example:

  > iterateN' ::
  >  Term s (PInteger :--> (a :--> a) :--> a :--> a) ->
  >  Term s PInteger ->
  >  Term s (a :--> a) ->
  >  Term s a
  > iterateN' self n f x =
  >   pif (n #== 0) x (self # n - 1 #$ f x)
  >
  > iterateN :: Term s (PInteger :--> (a :--> a) :--> a :--> a)
  > iterateN = pfix #$ plam iterateN'
  >

  Further examples can be found in examples/Recursion.hs
-}
pfix :: Term s (((a :--> b) :--> a :--> b) :--> a :--> b)
pfix = phoistAcyclic $
  punsafeCoerce $
    plam' $ \f ->
      plam' (\(x :: Term s POpaque) -> f # plam' (\(v :: Term s POpaque) -> punsafeCoerce x # x # v))
        # punsafeCoerce (plam' $ \(x :: Term s POpaque) -> f # plam' (\(v :: Term s POpaque) -> punsafeCoerce x # x # v))
