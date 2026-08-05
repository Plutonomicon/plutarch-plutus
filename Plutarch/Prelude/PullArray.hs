module Plutarch.Prelude.PullArray (
  -- * Type
  PPullArray,

  -- * Functions
  piota,
  pgenerate,
  pfromArray,
  pfromList,
  pmapArray,
  pimapArray,
  ptakeArray,
) where

import Data.Kind (Type)
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (
  Term,
  papp,
  pcompose,
  plam',
  punsafeCoerce,
 )
import Plutarch.Primitive.Apply (PlutarchType (PRepresentation), (#))
import Plutarch.Primitive.Array (PBArray)
import Plutarch.Primitive.BuiltinFun (pindexArray, plengthOfArray, plistToArray)
import Plutarch.Primitive.Con (PCon (pcon'), pcon)
import Plutarch.Primitive.Function ((:-->))
import Plutarch.Primitive.List (PBList)
import Plutarch.Primitive.Match (PMatch (pmatch'), pmatch)
import Plutarch.Primitive.Numeric (PNatural)
import Plutarch.Primitive.Ord (pmin)

-- | @since wip
data PPullArray (a :: S -> Type) (s :: S)
  = PPullArray (Term s PNatural) (Term s (PNatural :--> a))

-- | @since wip
instance PlutarchType a => PlutarchType (PPullArray a) where
  type PRepresentation (PPullArray a) = PPullArray (PRepresentation a)

-- | @since wip
instance PlutarchType a => PMatch (PPullArray a) where
  pmatch' ::
    forall (b :: S -> Type) (s :: S).
    Term s (PPullArray (PRepresentation a)) ->
    (PPullArray a s -> Term s b) ->
    Term s b
  pmatch' t f =
    let tAsMS = punsafeCoerce @_ @((PNatural :--> (PNatural :--> PRepresentation a) :--> b) :--> b) t
     in papp tAsMS $ plam' $ \len -> plam' $ \ix -> f (PPullArray len . punsafeCoerce $ ix)

-- | @since wip
instance PlutarchType a => PCon (PPullArray a) where
  pcon' ::
    forall (s :: S).
    PPullArray a s ->
    Term s (PPullArray (PRepresentation a))
  pcon' (PPullArray len ix) = punsafeCoerce $ plam' $ \cont ->
    cont # len # ix

{- | Given a length @n@, construct the pull array equivalent of @[0, 1, ... n -
1]@.

\(Theta(1)\) space and time complexity.

@since wip
-}
piota :: forall (s :: S). Term s PNatural -> Term s (PPullArray PNatural)
piota n = pcon $ PPullArray n (plam' id)

{- | Given a length and a function from indexes to values, construct the pull
array of that length, each of whose indexes stores the value computed by that
function on that index.

\(Theta(1)\) space and time complexity.

@since wip
-}
pgenerate ::
  forall (a :: S -> Type) (s :: S).
  PlutarchType a =>
  Term s PNatural ->
  Term s (PNatural :--> a) ->
  Term s (PPullArray a)
pgenerate len f = pcon $ PPullArray len f

{- | Given a builtin array, construct the equivalent pull array.

\(\Theta(1)\) space and time complexity.

@since wip
-}
pfromArray ::
  forall (a :: S -> Type) (s :: S).
  PlutarchType a =>
  Term s (PBArray a) ->
  Term s (PPullArray a)
pfromArray arr = pcon $ PPullArray (plengthOfArray # arr) (pindexArray # arr)

{- | Given a builtin list, construct the equivalent pull array. Uses
'plistToArray' internally.

\(\Theta(n)\) space and time complexity.

@since wip
-}
pfromList ::
  forall (a :: S -> Type) (s :: S).
  PlutarchType a =>
  Term s (PBList a) ->
  Term s (PPullArray a)
pfromList ell = pfromArray (plistToArray # ell)

{- | Given a \'transformation function\' and a pull array, construct a new pull
array where each element of the argument array has been transformed without
moving it.

\(\Theta(1)\) space and time complexity.

@since wip
-}
pmapArray ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  (PlutarchType a, PlutarchType b) =>
  Term s (a :--> b) ->
  Term s (PPullArray a) ->
  Term s (PPullArray b)
pmapArray f arr = pmatch arr $ \(PPullArray len g) ->
  pcon $ PPullArray len (pcompose f g)

{- | As 'pmapArray', but with an index-aware \'transformer function\'.

@since wip
-}
pimapArray ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  (PlutarchType a, PlutarchType b) =>
  Term s (PNatural :--> a :--> b) ->
  Term s (PPullArray a) ->
  Term s (PPullArray b)
pimapArray f arr = pmatch arr $ \(PPullArray len g) ->
  pcon $ PPullArray len (plam' $ \i -> pcompose (f # i) g # i)

{- | Given a size limit \(k\) and a pull array of length \(n\), construct a new
pull array that consists of the first \(\min \{k, n\}\) elements of the
argument pull array, at the same indexes.

\(\Theta(1)\) space and time complexity.

@since wip
-}
ptakeArray ::
  forall (a :: S -> Type) (s :: S).
  PlutarchType a =>
  Term s PNatural ->
  Term s (PPullArray a) ->
  Term s (PPullArray a)
ptakeArray lim arr = pmatch arr $ \(PPullArray len ix) ->
  pcon $ PPullArray (pmin len lim) ix
