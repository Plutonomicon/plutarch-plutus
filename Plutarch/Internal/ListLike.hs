module Plutarch.Internal.ListLike (
  PIsListLike,
  PListLike,
  PElemConstraint,
  pelimList,
  pcons,
  pnil,
  phead,
  ptail,
  pnull,
  pconvertLists,
  precList,
  psingleton,
  plength,
  ptryIndex,
  pdrop,
  pfoldl',
  pfoldl,
  pfoldr',
  pfoldr,
  pfoldrLazy,
  pall,
  pany,
  pmap,
  pfilter,
  pconcat,
  pzipWith,
  pzipWith',
) where

import Data.Kind (Constraint, Type)

import Plutarch.Builtin.Bool (
  PBool,
  pfalse,
  pif,
  ptrue,
  (#&&),
  (#||),
 )
import Plutarch.Builtin.Data (
  PBuiltinList (PCons, PNil),
  pheadBuiltin,
  pnullBuiltin,
  ptailBuiltin,
 )
import Plutarch.Builtin.Integer (
  PInteger,
  paddInteger,
  pconstantInteger,
 )

import Plutarch.Internal.Fix (pfix)
import {-# SOURCE #-} Plutarch.Internal.Lift (PlutusRepr)
import Plutarch.Internal.PLam (PLamN (plam))
import Plutarch.Internal.PlutusType (pcon, pmatch)
import Plutarch.Internal.Term (
  PDelayed,
  S,
  Term,
  pdelay,
  perror,
  phoistAcyclic,
  plet,
  (#),
  (#$),
  type (:-->),
 )

import Numeric.Natural (Natural)
import PlutusCore qualified as PLC

-- | 'PIsListLike list a' constraints 'list' be a 'PListLike' with valid element type, 'a'.
type PIsListLike list a = (PListLike list, PElemConstraint list a)

-- | Plutarch types that behave like lists.
class PListLike (list :: (S -> Type) -> S -> Type) where
  type PElemConstraint list (a :: S -> Type) :: Constraint

  -- | Canonical eliminator for list-likes.
  pelimList ::
    PElemConstraint list a =>
    (Term s a -> Term s (list a) -> Term s r) ->
    Term s r ->
    Term s (list a) ->
    Term s r

  -- | Cons an element onto an existing list.
  pcons :: PElemConstraint list a => Term s (a :--> list a :--> list a)

  -- | The empty list
  pnil :: PElemConstraint list a => Term s (list a)

  -- | Return the first element of a list. Partial, throws an error upon encountering an empty list.
  phead :: PElemConstraint list a => Term s (list a :--> a)
  phead = phoistAcyclic $ plam $ pelimList const perror

  -- | Take the tail of a list, meaning drop its head. Partial, throws an error upon encountering an empty list.
  ptail :: PElemConstraint list a => Term s (list a :--> list a)
  ptail = phoistAcyclic $ plam $ pelimList (\_ xs -> xs) perror

  -- | / O(1) /. Check if a list is empty
  pnull :: PElemConstraint list a => Term s (list a :--> PBool)
  pnull = phoistAcyclic $ plam $ pelimList (\_ _ -> pfalse) ptrue

instance PListLike PBuiltinList where
  type PElemConstraint PBuiltinList a = (PLC.Contains PLC.DefaultUni (PlutusRepr a))

  pelimList match_cons match_nil ls = pmatch ls $ \case
    PCons x xs -> match_cons x xs
    PNil -> match_nil

  pcons = plam $ \x xs -> pcon (PCons x xs)
  pnil = pcon PNil
  phead = pheadBuiltin
  ptail = ptailBuiltin
  pnull = pnullBuiltin

-- | / O(n) /. Convert from any ListLike to any ListLike, provided both lists' element constraints are met.
pconvertLists ::
  forall f g a s.
  (PIsListLike f a, PIsListLike g a) =>
  Term s (f a :--> g a)
pconvertLists = phoistAcyclic $
  pfix #$ plam $ \self ->
    pelimList
      (\x xs -> pcons # x #$ self # xs)
      pnil

-- | Like 'pelimList', but with a fixpoint recursion hatch.
precList ::
  PIsListLike list a =>
  (Term s (list a :--> r) -> Term s a -> Term s (list a) -> Term s r) ->
  (Term s (list a :--> r) -> Term s r) ->
  Term s (list a :--> r)
precList mcons mnil =
  pfix #$ plam $ \self ->
    pelimList
      (mcons self)
      (mnil self)

--------------------------------------------------------------------------------
-- Construction

-- | / O(1) /. Create a singleton list from an element
psingleton :: PIsListLike list a => Term s (a :--> list a)
psingleton = phoistAcyclic $ plam $ \x -> pcons # x # pnil

--------------------------------------------------------------------------------
-- Querying

-- | / O(n) /. Count the number of elements in the list
plength :: PIsListLike list a => Term s (list a :--> PInteger)
plength =
  phoistAcyclic $
    let go :: PIsListLike list a => Term s (PInteger :--> list a :--> PInteger)
        go = pfix #$ plam $ \self n -> pelimList (\_ xs -> self # (paddInteger # n # pconstantInteger 1) # xs) n
     in go # pconstantInteger 0

-- | Index a BuiltinList, throwing an error if the index is out of bounds.
ptryIndex :: PIsListLike list a => Natural -> Term s (list a) -> Term s a
ptryIndex n xs = phead # pdrop n xs

{- |
  Drop the first n fields of a List.

  The term will be statically generated as
  repeated applications of 'ptail', which will be more
  efficient in many circumstances.
-}
pdrop :: PIsListLike list a => Natural -> Term s (list a) -> Term s (list a)
pdrop n xs = pdrop' n # xs
  where
    pdrop' :: PIsListLike list a => Natural -> (forall (s :: S). Term s (list a :--> list a))
    pdrop' 0 = plam id
    pdrop' 1 = ptail
    pdrop' n' = phoistAcyclic $ plam $ \x -> ptail #$ pdrop' (n' - 1) # x

--------------------------------------------------------------------------------

-- | / O(n) /. Fold on a list left-associatively.
pfoldl :: PIsListLike list a => Term s ((b :--> a :--> b) :--> b :--> list a :--> b)
pfoldl = phoistAcyclic $
  plam $ \f ->
    pfix #$ plam $ \self z ->
      pelimList
        (\x xs -> self # (f # z # x) # xs)
        z

-- | The same as 'pfoldl', but with Haskell-level reduction function.
pfoldl' :: PIsListLike list a => (forall s. Term s b -> Term s a -> Term s b) -> Term s (b :--> list a :--> b)
pfoldl' f = phoistAcyclic $
  pfix #$ plam $ \self z ->
    pelimList
      (\x xs -> self # f z x # xs)
      z

-- | / O(n) /. Fold on a list right-associatively.
pfoldr :: PIsListLike list a => Term s ((a :--> b :--> b) :--> b :--> list a :--> b)
pfoldr = phoistAcyclic $
  plam $ \f z ->
    precList
      (\self x xs -> f # x # (self # xs))
      (const z)

-- | The same as 'pfoldr'', but with Haskell-level reduction function.
pfoldr' :: PIsListLike list a => (forall s. Term s a -> Term s b -> Term s b) -> Term s (b :--> list a :--> b)
pfoldr' f = phoistAcyclic $
  plam $ \z ->
    precList
      (\self x xs -> f x (self # xs))
      (const z)

{- | / O(n) /. Fold on a list right-associatively, with opportunity for short circuting.

May short circuit when given reducer function is lazy in its second argument.
-}
pfoldrLazy :: PIsListLike list a => Term s ((a :--> PDelayed b :--> b) :--> b :--> list a :--> b)
pfoldrLazy = phoistAcyclic $
  plam $ \f z ->
    precList
      (\self x xs -> f # x # pdelay (self # xs))
      (const z)

-- | / O(n) /. Check that predicate holds for all elements in a list.
pall :: PIsListLike list a => Term s ((a :--> PBool) :--> list a :--> PBool)
pall = phoistAcyclic $
  plam $ \predicate ->
    precList (\self x xs -> predicate # x #&& self # xs) (const ptrue)

-- | / O(n) /. Check that predicate holds for any element in a list.
pany :: PIsListLike list a => Term s ((a :--> PBool) :--> list a :--> PBool)
pany = phoistAcyclic $
  plam $ \predicate ->
    precList (\self x xs -> predicate # x #|| self # xs) (const pfalse)

-- | / O(n) /. Map a function over a list of elements
pmap :: (PListLike list, PElemConstraint list a, PElemConstraint list b) => Term s ((a :--> b) :--> list a :--> list b)
pmap = phoistAcyclic $
  plam $ \f ->
    precList (\self x xs -> pcons # (f # x) # (self # xs)) (const pnil)

-- | / O(n) /. Filter elements from a list that don't match the predicate.
pfilter :: PIsListLike list a => Term s ((a :--> PBool) :--> list a :--> list a)
pfilter =
  phoistAcyclic $
    plam $ \predicate ->
      precList
        ( \self x' xs -> plet x' $ \x ->
            pif
              (predicate # x)
              (pcons # x # (self # xs))
              (self # xs)
        )
        (const pnil)

--------------------------------------------------------------------------------

{- | / O(n) /. Concatenate two lists

 Example:
 > pconcat # psingleton x # psingleton y == plistLiteral [x, y]

 pconcat exhibits identities with empty lists such that
 > forall x. pconcat # pnil # x == x
 > forall x. pconcat # x # pnil == x
-}
pconcat :: PIsListLike list a => Term s (list a :--> list a :--> list a)
pconcat =
  phoistAcyclic $
    plam $ \xs ys ->
      precList
        ( \self x xs ->
            pcons # x # (self # xs)
        )
        (const ys)
        # xs

{- | / O(min(n, m)) /. Zip two lists together with a passed function.

If the lists are of differing lengths, cut to the shortest.
-}
pzipWith ::
  ( PListLike list
  , PElemConstraint list a
  , PElemConstraint list b
  , PElemConstraint list c
  ) =>
  Term s ((a :--> b :--> c) :--> list a :--> list b :--> list c)
pzipWith =
  phoistAcyclic $
    plam $ \f ->
      pfix #$ plam $ \self lx ly ->
        pelimList
          ( \x xs ->
              pelimList
                (\y ys -> pcons # (f # x # y) # (self # xs # ys))
                pnil
                ly
          )
          pnil
          lx

-- | Like 'pzipWith' but with Haskell-level merge function.
pzipWith' ::
  ( PListLike list
  , PElemConstraint list a
  , PElemConstraint list b
  , PElemConstraint list c
  ) =>
  (Term s a -> Term s b -> Term s c) ->
  Term s (list a :--> list b :--> list c)
pzipWith' f =
  pfix #$ plam $ \self lx ly ->
    pelimList
      ( \x xs ->
          pelimList
            (\y ys -> pcons # f x y # (self # xs # ys))
            pnil
            ly
      )
      pnil
      lx
