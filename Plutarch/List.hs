-- | Scott-encoded lists and ListLike typeclass
module Plutarch.List (
  PList (..),
  PListLike (..),
  PIsListLike,
  pconvertLists,

  -- * Comparison
  plistEquals,

  -- * Query
  pelem,
  plength,
  punsafeIndex,
  pdrop,

  -- * Construction
  psingleton,

  -- * Combine
  pconcat,
  pzipWith,
  pzipWith',
  pzip,

  -- * Traversals
  pmap,
  pfilter,

  -- * Catamorphisms
  precList,
  pfoldr,
  pfoldr',
  pfoldrLazy,
  pfoldl,
  pfoldl',

  -- * Special Folds
  pall,
  pany,
) where

import Numeric.Natural (Natural)

import Plutarch (PInner, PlutusType, pcon', pmatch')
import Plutarch.Bool (PBool (PFalse, PTrue), PEq, pif, (#&&), (#==), (#||))
import Plutarch.Integer (PInteger)
import Plutarch.Lift (pconstant)
import Plutarch.Pair (PPair (PPair))
import Plutarch.Prelude

import Data.Kind

data PList (a :: PType) (s :: S)
  = PSCons (Term s a) (Term s (PList a))
  | PSNil

instance PlutusType (PList a) where
  type PInner (PList a) c = (a :--> PList a :--> c) :--> PDelayed c :--> c

  pcon' :: forall s. PList a s -> forall b. Term s (PInner (PList a) b)
  pcon' (PSCons x xs) = plam $ \match_cons (_ :: Term _ _) -> match_cons # x # xs
  pcon' PSNil = plam $ \_match_cons match_nil -> pforce match_nil
  pmatch' xs f =
    xs # plam (\x xs -> f (PSCons x xs)) # pdelay (f PSNil)

instance PEq a => PEq (PList a) where
  (#==) xs ys = plistEquals # xs # ys

--------------------------------------------------------------------------------

-- | 'PIsListLike list a' constraints 'list' be a 'PListLike' with valid element type, 'a'.
type PIsListLike list a = (PListLike list, PElemConstraint list a)

-- | Plutarch types that behave like lists.
class PListLike (list :: (PType) -> PType) where
  type PElemConstraint list (a :: PType) :: Constraint

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
  pnull = phoistAcyclic $ plam $ pelimList (\_ _ -> pconstant False) $ pconstant True

instance PListLike PList where
  type PElemConstraint PList _ = ()
  pelimList match_cons match_nil ls = pmatch ls $ \case
    PSCons x xs -> match_cons x xs
    PSNil -> match_nil
  pcons = phoistAcyclic $ plam $ \x xs -> pcon (PSCons x xs)
  pnil = pcon PSNil

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

-- | / O(n) /. Check if element is in the list
pelem :: (PIsListLike list a, PEq a) => Term s (a :--> list a :--> PBool)
pelem =
  phoistAcyclic $
    plam $ \needle ->
      precList
        (\self x xs -> pif (x #== needle) (pcon PTrue) (self # xs))
        (\_self -> pcon PFalse)

-- | / O(n) /. Count the number of elements in the list
plength :: PIsListLike list a => Term s (list a :--> PInteger)
plength = phoistAcyclic $
  plam $ \xs ->
    let go :: PIsListLike list a => Term s (list a :--> PInteger :--> PInteger)
        go = (pfix #$ plam $ \self ls n -> pelimList (\_ xs -> self # xs # n + 1) n ls)
     in go # xs # 0

{- |
  Unsafely index a BuiltinList,
  throwing an error if the index is out of bounds.
-}
punsafeIndex :: (PIsListLike list a) => Natural -> Term s (list a) -> Term s a
punsafeIndex n xs = phead # (pdrop n xs)

{- |
  Drop the first n fields of a List.

  The term will be statically generated as
  repeated applications of 'ptail', which will be more
  efficient in many circumstances.
-}
pdrop :: (PIsListLike list a) => Natural -> Term s (list a) -> Term s (list a)
pdrop n xs = (phoistAcyclic $ plam $ \x -> pdrop' n x) # xs
  where
    pdrop' 0 xs' = xs'
    pdrop' n' xs' = pdrop' (n' - 1) (ptail # xs')

--------------------------------------------------------------------------------

-- | / O(n) /. Fold on a list left-associatively.
pfoldl :: PIsListLike list a => Term s ((b :--> a :--> b) :--> b :--> list a :--> b)
pfoldl = phoistAcyclic $
  plam $ \f ->
    pfix #$ plam $ \self z l ->
      pelimList
        (\x xs -> self # (f # z # x) # xs)
        z
        l

-- | The same as 'pfoldl', but with Haskell-level reduction function.
pfoldl' :: PIsListLike list a => (forall s. Term s b -> Term s a -> Term s b) -> Term s (b :--> list a :--> b)
pfoldl' f = phoistAcyclic $
  pfix #$ plam $ \self z l ->
    pelimList
      (\x xs -> self # f z x # xs)
      z
      l

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
    precList (\self x xs -> predicate # x #&& self # xs) (const $ pconstant True)

-- | / O(n) /. Check that predicate holds for any element in a list.
pany :: PIsListLike list a => Term s ((a :--> PBool) :--> list a :--> PBool)
pany = phoistAcyclic $
  plam $ \predicate ->
    precList (\self x xs -> predicate # x #|| self # xs) (const $ pconstant False)

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

{- | / O(min(n, m)) /. Zip two lists together, creating pairs of the elements.

If the lists are of differing lengths, cut to the shortest.
-}
pzip ::
  ( PListLike list
  , PElemConstraint list a
  , PElemConstraint list b
  , PElemConstraint list (PPair a b)
  ) =>
  Term s (list a :--> list b :--> list (PPair a b))
pzip = phoistAcyclic $ pzipWith' $ \x y -> pcon (PPair x y)

-- | / O(min(n, m)) /. Check if two lists are equal.
plistEquals :: (PIsListLike list a, PEq a) => Term s (list a :--> list a :--> PBool)
plistEquals =
  phoistAcyclic $
    pfix #$ plam $ \self xlist ylist ->
      pelimList
        ( \x xs ->
            pelimList (\y ys -> pif (x #== y) (self # xs # ys) (pconstant False)) (pconstant False) ylist
        )
        (pelimList (\_ _ -> pconstant False) (pconstant True) ylist)
        xlist
