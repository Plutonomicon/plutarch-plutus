-- | Scott-encoded lists and ListLike typeclass
module Plutarch.List (
  PScottList (..),
  PListLike (..),
  pconvertLists,

  -- * Comparison
  plistEquals,

  -- * Query
  pelem,
  plength,
  pnull,

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
  pall,
) where

import Plutarch
import Plutarch.Bool (PBool (..), PEq (..), pif, (#&&))
import Plutarch.Integer (PInteger)
import Plutarch.Pair (PPair (..))
import Plutarch.Prelude

import Data.Kind

data PScottList (a :: k -> Type) (s :: k)
  = PSCons (Term s a) (Term s (PScottList a))
  | PSNil

instance PlutusType (PScottList a) where
  type PInner (PScottList a) c = (a :--> PScottList a :--> c) :--> (PDelayed c) :--> c

  pcon' :: forall s. PScottList a s -> forall b. Term s (PInner (PScottList a) b)
  pcon' (PSCons x xs) = plam $ \match_cons (_ :: Term _ _) -> match_cons # x # xs
  pcon' PSNil = plam $ \_match_cons match_nil -> pforce match_nil
  pmatch' xs f =
    xs # (plam $ \x xs -> f (PSCons x xs)) # pdelay (f PSNil)

instance PEq a => PEq (PScottList a) where
  (#==) xs ys = plistEquals # xs # ys

--------------------------------------------------------------------------------

-- | Plutarch types that behave like lists.
class PListLike (list :: (k -> Type) -> k -> Type) where
  type PElemConstraint list :: (k -> Type) -> Constraint

  -- | Canonical eliminator for list-likes.
  --
  -- Example:
  -- > isEmpty :: PIsListLike list a => Term s (list a :--> PBool)
  -- > isEmpty = pelimList (plam $ \ _x _xs -> pcon PFalse) (pdelay $ pcon PTrue)
  pelimList :: PElemConstraint list a => Term s (a :--> list a :--> r) -> Term s (PDelayed r) -> Term s (list a :--> r)

  -- | / O(1) /. Cons an element onto an existing list.
  pconsList :: PElemConstraint list a => Term s (a :--> list a :--> list a)

  -- | / O(1) /. The empty list
  pnilList :: PElemConstraint list a => Term s (list a)

  -- | / O(1) /. Return the first element of a list. Partial, throws an error upon encountering an empty list.
  punsafeHead :: PIsListLike list a => Term s (list a :--> a)
  punsafeHead = pelimList (plam $ \x _xs -> x) (pdelay perror)

  -- | / O(1) /. Take the tail of a list, meaning drop its head. Partial, throws an error upon encountering an empty list.
  punsafeTail :: PIsListLike list a => Term s (list a :--> list a)
  punsafeTail = pelimList (plam $ \_x xs -> xs) (pdelay perror)


class EmptyConstraint x
instance EmptyConstraint x

instance PListLike PScottList where
  type PElemConstraint PScottList = EmptyConstraint
  pelimList match_cons match_nil =
    plam $ \ls -> pmatch ls $ \case
      PSCons x xs -> match_cons # x # xs
      PSNil -> pforce match_nil
  pconsList = plam $ \x xs -> pcon (PSCons x xs)
  pnilList = pcon PSNil

type PIsListLike list a = (PListLike list, PElemConstraint list a)


-- | / O(n) /. Convert from any ListLike to any ListLike, provided both lists' element constraints are met.
pconvertLists :: forall f g a s. (PElemConstraint f a, PElemConstraint g a, PListLike f, PListLike g) => Term s (f a :--> g a)
pconvertLists =
  pfix #$ plam $ \self ->
    pelimList
      (plam $ \x xs -> pconsList # x # (self # xs))
      (pdelay $ pnilList)

-- | Like 'pelimList', but with a fixpoint recursion hatch.
precList :: (PElemConstraint list a, PListLike list) => (Term s (list a :--> r) -> Term s a -> Term s (list a) -> Term s r) -> (Term s (list a :--> r) -> Term s r) -> Term s (list a :--> r)
precList mcons mnil =
  pfix #$ plam $ \self ->
    pelimList
      (plam $ \x xs -> mcons self x xs)
      (pdelay $ mnil self)

--------------------------------------------------------------------------------
-- Construction

-- | / O(1) /. Create a singleton list from an element
psingleton :: PIsListLike list a => Term s (a :--> list a)
psingleton = plam $ \x -> pconsList # x # pnilList

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
plength =
  plet
    ( pfix #$ plam $ \self ls n ->
        pelimList
          (plam $ \_x xs -> (self # xs # n + 1))
          (pdelay n)
          # ls
    )
    $ \go -> plam $ \xs -> go # xs # 0

-- | / O(1) /. Check if a list is empty
pnull :: PIsListLike list a => Term s (list a :--> PBool)
pnull = pelimList (plam $ \_ _ -> pcon PFalse) (pdelay $ pcon PTrue)

--------------------------------------------------------------------------------

-- | / O(n) /. Fold on a list right-associatively
pfoldr :: PIsListLike list a => Term s ((a :--> b :--> b) :--> b :--> list a :--> b)
pfoldr = phoistAcyclic $
  plam $ \f z ->
    precList
      (\self x xs -> f # x # (self # xs))
      (\_self -> z)

-- | The same as 'pfoldr'', but with Haskell-level reduction function.
pfoldr' :: PIsListLike list a => (forall s. Term s a -> Term s b -> Term s b) -> Term s (b :--> list a :--> b)
pfoldr' f = phoistAcyclic $
  plam $ \z ->
    precList
      (\self x xs -> f x (self # xs))
      (\_self -> z)

-- | / O(n) /. Check that predicate holds for all elements in a list
pall :: PIsListLike list a => Term s ((a :--> PBool) :--> list a :--> PBool)
pall = phoistAcyclic $
  plam $ \predicate ->
    pfoldr # (plam $ \x acc -> predicate # x #&& acc) # (pcon PTrue)

-- | / O(n) /. Map a function over a list of elements
pmap :: (PListLike list, PElemConstraint list a, PElemConstraint list b) => Term s ((a :--> b) :--> list a :--> list b)
pmap = phoistAcyclic $
  plam $ \f ->
    precList (\self x xs -> pconsList # (f # x) # (self # xs)) (\_self -> pnilList)

-- | / O(n) /. Filter elements from a list that don't match the predicate.
pfilter :: PIsListLike list a => Term s ((a :--> PBool) :--> list a :--> list a)
pfilter =
  phoistAcyclic $
    plam $ \predicate ->
      precList
        ( \self x xs ->
            pif
              (predicate # x)
              (pconsList # x # (self # xs))
              (self # xs)
        )
        (\_self -> pnilList)

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
            pconsList # x # (self # xs)
        )
        (\_self -> ys)
        # xs

-- | / O(min(n, m)) /. Zip two lists together with a passed function. If the lists are of differing lengths, cut to the shortest.
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
          ( plam $ \x xs ->
              pelimList
                (plam $ \y ys -> pconsList # (f # x # y) # (self # xs # ys))
                (pdelay pnilList)
                # ly
          )
          (pdelay pnilList)
          # lx

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
      ( plam $ \x xs ->
          pelimList
            (plam $ \y ys -> pconsList # (f x y) # (self # xs # ys))
            (pdelay pnilList)
            # ly
      )
      (pdelay pnilList)
      # lx

-- | / O(min(n, m)) /. Zip two lists together, creating pairs of the elements. If the lists are of differing lengths, cut to the shortest.
pzip ::
  ( PListLike list
  , PElemConstraint list a
  , PElemConstraint list b
  , PElemConstraint list (PPair a b)
  ) =>
  Term s (list a :--> list b :--> list (PPair a b))
pzip = pzipWith' $ \x y -> pcon (PPair x y)

-- Horribly inefficient.
plistEquals :: (PIsListLike list a, PElemConstraint list PBool, PEq a) => Term s (list a :--> list a :--> PBool)
plistEquals =
  phoistAcyclic $ plam $ \xs ys ->
    plength # xs #== plength # ys
      #&& pfoldr' (#&&) # (pcon PTrue) # (pzipWith' (#==) # xs # ys)
