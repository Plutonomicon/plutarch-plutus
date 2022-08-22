module Plutarch.Extra.List (
  preverse,
  pcheckSortedBy,
  pcheckSorted,
  pmergeBy,
  pmsortBy,
  pmsort,
  pnubBy,
  pnub,
  pisUniqBy,
  pisUniq,
  pmapMaybe,
  pfind',
  pfirstJust,
  plookup,
  plookupTuple,
  preplicate,
  pisUniqBy',
  pisUniq',
) where

import Plutarch.Api.V1.Tuple (PTuple)
import Plutarch.Prelude

-- | / O(n) /. reverses a list
preverse :: (PIsListLike l a) => Term s (l a :--> l a)
preverse =
  phoistAcyclic $
    pfoldl # plam (\ys y -> pcons # y # ys) # pnil

-- | / O(n) /. Returns true if the given list is sorted.
pcheckSortedBy ::
  (PIsListLike list a) =>
  Term s ((a :--> a :--> PBool) :--> list a :--> PBool)
pcheckSortedBy = phoistAcyclic $
  pfix #$ plam $ \self comp l ->
    pelimList
      ( \x xs ->
          pelimList (\y _ -> (comp # x # y) #&& self # comp # xs) (pconstant True) xs
      )
      (pconstant True)
      l

-- | / O(n) /.checks whether a list is sorted
pcheckSorted :: (PIsListLike l a, POrd a) => Term s (l a :--> PBool)
pcheckSorted = phoistAcyclic $ pcheckSortedBy # plam (#<=)

{- | / O(n) /. Merge two lists which are assumed to be ordered, given a custom comparator.
    The comparator should return true if first value is less than the second one.
-}
pmergeBy ::
  forall (a :: S -> Type) (s :: S) list.
  (PIsListLike list a) =>
  Term
    s
    ( (a :--> a :--> PBool)
        :--> list a
        :--> list a
        :--> list a
    )
pmergeBy =
  phoistAcyclic $
    pfix #$ plam $ \self comp a b ->
      pelimList
        ( \x xs ->
            pelimList
              ( \y ys ->
                  pif
                    (comp # x # y)
                    (pcons # x #$ self # comp # xs # b)
                    (pcons # y #$ self # comp # a # ys)
              )
              a
              b
        )
        b
        a

{- | / O(n log n) /. Merge sort, bottom-up version, given a custom comparator.

   Assuming the comparator returns true if first value is less than the second one,
    the list elements will be arranged in ascending order, keeping duplicates in the order
    they appeared in the input.
-}
pmsortBy ::
  forall s a l.
  (PIsListLike l a, PIsListLike l (l a)) =>
  Term s ((a :--> a :--> PBool) :--> l a :--> l a)
pmsortBy = phoistAcyclic $
  plam $ \comp xs ->
    mergeAll # comp # (pmap # psingleton # xs)
  where
    mergeAll :: Term _ ((a :--> a :--> PBool) :--> l (l a) :--> l a)
    mergeAll = phoistAcyclic $
      pfix #$ plam $ \self comp l ->
        pelimList
          ( \x xs ->
              pif
                (pnull # xs)
                x
                (self # comp #$ mergePairs # comp # l)
          )
          pnil
          l
    mergePairs :: Term _ ((a :--> a :--> PBool) :--> l (l a) :--> l (l a))
    mergePairs = phoistAcyclic $
      pfix #$ plam $ \self comp l ->
        pelimList
          ( \x xs ->
              pelimList
                ( \y ys ->
                    pcons # (pmergeBy # comp # x # y) # (self # comp # ys)
                )
                l
                xs
          )
          pnil
          l

-- | A special case of 'pmsortBy' which requires elements have 'POrd' instance.
pmsort ::
  (POrd a, PIsListLike l a, PIsListLike l (l a)) =>
  Term s (l a :--> l a)
pmsort = phoistAcyclic $ pmsortBy # comp
  where
    comp = phoistAcyclic $ plam (#<)

-- | /O(n log n)/. Remove duplicate elements in a list with the given function to decide duplicate.
pnubBy ::
  forall (a :: S -> Type) (s :: S) list.
  (PIsListLike list a, PIsListLike list (list a)) =>
  Term
    s
    ( (a :--> a :--> PBool)
        :--> list a
        :--> list a
    )
pnubBy = phoistAcyclic $
  plam $ \eq l ->
    pelimList (\x xs -> go # eq # x # xs) l l
  where
    go = phoistAcyclic pfix #$ plam $ \self eq seen l ->
      pelimList
        ( \x xs ->
            pif
              (eq # x # seen)
              (self # eq # seen # xs)
              (pcons # seen #$ self # eq # x # xs)
        )
        (psingleton # seen)
        l

-- | Special version of 'pnub', which requires elements have 'POrd' instance.
pnub ::
  forall (a :: S -> Type) (s :: S) list.
  (PIsListLike list a, PIsListLike list (list a), POrd a) =>
  Term s (list a :--> list a)
pnub = phoistAcyclic $ pnubBy # eq
  where
    eq = phoistAcyclic $ plam (#==)

-- | / O(n log n) /. Check if a list contains no duplicates.
pisUniqBy ::
  forall (a :: S -> Type) (s :: S) list.
  (PIsListLike list a, PIsListLike list (list a)) =>
  Term
    s
    ( (a :--> a :--> PBool)
        :--> list a
        :--> PBool
    )
pisUniqBy = phoistAcyclic $
  plam $ \eq xs ->
    let nubbed = pnubBy # eq # xs
     in plength # xs #== plength # nubbed

-- | A special case of 'pisUniqBy' which requires elements have 'POrd' instance.
pisUniq ::
  forall (a :: S -> Type) (s :: S) list.
  (POrd a, PIsListLike list a, PIsListLike list (list a)) =>
  Term s (list a :--> PBool)
pisUniq = phoistAcyclic $ pisUniqBy # plam (#==)

-- | A special version of `pmap` which allows list elements to be thrown out.
pmapMaybe ::
  forall list (a :: S -> Type) (b :: S -> Type) (s :: S).
  (PIsListLike list a, PIsListLike list b) =>
  Term
    s
    ( (a :--> PMaybe b)
        :--> list a
        :--> list b
    )
pmapMaybe = phoistAcyclic $
  pfix #$ plam $ \self f l ->
    pelimList
      ( \x xs -> pmatch (f # x) $ \case
          PJust ux -> pcons # ux #$ self # f # xs
          _ -> self # f # xs
      )
      pnil
      l

-- | Get the first element that matches a predicate or return Nothing.
pfind' ::
  forall (a :: S -> Type) (s :: S) list.
  PIsListLike list a =>
  (Term s a -> Term s PBool) ->
  Term s (list a :--> PMaybe a)
pfind' p =
  precList
    (\self x xs -> pif (p x) (pcon (PJust x)) (self # xs))
    (const $ pcon PNothing)

-- | Get the first element that maps to a 'PJust' in a list.
pfirstJust ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S) list.
  PIsListLike list a =>
  Term s ((a :--> PMaybe b) :--> list a :--> PMaybe b)
pfirstJust =
  phoistAcyclic $
    plam $ \p ->
      precList
        ( \self x xs ->
            -- In the future, this should use `pmatchSum`, I believe?
            pmatch (p # x) $ \case
              PNothing -> self # xs
              PJust v -> pcon (PJust v)
        )
        (const $ pcon PNothing)

-- | /O(n)/. Find the value for a given key in an associative list.
plookup ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S) list.
  (PEq a, PIsListLike list (PBuiltinPair a b)) =>
  Term s (a :--> list (PBuiltinPair a b) :--> PMaybe b)
plookup =
  phoistAcyclic $
    plam $ \k xs ->
      pmatch (pfind' (\p -> pfstBuiltin # p #== k) # xs) $ \case
        PNothing -> pcon PNothing
        PJust p -> pcon (PJust (psndBuiltin # p))

-- | /O(n)/. Find the value for a given key in an assoclist which uses 'PTuple's.
plookupTuple ::
  (PEq a, PIsListLike list (PAsData (PTuple a b)), PIsData a, PIsData b) =>
  Term s (a :--> list (PAsData (PTuple a b)) :--> PMaybe b)
plookupTuple =
  phoistAcyclic $
    plam $ \k xs ->
      pmatch (pfind' (\p -> (pfield @"_0" # pfromData p) #== k) # xs) $ \case
        PNothing -> pcon PNothing
        PJust p -> pcon (PJust (pfield @"_1" # pfromData p))

{- | Given an integer @n@ and a term, produce a list containing @n@
   copies of that term. Non-positive integers yield an empty list.
-}
preplicate ::
  PIsListLike f a =>
  Term s (PInteger :--> a :--> f a)
preplicate = phoistAcyclic $
  pfix #$ plam $ \self count x ->
    pif (count #<= 0) pnil (pcons # x # (self # (count - 1) # x))

-- | Special version of 'pisUniq'', the list elements should have 'PEq' instance.
pisUniq' ::
  forall (l :: (S -> Type) -> S -> Type) (a :: S -> Type) (s :: S).
  (PEq a, PIsListLike l a) =>
  Term s (l a :--> PBool)
pisUniq' = phoistAcyclic $ pisUniqBy' # plam (#==)

{- | Return true if all the elements in the given list are unique, given the equalator function.
   The list is assumed to be ordered.
-}
pisUniqBy' ::
  forall (l :: (S -> Type) -> S -> Type) (a :: S -> Type) (s :: S).
  (PIsListLike l a) =>
  Term s ((a :--> a :--> PBool) :--> l a :--> PBool)
pisUniqBy' = phoistAcyclic $
  plam $ \eq l ->
    pif (pnull # l) (pconstant True) $
      go # eq # (phead # l) # (ptail # l)
  where
    go :: Term _ ((a :--> a :--> PBool) :--> a :--> l a :--> PBool)
    go = phoistAcyclic $
      pfix #$ plam $ \self' eq x xs ->
        plet (self' # eq) $ \self ->
          pif (pnull # xs) (pconstant True) $
            plet (phead # xs) $ \x' ->
              pif (eq # x # x') (pconstant False) $
                self # x' #$ ptail # xs
