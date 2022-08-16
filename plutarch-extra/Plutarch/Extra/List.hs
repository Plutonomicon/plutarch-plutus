module Plutarch.Extra.List (
  preverse,
  pcheckSorted,
  pnotNull,
  pmergeBy,
  pmsortBy,
  pmsort,
  pnubSortBy,
  pnubSort,
  pisUniqBy,
  pisUniq,
  pmapMaybe,
  pfind',
  pfirstJust,
  plookup,
  plookupTuple,
  pisSortedBy,
  pisSorted,
  preplicate,
  pisUniqBy',
  pisUniq',
) where

import Plutarch.Api.V1.Tuple (PTuple)
import Plutarch.Extra.TermCont (pletC)
import Plutarch.Prelude

-- | / O(n) /. reverses a list
preverse :: (PIsListLike l a) => Term s (l a :--> l a)
preverse =
  phoistAcyclic $
    pfoldl # plam (\ys y -> pcons # y # ys) # pnil

-- | / O(n) /.checks whether a list is sorted
pcheckSorted :: (PIsListLike l a, POrd a) => Term s (l a :--> PBool)
pcheckSorted =
  pfix #$ plam $ \self xs ->
    pelimList
      ( \x1 xs ->
          pelimList
            (\x2 _ -> x1 #<= x2 #&& (self # xs))
            (pcon PTrue)
            xs
      )
      (pcon PTrue)
      xs

{- | True if a list is not empty.

   @since 1.1.0
-}
pnotNull ::
  forall (a :: S -> Type) (s :: S) list.
  PIsListLike list a =>
  Term
    s
    (list a :--> PBool)
pnotNull =
  phoistAcyclic $
    plam $
      pelimList (\_ _ -> pcon PTrue) (pcon PFalse)

{- | / O(n) /. Merge two lists which are assumed to be ordered, given a custom comparator.
    The comparator should return true if first value is less than the second one.

   @since 1.1.0
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
pmergeBy = phoistAcyclic $ pfix #$ plam go
  where
    go self comp a b =
      pif (pnull # a) b $
        pif (pnull # b) a $
          unTermCont $ do
            ah <- pletC $ phead # a
            at <- pletC $ ptail # a
            bh <- pletC $ phead # b
            bt <- pletC $ ptail # b

            pure $
              pif
                (comp # ah # bh)
                (pcons # ah #$ self # comp # at # b)
                (pcons # bh #$ self # comp # a # bt)

{- | / O(nlogn) /. Merge sort, bottom-up version, given a custom comparator.

   Assuming the comparator returns true if first value is less than the second one,
    the list elements will be arranged in ascending order, keeping duplicates in the order
    they appeared in the input.

   @since 1.1.0
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
      pfix #$ plam $ \self comp xs ->
        pif (pnull # xs) pnil $
          let y = phead # xs
              ys = ptail # xs
           in pif (pnull # ys) y $
                self # comp #$ mergePairs # comp # xs
    mergePairs :: Term _ ((a :--> a :--> PBool) :--> l (l a) :--> l (l a))
    mergePairs = phoistAcyclic $
      pfix #$ plam $ \self comp xs ->
        pif (pnull # xs) pnil $
          let y = phead # xs
           in plet (ptail # xs) $ \ys ->
                pif (pnull # ys) xs $
                  let z = phead # ys
                      zs = ptail # ys
                   in pcons # (pmergeBy # comp # y # z) # (self # comp # zs)

{- | A special case of 'pmsortBy' which requires elements have 'POrd' instance.

   @since 1.1.0
-}
pmsort ::
  (POrd a, PIsListLike l a, PIsListLike l (l a)) =>
  Term s (l a :--> l a)
pmsort = phoistAcyclic $ pmsortBy # comp
  where
    comp = phoistAcyclic $ plam (#<)

{- | / O(nlogn) /. Sort and remove dupicate elements in a list.

    The first parameter is a equalator, which should return true if the two given values are equal.
    The second parameter is a comparator, which should returns true if the first value is less than the second value.

    @since 1.1.0
-}
pnubSortBy ::
  forall (a :: S -> Type) (s :: S) list.
  (PIsListLike list a, PIsListLike list (list a)) =>
  Term
    s
    ( (a :--> a :--> PBool)
        :--> (a :--> a :--> PBool)
        :--> list a
        :--> list a
    )
pnubSortBy = phoistAcyclic $
  plam $ \eq comp l -> pif (pnull # l) l $
    unTermCont $ do
      sl <- pletC $ pmsortBy # comp # l

      let x = phead # sl
          xs = ptail # sl

      return $ go # eq # x # xs
  where
    go = phoistAcyclic pfix #$ plam go'
    go' self eq seen l =
      pif (pnull # l) (psingleton # seen) $
        unTermCont $ do
          x <- pletC $ phead # l
          xs <- pletC $ ptail # l

          return $
            pif
              (eq # x # seen)
              (self # eq # seen # xs)
              (pcons # seen #$ self # eq # x # xs)

{- | Special version of 'pnubSortBy', which requires elements have 'POrd' instance.

   @since 1.1.0
-}
pnubSort ::
  forall (a :: S -> Type) (s :: S) list.
  (PIsListLike list a, PIsListLike list (list a), POrd a) =>
  Term s (list a :--> list a)
pnubSort = phoistAcyclic $ pnubSortBy # eq # comp
  where
    eq = phoistAcyclic $ plam (#==)
    comp = phoistAcyclic $ plam (#<)

{- | / O(nlogn) /. Check if a list contains no duplicates.

   @since 1.0.0
-}
pisUniqBy ::
  forall (a :: S -> Type) (s :: S) list.
  (PIsListLike list a, PIsListLike list (list a)) =>
  Term
    s
    ( (a :--> a :--> PBool)
        :--> (a :--> a :--> PBool)
        :--> list a
        :--> PBool
    )
pisUniqBy = phoistAcyclic $
  plam $ \eq comp xs ->
    let nubbed = pnubSortBy # eq # comp # xs
     in plength # xs #== plength # nubbed

{- | A special case of 'pisUniqBy' which requires elements have 'POrd' instance.

   @since 1.1.0
-}
pisUniq ::
  forall (a :: S -> Type) (s :: S) list.
  (POrd a, PIsListLike list a, PIsListLike list (list a)) =>
  Term s (list a :--> PBool)
pisUniq = phoistAcyclic $ pisUniqBy # eq # comp
  where
    eq = phoistAcyclic $ plam (#==)
    comp = phoistAcyclic $ plam (#<)

{- | A special version of `pmap` which allows list elements to be thrown out.

    @since 1.1.0
-}
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
  pfix #$ plam $ \self f l -> pif (pnull # l) pnil $
    unTermCont $ do
      x <- pletC $ phead # l
      xs <- pletC $ ptail # l

      pure $
        pmatch (f # x) $ \case
          PJust ux -> pcons # ux #$ self # f # xs
          _ -> self # f # xs

{- | Get the first element that matches a predicate or return Nothing.

     @since 1.1.0
-}
pfind' ::
  forall (a :: S -> Type) (s :: S) list.
  PIsListLike list a =>
  (Term s a -> Term s PBool) ->
  Term s (list a :--> PMaybe a)
pfind' p =
  precList
    (\self x xs -> pif (p x) (pcon (PJust x)) (self # xs))
    (const $ pcon PNothing)

{- | Get the first element that maps to a 'PJust' in a list.

     @since 1.1.0
-}
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

{- | /O(n)/. Find the value for a given key in an associative list.

     @since 1.1.0
-}
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

{- | /O(n)/. Find the value for a given key in an assoclist which uses 'PTuple's.

     @since 1.1.0
-}
plookupTuple ::
  (PEq a, PIsListLike list (PAsData (PTuple a b)), PIsData a, PIsData b) =>
  Term s (a :--> list (PAsData (PTuple a b)) :--> PMaybe b)
plookupTuple =
  phoistAcyclic $
    plam $ \k xs ->
      pmatch (pfind' (\p -> (pfield @"_0" # pfromData p) #== k) # xs) $ \case
        PNothing -> pcon PNothing
        PJust p -> pcon (PJust (pfield @"_1" # pfromData p))

{- | O(n). Returns true if the given list is sorted.

     @since 1.1.0
-}
pisSortedBy ::
  (PIsListLike list a) =>
  Term s ((a :--> a :--> PBool) :--> (a :--> a :--> PBool) :--> list a :--> PBool)
pisSortedBy = phoistAcyclic $
  plam $ \eq lt l -> pif (pnull # l) (pconstant True) $
    unTermCont $ do
      h <- pletC $ phead # l
      t <- pletC $ ptail # l
      pure $ go # eq # lt # h # t
  where
    go = pfix #$ plam $ \self' eq lt x xs ->
      plet (self' # eq # lt) $ \self ->
        pif
          (pnull # xs)
          (pconstant True)
          $ plet (phead # xs) $ \x' ->
            pif
              (eq # x # x' #|| lt # x # x')
              (self # x' #$ ptail # xs)
              (pconstant False)

{- | Special version of 'pisSortedBy'.

     @since 1.1.0
-}
pisSorted ::
  (PIsListLike list a, POrd a) =>
  Term s (list a :--> PBool)
pisSorted = phoistAcyclic $ pisSortedBy # eq # lt
  where
    eq = phoistAcyclic $ plam (#==)
    lt = phoistAcyclic $ plam (#<)

{- | Given an integer @n@ and a term, produce a list containing
@n@ copies of that term. Non-positive integers yield an empty
list.

  @since 1.2.0
-}
preplicate ::
  PIsListLike f a =>
  Term s (PInteger :--> a :--> f a)
preplicate = phoistAcyclic $
  pfix #$ plam $ \self count x ->
    pif (count #<= 0) pnil (pcons # x # (self # (count - 1) # x))

{- | Special version of 'pisUniq'', the list elements should have 'PEq' instance.

 @since 1.3.0
-}
pisUniq' ::
  forall (l :: (S -> Type) -> S -> Type) (a :: S -> Type) (s :: S).
  (PEq a, PIsListLike l a) =>
  Term s (l a :--> PBool)
pisUniq' = phoistAcyclic $ pisUniqBy' # phoistAcyclic (plam (#==))

{- | Return true if all the elements in the given list are unique, given the equalator function.
   The list is assumed to be ordered.

 @since 1.3.0
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
