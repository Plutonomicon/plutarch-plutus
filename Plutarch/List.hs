{-# LANGUAGE UndecidableInstances #-}

-- | Scott-encoded lists and ListLike typeclass
module Plutarch.List (
  PList (PSCons, PSNil),
  ptryUncons,
  puncons,
  pzip,
  pfind,
  preverse,
  pcheckSorted,
  pelem,
  (#!!),
  pelemAt,
  pelemAt',
  plistEquals,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Bool (PBool (PFalse, PTrue), pif, ptrue, (#&&))
import Plutarch.Builtin.Integer (PInteger)
import Plutarch.Internal.Eq (PEq ((#==)))
import Plutarch.Internal.Fix (pfix)
import Plutarch.Internal.Lift (pconstant)
import Plutarch.Internal.ListLike (
  PElemConstraint,
  PIsListLike,
  PListLike,
  pcons,
  pelimList,
  pfoldl,
  phead,
  pnil,
  precList,
  ptail,
  pzipWith',
 )
import Plutarch.Internal.Ord (POrd ((#<), (#<=)))
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.PlutusType (
  PlutusType,
  pcon,
  pmatch,
 )
import Plutarch.Internal.Show (PShow (pshow'), pshowList)
import Plutarch.Internal.Term (
  S,
  Term,
  perror,
  phoistAcyclic,
  (#),
  (#$),
  (:-->),
 )
import Plutarch.Internal.Trace (ptraceInfo)
import Plutarch.Maybe (PMaybe (PJust, PNothing))
import Plutarch.Pair (PPair (PPair))
import Plutarch.Repr.SOP (DeriveAsSOPStruct (DeriveAsSOPStruct))

{- | SOP-encoded list.

@since 1.10.0
-}
data PList (a :: S -> Type) (s :: S)
  = PSCons (Term s a) (Term s (PList a))
  | PSNil
  deriving stock
    ( -- | @since 1.10.0
      Generic
    )
  deriving anyclass
    ( -- | @since 1.10.0
      SOP.Generic
    )

-- | @since 1.10.0
deriving via
  DeriveAsSOPStruct (PList a)
  instance
    PlutusType (PList a)

instance PShow a => PShow (PList a) where
  pshow' _ x = pshowList @PList @a # x

instance PEq a => PEq (PList a) where
  (#==) xs ys = plistEquals # xs # ys

--------------------------------------------------------------------------------

instance PListLike PList where
  type PElemConstraint PList _ = ()
  pelimList match_cons match_nil ls = pmatch ls $ \case
    PSCons x xs -> match_cons x xs
    PSNil -> match_nil
  pcons = phoistAcyclic $ plam $ \x xs -> pcon (PSCons x xs)
  pnil = pcon PSNil

--------------------------------------------------------------------------------

-- | Extract head and tail of the list, throws error if list is empty.
ptryUncons ::
  PIsListLike list a =>
  Term s (list a :--> PPair a (list a))
ptryUncons =
  phoistAcyclic $
    plam $
      pelimList (\x -> pcon . PPair x) perror

-- | Extract head and tail of the list, if list is not empty.
puncons ::
  PIsListLike list a =>
  Term s (list a :--> PMaybe (PPair a (list a)))
puncons =
  phoistAcyclic $
    plam $
      pelimList (\x -> pcon . PJust . pcon . PPair x) (pcon PNothing)

--------------------------------------------------------------------------------

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

-- | / O(n) /. like haskell level `find` but on plutarch level
pfind :: PIsListLike l a => Term s ((a :--> PBool) :--> l a :--> PMaybe a)
pfind = phoistAcyclic $
  pfix #$ plam $ \self f xs ->
    pelimList
      ( \y ys ->
          pif
            (f # y)
            (pcon $ PJust y)
            (self # f # ys)
      )
      (pcon PNothing)
      xs

{- | / O(n) /. Reverse a list-like structure.

@since 1.10.0
-}
preverse ::
  forall (l :: (S -> Type) -> S -> Type) (a :: S -> Type) (s :: S).
  PIsListLike l a =>
  Term s (l a :--> l a)
preverse = phoistAcyclic $ pfoldl # plam (\xs x -> pcons # x # xs) # pnil

{- | / O(n) /. Checks if a list-list structure is sorted.

@since 1.10.0
-}
pcheckSorted ::
  forall (l :: (S -> Type) -> S -> Type) (a :: S -> Type) (s :: S).
  (PIsListLike l a, POrd a) =>
  Term s (l a :--> PBool)
pcheckSorted =
  pfix #$ plam $ \self xs ->
    pelimList
      ( \x1 xs ->
          pelimList
            (\x2 _ -> x1 #<= x2 #&& (self # xs))
            ptrue
            xs
      )
      ptrue
      xs

-- | / O(n) /. Check if element is in the list
pelem :: (PIsListLike list a, PEq a) => Term s (a :--> list a :--> PBool)
pelem =
  phoistAcyclic $
    plam $ \needle ->
      precList
        (\self x xs -> pif (x #== needle) (pcon PTrue) (self # xs))
        (\_self -> pcon PFalse)

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

-- | / O(n) /. Like Haskell level `(!!)` but on the plutarch level
(#!!) :: PIsListLike l a => Term s (l a) -> Term s PInteger -> Term s a
l #!! i = pelemAt # i # l

{- | / O(n) /. Like Haskell level `(!!)` but on the Plutarch level, not infix and
    with arguments reversed, errors if the specified index is greater than or equal
    to the lists length
-}
pelemAt :: PIsListLike l a => Term s (PInteger :--> l a :--> a)
pelemAt = phoistAcyclic $
  plam $ \n xs ->
    pif
      (n #< 0)
      (ptraceInfo "pelemAt: negative index" perror)
      (pelemAt' # n # xs)

-- | / O(n) /. like `pelemAt` but doesn't fail on negative indexes
pelemAt' :: PIsListLike l a => Term s (PInteger :--> l a :--> a)
pelemAt' = phoistAcyclic $
  pfix #$ plam $ \self n xs ->
    pif
      (n #== 0)
      (phead # xs)
      (self # (n - 1) #$ ptail # xs)
