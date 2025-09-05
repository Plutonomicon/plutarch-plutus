{-# LANGUAGE AllowAmbiguousTypes #-}
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
  pmatchListN,
  pmatchList,
  pmatchListUnsafe,
  pmatchList',
  Length,
  Replicate,
  UnsafeConstrNP (..),
) where

import Data.Bifunctor (bimap)
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat, natVal, type (+), type (-))
import Generics.SOP (NP (..))
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
  InternalConfig (InternalConfig, internalConfig'dataRecPMatchOptimization),
  S,
  Term,
  perror,
  pgetInternalConfig,
  phoistAcyclic,
  plet,
  pplaceholder,
  punsafeCoerce,
  pwithInternalConfig,
  (#),
  (#$),
  (:-->),
 )
import Plutarch.Internal.TermCont (pfindAllPlaceholders, unTermCont)
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

{- | Match first N elements from the list. It's is better to use @pmatchList@ if number of elements that needs to be
match does not need to be dynamically determined. It is important to understand each element given in Haskell
list is "computation" to get nth element. If those need to be referenced multiple times, it needs to be pletted to
prevent duplication of computation.

@since WIP
-}
pmatchListN ::
  forall b li a s.
  PIsListLike li a =>
  Integer ->
  Term s (li a) ->
  ([Term s a] -> Term s (li a) -> Term s b) ->
  Term s b
pmatchListN n xs f = pgetInternalConfig $ \cfg -> unTermCont $ do
  let
    -- placeholders are from 0 to n - 1, n is the "rest" part
    placeholders = pplaceholder <$> [0 .. (n - 1)]
    placeholderApplied = pwithInternalConfig (InternalConfig False) $ f placeholders (pplaceholder n)

  usedFields <-
    if internalConfig'dataRecPMatchOptimization cfg
      then pfindAllPlaceholders placeholderApplied
      else pure [0 .. n]

  pure $ pmatchList' n usedFields xs f

{- | Same functionality to @pmatchListN@ but each matched value will be given in @NP@ for better typing. Same performance
implications as @pmatchListN@.

@since WIP
-}
pmatchList ::
  forall n r li a s.
  ( PIsListLike li a
  , KnownNat (Length (Replicate n a))
  , UnsafeConstrNP (Replicate n a)
  ) =>
  Term s (li a) ->
  (NP (Term s) (Replicate n a) -> Term s (li a) -> Term s r) ->
  Term s r
pmatchList = pmatchListUnsafe @(Replicate n a)

{- | Same as @pmatchList@ but allows matching each element to arbitrary type. Essentially, this is @pmatchList@ combined
with @punsafeCoerce@; therefore, this is unsafe and will require careful attention when using this.

This function is especially helpful when matching on @PBuiltinList (PData)@ when user knows the type of each elements.
If first two elements are Data Integers, one can use @pmatchListUnsafe @'[PAsData PInteger, PAsData PInteger] li@ and
have everything already coerced when it's being matched.

@since WIP
-}
pmatchListUnsafe ::
  forall (struct :: [S -> Type]) r li a s.
  (PIsListLike li a, KnownNat (Length struct), UnsafeConstrNP struct) =>
  Term s (li a) ->
  (NP (Term s) struct -> Term s (li a) -> Term s r) ->
  Term s r
pmatchListUnsafe xs f =
  let
    go xs' rest =
      case constrNP @struct xs' of
        Nothing -> error "impossible"
        Just xs'' -> f xs'' rest
   in
    pmatchListN (natVal (Proxy @(Length struct))) xs go

-- TODO: make pmatchDataRec use this
-- NOTE: Maybe not, it could require some more coercing, less safer when messing with internal code.
pmatchList' ::
  forall b li a s.
  PIsListLike li a =>
  Integer ->
  [Integer] ->
  Term s (li a) ->
  ([Term s a] -> Term s (li a) -> Term s b) ->
  Term s b
pmatchList' n usedFields xs f = unTermCont $ do
  let
    mkMatches ::
      Integer ->
      Term s (li a) ->
      (Integer, Term s (li a)) ->
      ([(Term s a, Term s (li a))] -> Term s r) ->
      Term s r
    mkMatches idx running lastBind@(lastBindIdx, lastBindT) cps
      | idx > n = cps [] -- >= is not correct, we want to compute one more for the "rest" part.
      | idx `elem` usedFields =
          let
            isLastBind = all (<= idx) usedFields
            dropAmount = fromInteger $ idx - lastBindIdx
            dropToCurrent' :: forall s'. Term s' (li a) -> Term s' (li a)
            dropToCurrent' = foldr (.) id $ replicate dropAmount (ptail #)

            -- If this is last term, or amount of @ptail@ we need is less than 3, we don't hoist.
            currentTerm
              | isLastBind || dropAmount <= 3 = dropToCurrent' lastBindT
              | otherwise = phoistAcyclic (plam dropToCurrent') # lastBindT

            bindIfNeeded = if isLastBind then (\a h -> h a) else plet
           in
            bindIfNeeded currentTerm $ \newBind ->
              mkMatches (idx + 1) (ptail # running) (idx, newBind) $ \rest ->
                cps $ (phead # newBind, newBind) : rest
      | otherwise =
          mkMatches (idx + 1) (ptail # running) lastBind $
            \rest ->
              cps $ (phead # running, running) : rest

  pure $ mkMatches 0 xs (0, xs) (uncurry f . bimap (take $ fromInteger n) last . unzip)

type family Length xs where
  Length '[] = 0
  Length (_ ': xs) = 1 + Length xs

type family Replicate n a where
  Replicate 0 _ = '[]
  Replicate x a = a ': Replicate (x - 1) a

-- This is very dangerous, be extra careful when using it
-- Essentially, this will construct NP of specified structure from the given list of term
-- There is no type assurances whatsoever and each term will be coerced.
class UnsafeConstrNP xs where
  constrNP :: forall a s. [Term s a] -> Maybe (NP (Term s) xs)

instance UnsafeConstrNP xs => UnsafeConstrNP (x ': xs) where
  constrNP [] = Nothing
  constrNP (x : xs) = do
    rest <- constrNP @xs xs
    pure $ punsafeCoerce x :* rest

instance UnsafeConstrNP '[] where
  constrNP _ = Just Nil
