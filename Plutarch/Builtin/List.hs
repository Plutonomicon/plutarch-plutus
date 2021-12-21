{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Builtin.List (
  -- * List type
  PList (..),

  -- * List construction
  cons,
  nil,
  mkList,
  singleton,
  append,

  -- * List access
  head,
  contains,
  atIndex,
) where

import Data.Proxy
import Plutarch
import Plutarch.Bool
import Plutarch.Builtin
import Plutarch.Builtin.List.Type
import Plutarch.Integer (PInteger)
import Plutarch.Prelude
import qualified PlutusCore as PLC
import Prelude hiding (head)

instance ListElemUni (a :: k -> Type) => PlutusType (PList a) where
  type PInner (PList a) _ = PList a
  pcon' PNil =
    punsafeConstant $
      PLC.Some $
        PLC.ValueOf (PLC.DefaultUniList $ listElemUni (Proxy :: Proxy a)) []
  pcon' (PCons x xs) = MkCons #£ x £ xs
  pmatch' = pmatchList

pmatchList :: forall k (s :: k) (a :: k -> Type) (b :: k -> Type). Term s (PList a) -> (PList a s -> Term s b) -> Term s b
pmatchList list f =
  plet (NullList #£ list) $ \isEmpty ->
    pif
      (punsafeCoerce isEmpty)
      (f PNil)
      $ plet
        (HeadList #£ list)
        ( \head ->
            plet (TailList #£ list) $ \tail ->
              f $ PCons head tail
        )

cons :: forall k (s :: k) (a :: k -> Type). ListElemUni a => Term s a -> Term s (PList a) -> Term s (PList a)
cons x xs = pcon' $ PCons x xs

nil :: forall k (s :: k) (a :: k -> Type). ListElemUni a => Term s (PList a)
nil = pcon' PNil

-- | Build a polymorphic list from a list of PLC terms.
mkList :: forall k (s :: k) (a :: k -> Type). ListElemUni a => [Term s a] -> Term s (PList a)
mkList = \case
  [] -> nil
  (x : xs) -> cons x (mkList xs)

-- | Return the head of a list; fails on empty list.
head :: forall k (s :: k) (c :: k -> Type). Term s (PList c) -> Term s c
head list =
  pmatchList list $ \case
    PNil -> perror
    PCons x _ -> x

-- | Create a singleton list
singleton :: ListElemUni a => Term s (a :--> PList a)
singleton =
  plam $ \x ->
    pcon' (PCons x $ pcon' PNil)

-- | Return True if the list contains the given element.
contains :: (PEq a, ListElemUni a) => ClosedTerm (PList a :--> a :--> PBool)
contains =
  pfix £$ plam $ \self list k ->
    pmatch' list $ \case
      PNil ->
        pcon PFalse
      PCons x xs ->
        pif
          (k £== x)
          (pcon PTrue)
          (self £ xs £ k)

-- | Return the element at given index; fail otherwise.
atIndex :: ListElemUni a => ClosedTerm (PInteger :--> PList a :--> a)
atIndex =
  pfix £$ plam $ \self n' list ->
    pmatch' list $ \case
      PNil ->
        perror
      PCons x xs ->
        pif
          (n' £== 0)
          x
          (self £ (n' - 1) £ xs)

-- | Append two lists
append :: ListElemUni a => ClosedTerm (PList a :--> PList a :--> PList a)
append =
  pfix £$ plam $ \self list1 list2 ->
    pmatch' list1 $ \case
      PNil ->
        list2
      PCons x xs ->
        pcon' (PCons x $ self £ xs £ list2)
