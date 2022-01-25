{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.DataRepr.Internal.HList (
  -- * HRec and Label types
  HRec (HNil, HCons),
  Labeled (Labeled, unLabeled),

  -- * Field indexing functions
  hrecField,

  -- * Type families
  type IndexList,
  type IndexLabel,
  type SingleItem,
  type Take,
  type Drop,
  type Range,
  type Min,
  type Max,
  type FindMinMax',
  type FindMinMax,

  -- * Internal utils
  Elem (..),
  ElemOf (..),
) where

import Data.Kind (Type)
import Data.Type.Bool (type If)
import GHC.Records (HasField (..))
import GHC.TypeLits (
  ErrorMessage (Text),
  Nat,
  Symbol,
  TypeError,
  type (+),
  type (-),
  type (<=?),
 )

--------------------------------------------------------------------------------
---------- HList and HRec types

newtype Labeled sym a = Labeled {unLabeled :: a}

data HRec (as :: [Type]) where
  HNil :: HRec '[]
  HCons :: (Labeled name a) -> HRec as -> HRec ((Labeled name a) ': as)

-- | GADT proof-witness of HList membership, usable as an index
data Elem (a :: k) (as :: [k]) where
  Here :: Elem a (a ': as)
  There :: Elem a as -> Elem a (b ': as)

---------- Field indexing functions

-- | Index HRec using Elem
indexHRec :: HRec as -> (forall a. Elem a as -> a)
indexHRec (HCons x _) Here = x
indexHRec (HCons _ xs) (There i) = indexHRec xs i
indexHRec HNil impossible = case impossible of {}

{- |
  Index a HList with a field in a provided list of fields.

  >>> xs = HRec @["x", "y", "z"] (HCons 1 (HCons 2 (HCons 3 HNil)))
  >>> hrecField @"y" @["x", "y", "z"] xs
  >>> 2
-}
hrecField ::
  forall name a as.
  ( (IndexLabel name as ~ a)
  , ElemOf name a as
  ) =>
  HRec as ->
  a
hrecField xs = unLabeled $ indexHRec xs $ elemOf @name @a @as

---------- Type families

-- | Indexing type-level lists
type family IndexList (n :: Nat) (l :: [k]) :: k where
  IndexList _ '[] = TypeError ( 'Text "IndexList: index out of bounds")
  IndexList 0 (x ': _) = x
  IndexList n (x : xs) = IndexList (n - 1) xs

-- | Indexing list of labeled types by label
type family IndexLabel (name :: Symbol) (as :: [Type]) :: Type where
  IndexLabel name ((Labeled name a) ': _) = a
  IndexLabel name (_ ': as) = IndexLabel name as

-- | Return the single item from a singleton list
type family SingleItem (as :: [k]) :: k where
  SingleItem '[a] = a

type family Take (n :: Nat) (as :: [k]) :: [k] where
  Take 0 xs = '[]
  Take n (x ': xs) = x ': (Take (n - 1) xs)

type family Drop (n :: Nat) (as :: [k]) :: [k] where
  Drop 0 xs = xs
  Drop n (x ': xs) = Drop (n - 1) xs

type family Range (from :: Nat) (to :: Nat) (as :: [k]) :: [k] where
  Range from to xs = Take (to - from + 1) (Drop from xs)

type family Min (a :: Nat) (b :: Nat) :: Nat where
  Min a b = If (a <=? b) a b

type family Max (a :: Nat) (b :: Nat) :: Nat where
  Max a b = If (a <=? b) b a

type family FindMinMax' (min :: Nat) (max :: Nat) (as :: [Nat]) :: (Nat, Nat) where
  FindMinMax' min max '[] = '(min, max)
  FindMinMax' min max (a ': as) = FindMinMax' (Min min a) (Max max a) as

type family FindMinMax (as :: [Nat]) :: (Nat, Nat) where
  FindMinMax (a ': as) = FindMinMax' a a (a ': as)

---------- Internal utils

{- |
  Construct an `Elem` via Nat.

  This class could instead be a more direct version of 'indexHList',
  but perhaps the `Elem` encoding will be useful.
-}
class
  (IndexLabel name as ~ a) =>
  ElemOf (name :: Symbol) (a :: Type) (as :: [Type])
    | as name -> a
  where
  -- | Construct the `Elem` corresponding to a Nat index.
  --
  --    Example:
  --
  --    >>> natElem @_ @0
  --    Here
  --
  --    >>> natElem @_ @3
  --    There (There (There Here))
  elemOf :: Elem (Labeled name a) as

instance {-# OVERLAPPING #-} ElemOf name a ((Labeled name a) ': as) where
  elemOf :: Elem (Labeled name a) ((Labeled name a) ': as)
  elemOf = Here

instance
  {-# OVERLAPPABLE #-}
  ( IndexLabel name (b ': as) ~ a
  , ElemOf name a as
  ) =>
  ElemOf name a (b ': as)
  where
  elemOf :: Elem (Labeled name a) (b ': as)
  elemOf = There (elemOf @name @a @as)

---------- HasField instances

instance
  forall name a as.
  ( (IndexLabel name as) ~ a
  , ElemOf name a as
  ) =>
  HasField name (HRec as) a
  where
  getField = hrecField @name
