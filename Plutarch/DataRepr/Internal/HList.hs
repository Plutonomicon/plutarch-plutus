{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.DataRepr.Internal.HList (
  -- * HList and HRec types
  HList (..),
  HRec (..),

  -- * Field indexing functions
  hlistField,
  hrecField,

  -- * Type families
  type IndexList,
  type IndexOf,
  type SingleItem,
  type Take,
  type Drop,
  type Range,

  -- * Internal utils
  Elem (..),
  indexHList,
  NatElem (..),
) where

import Data.Kind (Type)
import GHC.Records (HasField (..))
import GHC.TypeLits (
  Nat,
  Symbol,
  type (+),
  type (-),
 )

--------------------------------------------------------------------------------
---------- HList and HRec types

-- | Usual GADT Heterogenous List encoding
data HList (xs :: [Type]) where
  HNil :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

{- |
  Heterogenous Record, using a list of symbols as a
  set of corresponding indices by position.
-}
newtype HRec (fs :: [Symbol]) (as :: [Type])
  = HRec (HList as)

-- | GADT proof-witness of HList membership, usable as an index
data Elem (x :: k) (xs :: [k]) where
  Here :: Elem x (x ': xs)
  There :: Elem x xs -> Elem x (y ': xs)

---------- Field indexing functions

{- |
  Index a HList with a field in a provided list of fields.

  >>> xs = HCons 1 (HCons 2 (HCons 3 HNil))
  >>> hlistField @"y" @["x", "y", "z"] xs
  >>> 2
-}
hlistField ::
  forall f fs x xs n.
  ( (IndexOf f fs ~ n)
  , (IndexList n xs) ~ x
  , NatElem n x xs
  ) =>
  HList xs ->
  x
hlistField xs = indexHList xs $ fieldElem @f @fs

{- |
  Index a HList with a field in a provided list of fields.

  >>> xs = HRec @["x", "y", "z"] (HCons 1 (HCons 2 (HCons 3 HNil)))
  >>> hrecField @"y" @["x", "y", "z"] xs
  >>> 2
-}
hrecField ::
  forall f fs x xs n.
  ( (IndexOf f fs ~ n)
  , (IndexList n xs) ~ x
  , NatElem n x xs
  ) =>
  HRec fs xs ->
  x
hrecField (HRec xs) = indexHList xs $ fieldElem @f @fs

---------- Type families

-- | Indexing type-level lists
type family IndexList (n :: Nat) (l :: [k]) :: k where
  IndexList 0 (x ': _) = x
  IndexList n (x : xs) = IndexList (n - 1) xs

-- | Get the Index of a type in a list
type family IndexOf (x :: k) (xs :: [k]) :: Nat where
  IndexOf x (x ': _) = 0
  IndexOf x (y ': xs) = (IndexOf x xs + 1)

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

---------- Internal utils

-- | Index HList using Elem
indexHList :: HList xs -> (forall x. Elem x xs -> x)
indexHList (HCons x _) Here = x
indexHList (HCons _ xs) (There i) = indexHList xs i
indexHList HNil impossible = case impossible of {}

{- |
  Construct an Elem via the position of a given type
  in a type-level list.

  The intended use is to 'lookup' the index of a
  field name:

  >>> fieldElem @"z" @["x", "y", "z", "w"]
  >>> There (There Here))
-}
fieldElem ::
  forall f fs x xs n.
  ( (IndexOf f fs ~ n)
  , (IndexList n xs) ~ x
  , NatElem n x xs
  ) =>
  Elem x xs
fieldElem = natElem @_ @n

{- |
  Construct an `Elem` via Nat.

  This class could instead be a more direct version of 'indexHList',
  but perhaps the `Elem` encoding will be useful.
-}
class
  (IndexList n xs ~ x) =>
  NatElem (n :: Nat) (x :: k) (xs :: [k])
    | xs n -> x
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
  natElem :: Elem x xs

instance {-# OVERLAPS #-} NatElem 0 x (x ': xs) where
  natElem :: Elem x (x ': xs)
  natElem = Here

instance
  {-# OVERLAPPABLE #-}
  ( IndexList n (y ': xs) ~ x
  , NatElem (n - 1) x xs
  ) =>
  NatElem n x (y ': xs)
  where
  natElem :: Elem x (y ': xs)
  natElem = There (natElem @_ @(n - 1) @x @xs)

---------- HasField instances

instance
  forall f fs a as n.
  ( (IndexOf f fs ~ n)
  , (IndexList n as) ~ a
  , NatElem n a as
  ) =>
  HasField f (HRec fs as) a
  where
  getField = hrecField @f
