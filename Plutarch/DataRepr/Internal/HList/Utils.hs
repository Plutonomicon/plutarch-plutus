{-# LANGUAGE UndecidableInstances #-}

module Plutarch.DataRepr.Internal.HList.Utils (
  Labeled (..),
  Elem (..),
  IndexList,
  IndexLabel,
  SingleItem,
  Drop,
) where

import Data.Kind (Type)
import GHC.TypeLits (
  ErrorMessage (Text),
  Nat,
  Symbol,
  TypeError,
  type (-),
 )

newtype Labeled sym a = Labeled {unLabeled :: a}

-- | GADT proof-witness of HList membership, usable as an index
data Elem (a :: k) (as :: [k]) where
  Here :: Elem a (a ': as)
  There :: Elem a as -> Elem a (b ': as)

---------- Type families

-- | Indexing type-level lists
type family IndexList (n :: Nat) (l :: [k]) :: k where
  IndexList _ '[] = TypeError ('Text "IndexList: index out of bounds")
  IndexList 0 (x ': _) = x
  IndexList n (_ ': xs) = IndexList (n - 1) xs

-- | Indexing list of labeled pairs by label
type IndexLabel :: Symbol -> [(Symbol, Type)] -> Type
type family IndexLabel name as where
  IndexLabel name ('(name, a) ': _) = a
  IndexLabel name (_ ': as) = IndexLabel name as

-- | Return the single item from a singleton list
type family SingleItem (as :: [k]) :: k where
  SingleItem '[a] = a

-- | Drop first n fields of a list
type family Drop (n :: Nat) (as :: [k]) :: [k] where
  Drop 0 xs = xs
  Drop n (_ ': xs) = Drop (n - 1) xs
