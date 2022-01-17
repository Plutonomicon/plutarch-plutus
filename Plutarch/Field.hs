{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Plutarch.Field (HList (..), indexHList, Elem (..), NatElem (..), getField) where

import Data.Kind (Type)
--import Data.Proxy (Proxy (..))
import GHC.TypeLits (type (-), type (+), Nat)

--------------------------------------------------------------------------------

data HList (xs :: [Type]) where
  HNil :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

data Elem (x :: k) (xs :: [k]) where
  Here :: Elem x (x ': xs)
  There :: Elem x xs -> Elem x (y ': xs)

type family IndexList (n :: Nat) (l :: [k]) :: k where
  IndexList 0 (x ': _) = x
  IndexList n (x : xs) = IndexList (n - 1) xs

indexHList :: HList xs -> (forall x. Elem x xs -> x)
indexHList (HCons x _) Here = x
indexHList (HCons _ xs) (There i) = indexHList xs i
indexHList HNil impossible = case impossible of {}

getField 
  :: forall f fs x xs n.
  ( (IndexOf f fs ~ n)
  , (IndexList n xs) ~ x
  , NatElem n x xs
  ) =>
  HList xs -> x
getField xs = indexHList xs (natElem @_ @n)

type family IndexOf (x :: k) (xs :: [k]) :: Nat where
  IndexOf x (x ': _) = 0
  IndexOf x (y ': xs) = (IndexOf x xs + 1)

--type family Field (f :: l) (fs :: [l]) (xs :: [k]) :: k where
--  Field f fs xs = IndexList (IndexOf f fs) xs

class 
  (IndexList n xs ~ x) => 
  NatElem (n :: Nat) (x :: k) (xs :: [k]) | xs n -> x where
  natElem :: Elem x xs

instance {-# OVERLAPS #-} NatElem 0 x (x ': xs) where
  natElem :: Elem x (x ': xs)
  natElem = Here

instance {-# OVERLAPPABLE #-}
  ( IndexList n (y ': xs) ~ x
  , NatElem (n - 1) x xs 
  ) =>
  NatElem n x (y ': xs) where
  natElem :: Elem x (y ': xs)
  natElem = There (natElem @_ @(n-1) @x @xs)