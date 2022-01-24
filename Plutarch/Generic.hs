{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Common generics-sop utilities for use in Plutarch.
module Plutarch.Generic (
  MkSum (..),
) where

import Data.Kind (Type)
import GHC.TypeLits (Nat, type (-))
import Generics.SOP (I, NP, NS (S, Z))
import Plutarch.DataRepr.Internal.HList (IndexList)

{- |
Infrastructure to create a single sum constructor given its type index and value.

- `mkSum @0 @(Code a) x` creates the first sum constructor;
- `mkSum @1 @(Code a) x` creates the second sum constructor;
- etc.

It is type-checked that the `x` here matches the type of nth constructor of `a`.
-}
class MkSum (idx :: Nat) (xss :: [[Type]]) where
  mkSum :: NP I (IndexList idx xss) -> NS (NP I) xss

instance {-# OVERLAPPING #-} MkSum 0 (xs ': xss) where
  mkSum = Z

instance
  {-# OVERLAPPABLE #-}
  ( MkSum (idx - 1) xss
  , IndexList idx (xs ': xss) ~ IndexList (idx - 1) xss
  ) =>
  MkSum idx (xs ': xss)
  where
  mkSum x = S $ mkSum @(idx - 1) @xss x
