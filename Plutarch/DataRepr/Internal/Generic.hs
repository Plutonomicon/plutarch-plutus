{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{- Common generics-sop utilities for use in Plutarch.

  TODO: Move to Plutarch.Internal.Generic?
-}
module Plutarch.DataRepr.Internal.Generic (
  -- * Plutarch adapters for generics-sop API
  PGeneric,
  PCode,
  pfrom,

  -- * Helpers for when existing generics-sop combinators are insufficient.
  MkSum (..),
) where

import Data.Kind (Constraint, Type)
import GHC.TypeLits (Nat, type (-))
import Generics.SOP (All, AllZip, Code, Generic (from), I, LiftedCoercible, NP, NS (S, Z), SOP, SameShapeAs, Top, hfromI)
import Generics.SOP.Constraint (AllZipF)
import Plutarch.DataRepr.Internal.HList.Utils (IndexList)
import Plutarch.Internal (PType, S, Term)
import Plutarch.Internal.TypeFamily (ToPType2)

{- `Generic` constraint extended to work with Plutarch types.
-}
type PGeneric :: S -> PType -> Constraint
type PGeneric s a =
  ( Generic (a s)
  , SameShapeAs (Code (a s)) (ToPType2 (Code (a s)))
  , SameShapeAs (ToPType2 (Code (a s))) (Code (a s))
  , AllZipF (AllZip (LiftedCoercible I (Term s))) (Code (a s)) (ToPType2 (Code (a s)))
  , All Top (ToPType2 (Code (a s)))
  )

{- Like `Code` but for Plutarch types -}
type PCode s a = ToPType2 (Code (a s))

{- | Like `from` but for Plutarch terms

  Instead of `I`, this uses `Term s` as the container type.
-}
pfrom :: PGeneric s a => a s -> SOP (Term s) (PCode s a)
pfrom = hfromI . from

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
