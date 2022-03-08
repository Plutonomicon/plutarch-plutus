{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{- Common generics-sop utilities for use in Plutarch.
-}
module Plutarch.Internal.Generic (
  -- * Plutarch adapters for generics-sop API
  PGeneric,
  PCode,
  pfrom,
  pto,

  -- * Helpers for when existing generics-sop combinators are insufficient.
  MkSum (mkSum),
) where

import Data.Kind (Constraint, Type)
import GHC.TypeLits (Nat, type (-))
import Generics.SOP (All, AllZip, Code, Generic (from, to), I, LiftedCoercible, NP, NS (S, Z), SOP, SameShapeAs, Top, hfromI, htoI)
import Generics.SOP.Constraint (AllZipF)
import Plutarch.DataRepr.Internal.HList.Utils (IndexList)
import Plutarch.Internal (PType, S, Term)
import Plutarch.Internal.TypeFamily (ToPType2)

-- | `Generic` constraint extended to work with Plutarch types.
type PGeneric :: S -> PType -> Constraint
type PGeneric s a =
  ( Generic (a s)
  , SameShapeAs (Code (a s)) (PCode s a)
  , SameShapeAs (PCode s a) (Code (a s))
  , AllZipF (AllZip (LiftedCoercible I (Term s))) (Code (a s)) (PCode s a)
  , AllZipF (AllZip (LiftedCoercible (Term s) I)) (PCode s a) (Code (a s))
  , All Top (PCode s a)
  )

-- | Like `Code` but for Plutarch types
type PCode s a = ToPType2 (Code (a s))

{- | Like `from` but for Plutarch terms

  Instead of `I`, this uses `Term s` as the container type.
-}
pfrom :: PGeneric s a => a s -> SOP (Term s) (PCode s a)
pfrom = hfromI . from

-- | Like `to` but for Plutarch terms. Analogous to `pfrom`.
pto :: PGeneric s a => SOP (Term s) (PCode s a) -> a s
pto = to . htoI

{- |
Infrastructure to create a single sum constructor given its type index and value.

- `mkSum @0 @(Code a) x` creates the first sum constructor;
- `mkSum @1 @(Code a) x` creates the second sum constructor;
- etc.

It is type-checked that the `x` here matches the type of nth constructor of `a`.
-}
class MkSum (idx :: Nat) (xss :: [[k]]) (f :: k -> Type) where
  mkSum :: NP f (IndexList idx xss) -> NS (NP f) xss

instance {-# OVERLAPPING #-} MkSum 0 (xs ': xss) f where
  mkSum = Z

instance
  {-# OVERLAPPABLE #-}
  ( MkSum (idx - 1) xss f
  , IndexList idx (xs ': xss) ~ IndexList (idx - 1) xss
  ) =>
  MkSum idx (xs ': xss) f
  where
  mkSum x = S $ mkSum @_ @(idx - 1) @xss x
