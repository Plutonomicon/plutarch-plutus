{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{- Common generics-sop utilities for use in Plutarch.
-}
module Plutarch.Internal.Generic (
  -- * Plutarch adapters for generics-sop API
  PGeneric,
  PCode,
  gpfrom,
  gpto,

  -- * Helpers for when existing generics-sop combinators are insufficient.
  MkNS,
  mkNS,
  mkSum,
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
gpfrom :: PGeneric s a => a s -> SOP (Term s) (PCode s a)
gpfrom = hfromI . from

-- | Like `to` but for Plutarch terms. Analogous to `gpfrom`.
gpto :: PGeneric s a => SOP (Term s) (PCode s a) -> a s
gpto = to . htoI

{- |
Infrastructure to create a single sum constructor given its type index and value.

- `mkSum @0 @(Code a) x` creates the first sum constructor;
- `mkSum @1 @(Code a) x` creates the second sum constructor;
- etc.

It is type-checked that the `x` here matches the type of nth constructor of `a`.
-}
class MkNS (idx :: Nat) (xs :: [k]) (f :: k -> Type) where
  mkNS :: f (IndexList idx xs) -> NS f xs

instance {-# OVERLAPPING #-} MkNS 0 (x ': xs) f where
  mkNS = Z

instance
  {-# OVERLAPPABLE #-}
  ( MkNS (idx - 1) xs f
  , IndexList idx (x ': xs) ~ IndexList (idx - 1) xs
  ) =>
  MkNS idx (x ': xs) f
  where
  mkNS x = S $ mkNS @_ @(idx - 1) @xs x

mkSum :: forall idx xss f. MkNS idx xss (NP f) => NP f (IndexList idx xss) -> NS (NP f) xss
mkSum = mkNS @_ @idx @xss @(NP f)
