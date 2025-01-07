module Plutarch.Internal.Semigroup (
  -- * Type classes
  PSemigroup (..),
  PMonoid (..),
) where

import Data.Kind (Type)
import Plutarch.Internal.Numeric (
  PNatural,
  PPositive,
  pbySquaringDefault,
  pnaturalToPositiveCPS,
 )
import Plutarch.Internal.Other (pto)
import Plutarch.Internal.PlutusType (PInner)
import Plutarch.Internal.Term (S, Term)
import Plutarch.Unsafe (punsafeDowncast)

class PSemigroup (a :: S -> Type) where
  (#<>) :: forall (s :: S). Term s a -> Term s a -> Term s a
  default (#<>) ::
    forall (s :: S).
    PSemigroup (PInner a) =>
    Term s a ->
    Term s a ->
    Term s a
  x #<> y = punsafeDowncast $ pto x #<> pto y
  {-# INLINEABLE pstimes #-}
  pstimes :: forall (s :: S). Term s PPositive -> Term s a -> Term s a
  pstimes p x = pbySquaringDefault (#<>) x p

class PSemigroup a => PMonoid (a :: S -> Type) where
  pmempty :: forall (s :: S). Term s a
  default pmempty ::
    forall (s :: S).
    PMonoid (PInner a) =>
    Term s a
  pmempty = punsafeDowncast pmempty
  {-# INLINEABLE pmtimes #-}
  pmtimes :: forall (s :: S). Term s PNatural -> Term s a -> Term s a
  pmtimes n x = pnaturalToPositiveCPS pmempty (`pstimes` x) n
