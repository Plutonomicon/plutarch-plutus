{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Num (PNum (..), PFractional (..)) where

import Plutarch.Internal (
  PType,
  Term,
  pthrow,
  punsafeCoerce,
  (:-->),
 )
import Plutarch.Internal.Other (pto)
import Plutarch.Internal.PLam ((#))
import Plutarch.Internal.PlutusType (PInner)
import Plutarch.Unsafe (punsafeDowncast)

class PNum (a :: PType) where
  (#+) :: Term s a -> Term s a -> Term s a
  default (#+) :: PNum (PInner a) => Term s a -> Term s a -> Term s a
  x #+ y = punsafeDowncast $ pto x #+ pto y

  (#-) :: Term s a -> Term s a -> Term s a
  default (#-) :: PNum (PInner a) => Term s a -> Term s a -> Term s a
  x #- y = punsafeDowncast $ pto x #- pto y

  (#*) :: Term s a -> Term s a -> Term s a
  default (#*) :: PNum (PInner a) => Term s a -> Term s a -> Term s a
  x #* y = punsafeDowncast $ pto x #* pto y

  pnegate :: Term s (a :--> a)
  default pnegate :: PNum (PInner a) => Term s (a :--> a)
  pnegate = punsafeCoerce (pnegate :: Term s (PInner a :--> PInner a))

  pabs :: Term s (a :--> a)
  default pabs :: PNum (PInner a) => Term s (a :--> a)
  pabs = punsafeCoerce (pabs :: Term s (PInner a :--> PInner a))

  psignum :: Term s (a :--> a)
  default psignum :: PNum (PInner a) => Term s (a :--> a)
  psignum = punsafeCoerce (psignum :: Term s (PInner a :--> PInner a))

  pfromInteger :: Integer -> Term s a
  default pfromInteger :: PNum (PInner a) => Integer -> Term s a
  pfromInteger x = punsafeDowncast $ pfromInteger x

-- orphan instance, but only visibly orphan when importing internal modules
instance PNum a => Num (Term s a) where
  (+) = (#+)
  (-) = (#-)
  (*) = (#*)
  abs x = pabs # x
  negate x = pnegate # x
  signum x = psignum # x
  fromInteger = pfromInteger

class PFractional (a :: PType) where
  (#/) :: Term s a -> Term s a -> Term s a
  precip :: Term s (a :--> a)

instance (PNum a, PFractional a) => Fractional (Term s a) where
  (/) = (#/)
  recip x = precip # x
  fromRational _ = pthrow "unsupported operation"