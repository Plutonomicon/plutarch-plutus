{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Num (PNum (..)) where

import Plutarch.Internal (
  PType,
  Term,
  punsafeCoerce,
  (#),
  (:-->),
 )
import Plutarch.Internal.Other (pto)
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

-- prohibit mixing arithmetic operators such as
-- (2 #+ 3 #* 4) without explicit precedence.
infix 6 #+
infix 6 #-
infix 6 #*

-- orphan instance, but only visibly orphan when importing internal modules
instance PNum a => Num (Term s a) where
  (+) = (#+)
  (-) = (#-)
  (*) = (#*)
  abs x = pabs # x
  negate x = pnegate # x
  signum x = psignum # x
  fromInteger = pfromInteger
