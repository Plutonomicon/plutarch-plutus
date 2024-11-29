{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Internal.Numeric (
  PNum (..),
  PIntegral (..),
) where

import Data.Kind (Type)
import Plutarch.Internal.Other (pto)
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.PlutusType (PInner)
import Plutarch.Internal.Term (
  S,
  Term,
  phoistAcyclic,
  punsafeCoerce,
  (#),
  (:-->),
 )
import Plutarch.Unsafe (punsafeDowncast)

class PNum (a :: S -> Type) where
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
  {-# INLINEABLE (+) #-}
  (+) = (#+)
  {-# INLINEABLE (-) #-}
  (-) = (#-)
  {-# INLINEABLE (*) #-}
  (*) = (#*)
  {-# INLINEABLE abs #-}
  abs x = pabs # x
  {-# INLINEABLE negate #-}
  negate x = pnegate # x
  {-# INLINEABLE signum #-}
  signum x = psignum # x
  {-# INLINEABLE fromInteger #-}
  fromInteger = pfromInteger

class PIntegral a where
  pdiv :: Term s (a :--> a :--> a)
  default pdiv :: PIntegral (PInner a) => Term s (a :--> a :--> a)
  pdiv = phoistAcyclic $ plam $ \x y -> punsafeDowncast $ pdiv # pto x # pto y
  pmod :: Term s (a :--> a :--> a)
  default pmod :: PIntegral (PInner a) => Term s (a :--> a :--> a)
  pmod = phoistAcyclic $ plam $ \x y -> punsafeDowncast $ pmod # pto x # pto y
  pquot :: Term s (a :--> a :--> a)
  default pquot :: PIntegral (PInner a) => Term s (a :--> a :--> a)
  pquot = phoistAcyclic $ plam $ \x y -> punsafeDowncast $ pquot # pto x # pto y
  prem :: Term s (a :--> a :--> a)
  default prem :: PIntegral (PInner a) => Term s (a :--> a :--> a)
  prem = phoistAcyclic $ plam $ \x y -> punsafeDowncast $ prem # pto x # pto y
