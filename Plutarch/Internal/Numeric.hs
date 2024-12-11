{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Internal.Numeric (
  PIntegral (..),
) where

import Plutarch.Builtin.Integer (PInteger)
import Plutarch.Internal.Numeric.Additive (pnegate, (#+), (#-))
import Plutarch.Internal.Numeric.Multiplicative ((#*))
import Plutarch.Internal.Numeric.Ring (PIntegralDomain (pabs, psignum), pfromInteger)
import Plutarch.Internal.Other (pto)
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.PlutusType (PInner)
import Plutarch.Internal.Term (
  Term,
  phoistAcyclic,
  punsafeBuiltin,
  (#),
  (:-->),
 )
import Plutarch.Unsafe (punsafeDowncast)
import PlutusCore qualified as PLC

-- orphan instance, but only visibly orphan when importing internal modules
instance PIntegralDomain a => Num (Term s a) where
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

instance PIntegral PInteger where
  {-# INLINEABLE pdiv #-}
  pdiv = punsafeBuiltin PLC.DivideInteger
  {-# INLINEABLE pmod #-}
  pmod = punsafeBuiltin PLC.ModInteger
  {-# INLINEABLE pquot #-}
  pquot = punsafeBuiltin PLC.QuotientInteger
  {-# INLINEABLE prem #-}
  prem = punsafeBuiltin PLC.RemainderInteger
