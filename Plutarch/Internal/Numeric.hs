{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Internal.Numeric (
  PNum (..),
  PIntegral (..),
) where

import Data.Kind (Type)
import Plutarch.Builtin.Integer (PInteger)
import Plutarch.Internal.Lift (pconstant)
import Plutarch.Internal.Numeric.Additive (PAbs, pabs, pnegate, (#+), (#-))
import Plutarch.Internal.Numeric.Multiplicative (PSignum (psignum), (#*))
import Plutarch.Internal.Other (pto)
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.PlutusType (PInner)
import Plutarch.Internal.Term (
  S,
  Term,
  phoistAcyclic,
  punsafeBuiltin,
  (#),
  (:-->),
 )
import Plutarch.Unsafe (punsafeDowncast)
import PlutusCore qualified as PLC

class PNum (a :: S -> Type) where
  pfromInteger :: Integer -> Term s a
  default pfromInteger :: PNum (PInner a) => Integer -> Term s a
  pfromInteger x = punsafeDowncast $ pfromInteger x

instance PNum PInteger where
  {-# INLINEABLE pfromInteger #-}
  pfromInteger = pconstant

-- orphan instance, but only visibly orphan when importing internal modules
instance (PAbs a, PSignum a, PNum a) => Num (Term s a) where
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
