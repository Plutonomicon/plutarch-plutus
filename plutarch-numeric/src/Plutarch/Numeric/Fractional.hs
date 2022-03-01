module Plutarch.Numeric.Fractional (
  Fractionable (..),
  PFractionable (..),
) where

import Data.Kind (Type)
import Plutarch (S, Term, pfix, plam, (#), (#$), type (:-->))
import Plutarch.Bool (pif, (#==))
import Plutarch.Integer (PInteger, prem)
import Plutarch.Numeric.NZInteger (NZInteger (NZInteger))
import Plutarch.Numeric.NZNatural (NZNatural (NZNatural), PNZNatural)
import Plutarch.Numeric.Natural (Natural (Natural), PNatural)
import Plutarch.Unsafe (punsafeBuiltin, punsafeCoerce)
import PlutusCore qualified as PLC

-- | @since 1.0
class Fractionable (a :: Type) where
  -- | @since 1.0
  scale :: a -> NZNatural -> a

  -- | @since 1.0
  unscale :: a -> NZNatural -> a

  -- | @since 1.0
  findScale :: a -> NZNatural -> NZNatural

-- | @since 1.0
instance Fractionable Integer where
  {-# INLINEABLE scale #-}
  scale x (NZNatural y) = x * y
  {-# INLINEABLE unscale #-}
  unscale x (NZNatural y) = x `quot` y
  {-# INLINEABLE findScale #-}
  findScale x (NZNatural y) = NZNatural $ x `gcd` y

-- | @since 1.0
deriving via Integer instance Fractionable NZNatural

-- | @since 1.0
deriving via Integer instance Fractionable Natural

-- | @since 1.0
deriving via Integer instance Fractionable NZInteger

-- | @since 1.0
class PFractionable (a :: S -> Type) where
  -- | @since 1.0
  pscale ::
    forall (s :: S).
    Term s a ->
    Term s PNZNatural ->
    Term s a

  -- | @since 1.0
  punscale ::
    forall (s :: S).
    Term s a ->
    Term s PNZNatural ->
    Term s a

  -- | @since 1.0
  pfindScale ::
    forall (s :: S).
    Term s a ->
    Term s PNZNatural ->
    Term s PNZNatural

-- | @since 1.0
instance PFractionable PInteger where
  {-# INLINEABLE pscale #-}
  pscale t t' = punsafeBuiltin PLC.MultiplyInteger # t # t'
  {-# INLINEABLE punscale #-}
  punscale t t' = punsafeBuiltin PLC.QuotientInteger # t # t'
  {-# INLINEABLE pfindScale #-}
  pfindScale t t' = punsafeCoerce (pgcd # t #$ punsafeCoerce t')

-- | @since 1.0
instance PFractionable PNZNatural where
  {-# INLINEABLE pscale #-}
  pscale t t' = punsafeBuiltin PLC.MultiplyInteger # t # t'
  {-# INLINEABLE punscale #-}
  punscale t t' = punsafeBuiltin PLC.QuotientInteger # t # t'
  {-# INLINEABLE pfindScale #-}
  pfindScale t t' = punsafeCoerce (pgcd #$ punsafeCoerce t #$ punsafeCoerce t')

-- | @since 1.0
instance PFractionable PNatural where
  {-# INLINEABLE pscale #-}
  pscale t t' = punsafeBuiltin PLC.MultiplyInteger # t # t'
  {-# INLINEABLE punscale #-}
  punscale t t' = punsafeBuiltin PLC.QuotientInteger # t # t'
  {-# INLINEABLE pfindScale #-}
  pfindScale t t' = punsafeCoerce (pgcd #$ punsafeCoerce t #$ punsafeCoerce t')

-- Helpers

pgcd :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
pgcd = pfix #$ plam $ \self x y -> pif (y #== 0) x (self # y # (prem # x # y))
