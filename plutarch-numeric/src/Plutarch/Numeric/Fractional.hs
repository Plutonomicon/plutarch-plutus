module Plutarch.Numeric.Fractional (
  Fractionable (..),
  PFractionable (..),
) where

import Data.Kind (Type)
import Plutarch (S, Term, pfix, plam, (#), (#$), type (:-->))
import Plutarch.Bool (pif, (#==))
import Plutarch.Integer (PInteger, prem)
import Plutarch.Numeric.NZInteger (NZInteger (NZInteger), PNZInteger)
import Plutarch.Numeric.NZNatural (NZNatural (NZNatural), PNZNatural)
import Plutarch.Numeric.Natural (Natural (Natural), PNatural)
import Plutarch.Unsafe (punsafeBuiltin, punsafeCoerce)
import PlutusCore qualified as PLC

{- | Represents a type's ability to be extended to the field of fractions. Also
 provides key methods required for the implementation of
 'Plutarch.Numeric.Ratio.Ratio' to function
 correctly.

 = Laws

 'scale' should describe a right monoidal action of
 'Plutarch.Numeric.Monoidal.Multiplicative' 'NZNatural' on @a@, with 'unscale' acting
 as a cancellation. Specifically:

 * @'scale' x 'Plutarch.Numeric.Multiplicative.one'@ @=@ @'unscale' x
 'Plutarch.Numeric.Multiplicative.one'@ @=@ @x@
 * @'scale' x (n 'Plutarch.Numeric.Multiplicative.*' m)@ @=@ @'scale' ('scale'
 x n) m@
 * @'unscale' ('scale' x n) n@ @=@ @x@

 'findScale' should follow these laws:

 * @'findScale' x 'Plutarch.Numeric.Multiplicative.one'@ @=@
 @'Plutarch.Numeric.Multiplicative.one'@
 * @'findScale' ('scale' x m) (m 'Plutarch.Numeric.Multiplicative.*' n)@ @=@
 @m 'Plutarch.Numeric.Multiplicative.*' 'findScale' x n@
 * If @m = 'findScale' x n@, then @'findScale' ('unscale' x m) m =
 'Plutarch.Numeric.Multiplicative.one'@

 @since 1.0
-}
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

{- | This is \'morally equivalent\' to 'Fractionable', except that it's designed
 for convenient use with Plutarch 'Term's.

 = Laws

 The laws are \'morally the same\' as those for 'Fractionable', but we restate
 them here for clarity.

 * @'pscale' x 'Plutarch.Numeric.Multiplicative.one'@ @=@ @'punscale' x
 'Plutarch.Numeric.Multiplicative.one'@ @=@ @x@
 * @'pscale' x (n 'Plutarch.Numeric.Multiplicative.*' m)@ @=@ @'pscale' ('pscale'
 x n) m@
 * @'punscale' ('pscale' x n) n@ @=@ @x@
 * @'pfindScale' x 'Plutarch.Numeric.Multiplicative.one'@ @=@
 @'Plutarch.Numeric.Multiplicative.one'@
 * @'pfindScale' ('pscale' x m) (m 'Plutarch.Numeric.Multiplicative.*' n)@ @=@
 @m 'Plutarch.Numeric.Multiplicative.*' 'pfindScale' x n@
 * If @m = 'pfindScale' x n@, then @'pfindScale' ('punscale' x m) m =
 'Plutarch.Numeric.Multiplicative.one'@

 @since 1.0
-}
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
instance PFractionable PNZInteger where
  {-# INLINEABLE pscale #-}
  pscale t t' = punsafeBuiltin PLC.MultiplyInteger # t # t'
  {-# INLINEABLE punscale #-}
  punscale t t' = punsafeBuiltin PLC.QuotientInteger # t # t'
  {-# INLINEABLE pfindScale #-}
  pfindScale t t' = punsafeCoerce (pgcd #$ punsafeCoerce t #$ punsafeCoerce t')

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
