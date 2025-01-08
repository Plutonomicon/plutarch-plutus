module Plutarch.Internal.Semigroup (
  -- * Type classes
  PSemigroup (..),
  PMonoid (..),
) where

import Data.ByteString qualified as BS
import Data.Kind (Type)
import Data.Text qualified as Text
import Plutarch.Builtin.BLS (
  PBuiltinBLS12_381_G1_Element,
  PBuiltinBLS12_381_G2_Element,
  PBuiltinBLS12_381_MlResult,
 )
import Plutarch.Builtin.Bool (pif)
import Plutarch.Builtin.ByteString (
  PByteString,
  pindexBS,
  plengthBS,
  preplicateBS,
 )
import Plutarch.Builtin.String (PString)
import Plutarch.Builtin.Unit (PUnit, punit)
import Plutarch.Internal.Eq ((#==))
import Plutarch.Internal.Numeric (
  PNatural,
  PPositive,
  pbySquaringDefault,
  pnaturalToPositiveCPS,
  pscaleNatural,
  pscalePositive,
  pzero,
  (#*),
  (#+),
 )
import Plutarch.Internal.Other (pto)
import Plutarch.Internal.PlutusType (PInner)
import Plutarch.Internal.Term (
  S,
  Term,
  plet,
  punsafeBuiltin,
  punsafeConstantInternal,
  (#),
  (#$),
 )
import Plutarch.Unsafe (punsafeDowncast)
import PlutusCore qualified as PLC

{- | = Laws

The only mandatory law is that '#<>' must be associative:

@x #<> (y #<> z)@ @=@ @(x #<> y) #<> z@

If you define 'pstimes', ensure the following also hold:

1. @pstimes pone x@ @=@ @x@
2. @(pstimes p1 x) #<> (pstimes p2 x)@ @=@ @pstimes (p1 #+ p2) x@
3. @pstimes p1 (pstimes p2 x)@ @=@ @pstimes (p1 #* p2) x@

The default implementation automatically ensures these laws hold.

@since WIP
-}
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

infixr 6 #<>

-- | @since WIP
instance PSemigroup PUnit where
  {-# INLINEABLE (#<>) #-}
  x #<> y = plet x $ \_ -> plet y $ const punit
  {-# INLINEABLE pstimes #-}
  pstimes _ x = plet x $ const punit

-- | @since WIP
instance PSemigroup PString where
  {-# INLINEABLE (#<>) #-}
  x #<> y = punsafeBuiltin PLC.AppendString # x # y

-- | @since WIP
instance PSemigroup PByteString where
  {-# INLINEABLE (#<>) #-}
  x #<> y = punsafeBuiltin PLC.AppendByteString # x # y
  {-# INLINEABLE pstimes #-}
  pstimes p bs = plet bs $ \bs' ->
    pif
      (plengthBS # bs' #== 1)
      (preplicateBS # pto p #$ pindexBS # bs' # 0)
      (pbySquaringDefault (#<>) bs' p)

{- | BLS points form a group technically, but a @PGroup@ notion would be too
niche to be useful. Unlike other types which could be semigroups (or monoids)
in many ways, BLS points have only one (essentially their additive
instances), so we can provide these.

@since WIP
-}
instance PSemigroup PBuiltinBLS12_381_G1_Element where
  {-# INLINEABLE (#<>) #-}
  (#<>) = (#+)
  {-# INLINEABLE pstimes #-}
  pstimes p x = pscalePositive x p

-- | @since WIP
instance PSemigroup PBuiltinBLS12_381_G2_Element where
  {-# INLINEABLE (#<>) #-}
  (#<>) = (#+)
  {-# INLINEABLE pstimes #-}
  pstimes p x = pscalePositive x p

{- | Since multiplication of Miller loop results exists, they are technically
semigroups, though confusingly in a /different/ way to BLS curve points.

@since WIP
-}
instance PSemigroup PBuiltinBLS12_381_MlResult where
  {-# INLINEABLE (#<>) #-}
  (#<>) = (#*)

{- | = Laws

1. @pmempty #<> x@ @=@ @x #<> pmempty@ @=@ @x@
2. @pstimes n pmempty@ @=@ @pmempty@

If you define 'pmtimes', ensure the following as well:

3. @pmtimes (ppositiveToNatural # p) x@ @=@
   @pstimes p x@
4. @pmtimes pzero x@ @=@ @pmempty@

The default implementation of 'pmtimes' ensures these laws hold.

@since WIP
-}
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

-- | @since WIP
instance PMonoid PUnit where
  {-# INLINEABLE pmempty #-}
  pmempty = punit
  {-# INLINEABLE pmtimes #-}
  pmtimes _ x = plet x $ const punit

-- | @since WIP
instance PMonoid PString where
  {-# INLINEABLE pmempty #-}
  pmempty = punsafeConstantInternal $ PLC.someValue Text.empty

-- | @since WIP
instance PMonoid PByteString where
  {-# INLINEABLE pmempty #-}
  pmempty = punsafeConstantInternal $ PLC.someValue BS.empty

-- | @since WIP
instance PMonoid PBuiltinBLS12_381_G1_Element where
  {-# INLINEABLE pmempty #-}
  pmempty = pzero
  {-# INLINEABLE pmtimes #-}
  pmtimes n x = pscaleNatural x n

-- | @since WIP
instance PMonoid PBuiltinBLS12_381_G2_Element where
  {-# INLINEABLE pmempty #-}
  pmempty = pzero
  {-# INLINEABLE pmtimes #-}
  pmtimes n x = pscaleNatural x n
