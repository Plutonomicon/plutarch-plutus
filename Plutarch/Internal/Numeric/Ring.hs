module Plutarch.Internal.Numeric.Ring (
  -- * Type classes
  PRing (..),
  PIntegralDomain (..),
) where

import Data.Kind (Type)
import Plutarch.Builtin.Bool (pcond, pif)
import Plutarch.Builtin.Integer (PInteger, pconstantInteger)
import Plutarch.Internal.Eq ((#==))
import Plutarch.Internal.Numeric.Additive (PAdditiveGroup (pnegate), pzero)
import Plutarch.Internal.Numeric.Multiplicative (PMultiplicativeMonoid (pone))
import Plutarch.Internal.Ord (POrd ((#<=)))
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.PlutusType (PInner)
import Plutarch.Internal.Term (S, Term, phoistAcyclic, (#), (:-->))
import Plutarch.Unsafe (punsafeDowncast)

{- | = Laws

1. @pfromInteger 0@ @=@ @pzero@
2. @pfromInteger 1@ @=@ @pone@
3. @pfromInteger (x + y)@ @=@ @pfromInteger x #+ pfromInteger y@
4. @pfromInteger (x * y)@ @=@ @pfromInteger x #* pfromInteger y@

Additionally, the following \'interaction laws\' must hold between the
instances of 'PAdditiveGroup' and 'PMultiplicativeMonoid' for @a@:

5. @x #* (y #+ z)@ @=@ @(x #* y) #+ (x #* z)@ (@#*@ left-distributes over
   @#+@)
6. @(y #+ z) #* x@ @=@ @(y #* x) #+ (z #* x)@ (@#*@ right-distributes over
   @#+@)

@since WIP
-}
class
  (PAdditiveGroup a, PMultiplicativeMonoid a) =>
  PRing (a :: S -> Type)
  where
  pfromInteger :: forall (s :: S). Integer -> Term s a
  default pfromInteger :: forall (s :: S). PRing (PInner a) => Integer -> Term s a
  pfromInteger x = punsafeDowncast $ pfromInteger x

-- | @since WIP
instance PRing PInteger where
  {-# INLINEABLE pfromInteger #-}
  pfromInteger = pconstantInteger

{- | = Laws

= Pedantry note

Technically, the requirements here are too strong: we demand an /ordered/
ring, which integral domains don't necessarily have to be. However, in our
case, our hand is forced by expected semantics: in abstract algebra, both the
absolute value and the signum are real numbers (which are always totally
ordered) but in our case, both must be elements of the integral domain
itself. Thus, in order for the laws to make any sense, we have to ensure a
total order on the integral domain. Since all of our integral domains are
\'at least as big\' as the integers, this doesn't pose a huge problem.

@since WIP
-}
class (PRing a, POrd a) => PIntegralDomain (a :: S -> Type) where
  {-# INLINEABLE psignum #-}
  psignum :: forall (s :: S). Term s (a :--> a)
  default psignum :: forall (s :: S). Term s (a :--> a)
  psignum = phoistAcyclic $ plam $ \x ->
    pcond
      [ (x #== pzero, pzero)
      , (x #<= pzero, pnegate # pone)
      ]
      pone
  {-# INLINEABLE pabs #-}
  pabs :: forall (s :: S). Term s (a :--> a)
  default pabs :: forall (s :: S). Term s (a :--> a)
  pabs = phoistAcyclic $ plam $ \x ->
    pif
      (x #<= pzero)
      (pnegate # x)
      x

-- | @since WIP
instance PIntegralDomain PInteger where
  {-# INLINEABLE psignum #-}
  psignum = phoistAcyclic $ plam $ \x ->
    pcond
      [ (x #== pconstantInteger 0, pconstantInteger 0)
      , (x #<= pconstantInteger 0, pconstantInteger (-1))
      ]
      (pconstantInteger 1)
