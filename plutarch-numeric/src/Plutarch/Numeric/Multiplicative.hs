module Plutarch.Numeric.Multiplicative (
  MultiplicativeSemigroup (..),
  MultiplicativeMonoid (..),
) where

import Plutarch (Term, pcon, plet, (#))
import Plutarch.Bool (pif, (#<), (#==))
import Plutarch.Integer (PInteger)
import Plutarch.Lift (pconstant)
import Plutarch.Numeric.Fractional (Fractionable, PFractionable)
import Plutarch.Numeric.NZInteger (NZInteger (NZInteger), PNZInteger)
import Plutarch.Numeric.NZNatural (NZNatural (NZNatural), PNZNatural)
import Plutarch.Numeric.Natural (Natural (Natural), PNatural)
import Plutarch.Numeric.Ratio (
  PRatio,
  Ratio (Ratio),
  pconRatio,
  pmatchRatio,
  pmatchRatios,
  ratio,
 )
import Plutarch.Pair (PPair (PPair))
import Plutarch.Unsafe (punsafeBuiltin, punsafeCoerce)
import PlutusCore qualified as PLC
import Prelude hiding (abs, signum, (*))
import Prelude qualified

{- | A semigroup, meant to be morally equivalent to numerical multiplication.

 = Laws

 Formally, an instance of 'MultiplicativeSemigroup' must be a semigroup with
 '*' as its operation. This requires that @(x '*' y) '*' z = x '*' (y '*' z)@.

 = Note

 In general, multiplication is /not/ commutative. Types for which it is will
 specify it as such in their instance documentation.

 @since 1.0
-}
class MultiplicativeSemigroup a where
  -- | @since 1.0
  (*) :: a -> a -> a

infixl 7 *

{- | Multiplication for 'Integer' is commutative.

 @since 1.0
-}
instance MultiplicativeSemigroup Integer where
  {-# INLINEABLE (*) #-}
  (*) = (Prelude.*)

{- | Multiplication for 'NZInteger' is commutative.

 @since 1.0
-}
deriving via Integer instance (MultiplicativeSemigroup NZInteger)

{- | Multiplication for 'Natural' is commutative.

 @since 1.0
-}
deriving via Integer instance (MultiplicativeSemigroup Natural)

{- | Multiplication for 'NZNatural' is commutative.

 @since 1.0
-}
deriving via Integer instance (MultiplicativeSemigroup NZNatural)

{- | Multiplication for @'Ratio' a@ is commutative whenever multiplication for
 @a@ is.

 @since 1.0
-}
instance
  (Fractionable a, MultiplicativeSemigroup a) =>
  MultiplicativeSemigroup (Ratio a)
  where
  {-# INLINEABLE (*) #-}
  Ratio (num, den) * Ratio (num', den') = ratio (num * num') (den * den')

{- | Multiplication for @'Term' s 'PInteger'@ is commutative.

 @since 1.0
-}
instance MultiplicativeSemigroup (Term s PInteger) where
  {-# INLINEABLE (*) #-}
  (*) = (Prelude.*)

{- | Multiplication for @'Term' s 'PNatural'@ is commutative.

 @since 1.0
-}
instance MultiplicativeSemigroup (Term s PNatural) where
  {-# INLINEABLE (*) #-}
  x * y = punsafeBuiltin PLC.MultiplyInteger # x # y

{- | Multiplication for @'Term' s 'PNZNatural'@ is commutative.

 @since 1.0
-}
instance MultiplicativeSemigroup (Term s PNZNatural) where
  {-# INLINEABLE (*) #-}
  x * y = punsafeBuiltin PLC.MultiplyInteger # x # y

{- | Multiplication for @'Term' s 'PNZInteger'@ is commutative.

 @since 1.0
-}
instance MultiplicativeSemigroup (Term s PNZInteger) where
  {-# INLINEABLE (*) #-}
  x * y = punsafeBuiltin PLC.MultiplyInteger # x # y

{- | Multiplication for @'Term' s ('PRatio' a)@ is commutative whenever
 multiplication for @'Term' s a@ is.

 @since 1.0
-}
instance
  (PFractionable a, MultiplicativeSemigroup (Term s a)) =>
  MultiplicativeSemigroup (Term s (PRatio a))
  where
  {-# INLINEABLE (*) #-}
  t * t' = pmatchRatios t t' $ \num num' den den' ->
    plet (num * num') $ \newNum ->
      plet (den * den') $ \newDen ->
        pconRatio newNum newDen

{- | A 'MultiplicativeSemigroup' extended with a notion of unit.

 = Laws

 Formally, an instance of 'MultiplicativeMonoid' must be a monoid with 'one' as its
 identity. This requires that @'one' '*' x = x '*' 'one' = x@. Furthermore,
 'abs' and 'signum' must follow these laws:

 * @'abs' 'one'@ @=@ @'signum' 'one'@ @=@ @'one'@
 * @'abs' x '*' 'signum' x@ @=@ @x@
 * @'abs' (x '*' y)@ @=@ @'abs' x '*' 'abs' y@

 @since 1.0
-}
class (MultiplicativeSemigroup a) => MultiplicativeMonoid a where
  -- | @since 1.0
  one :: a

  -- | @since 1.0
  abs :: a -> a

  -- | @since 1.0
  signum :: a -> a

-- | @since 1.0
instance MultiplicativeMonoid Integer where
  {-# INLINEABLE one #-}
  one = 1
  {-# INLINEABLE abs #-}
  abs = Prelude.abs
  {-# INLINEABLE signum #-}
  signum = Prelude.signum

-- | @since 1.0
deriving via Integer instance MultiplicativeMonoid NZInteger

-- | @since 1.0
deriving via Integer instance MultiplicativeMonoid Natural

-- | @since 1.0
deriving via Integer instance MultiplicativeMonoid NZNatural

-- | @since 1.0
instance
  (Fractionable a, MultiplicativeMonoid a) =>
  MultiplicativeMonoid (Ratio a)
  where
  {-# INLINEABLE one #-}
  one = Ratio (one, one)
  {-# INLINEABLE abs #-}
  abs (Ratio (num, den)) = Ratio (abs num, den)
  {-# INLINEABLE signum #-}
  signum (Ratio (num, _)) = Ratio (signum num, one)

-- | @since 1.0
instance MultiplicativeMonoid (Term s PInteger) where
  {-# INLINEABLE one #-}
  one = 1
  {-# INLINEABLE abs #-}
  abs t = pif (t #< 0) (Prelude.negate t) t
  {-# INLINEABLE signum #-}
  signum t = pif (t #< 0) (Prelude.negate 1) . pif (t #== 0) 0 $ one

-- | @since 1.0
instance MultiplicativeMonoid (Term s PNatural) where
  {-# INLINEABLE one #-}
  one = pconstant . Natural $ 1
  {-# INLINEABLE abs #-}
  abs = id
  {-# INLINEABLE signum #-}
  signum t =
    pif
      (t #== punsafeCoerce (0 :: Term s PInteger))
      (punsafeCoerce (0 :: Term s PInteger))
      one

-- | @since 1.0
instance MultiplicativeMonoid (Term s PNZInteger) where
  {-# INLINEABLE one #-}
  one = pconstant . NZInteger $ 1
  {-# INLINEABLE abs #-}
  abs t = pif (t #< one) go t
    where
      go :: Term s PNZInteger
      go = punsafeCoerce . Prelude.negate @(Term s PInteger) . punsafeCoerce $ t
  {-# INLINEABLE signum #-}
  signum t = pif (t #< one) (pconstant . NZInteger $ (-1)) one

-- | @since 1.0
instance MultiplicativeMonoid (Term s PNZNatural) where
  {-# INLINEABLE one #-}
  one = pconstant . NZNatural $ 1
  {-# INLINEABLE abs #-}
  abs = id
  {-# INLINEABLE signum #-}
  signum _ = one

-- | @since 1.0
instance
  (PFractionable a, MultiplicativeMonoid (Term s a)) =>
  MultiplicativeMonoid (Term s (PRatio a))
  where
  {-# INLINEABLE one #-}
  one = punsafeCoerce . pcon $ PPair (one :: Term s a) (one :: Term s PNZNatural)
  {-# INLINEABLE abs #-}
  abs t = pmatchRatio t $ \num den -> punsafeCoerce . pcon $ PPair (abs num) den
  {-# INLINEABLE signum #-}
  signum t = pmatchRatio t $ \num _ ->
    punsafeCoerce . pcon $ PPair (signum num) (one :: Term s PNZNatural)
