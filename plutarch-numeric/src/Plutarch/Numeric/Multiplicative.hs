module Plutarch.Numeric.Multiplicative (
  MultiplicativeSemigroup(..),
  MultiplicativeMonoid (..)
  ) where

import Plutarch.Numeric.NZInteger (NZInteger (NZInteger), PNZInteger)
import Plutarch.Numeric.Natural (Natural (Natural), PNatural)
import Plutarch.Numeric.NZNatural (NZNatural (NZNatural), PNZNatural)
import Plutarch (Term, (#))
import Plutarch.Bool (pif, (#<), (#==))
import Plutarch.Integer (PInteger)
import qualified PlutusCore as PLC
import Plutarch.Unsafe (punsafeBuiltin, punsafeCoerce)
import Plutarch.Lift (pconstant)

{- | A semigroup, meant to be morally equivalent to numerical multiplication.

 = Laws

 Formally, an instance of 'MultiplicativeSemigroup' must be a semigroup with
 '*' as its operation. Furthermore, 'Additive' 'NZNatural' must be a right
 semigroup action, and 'Multiplicative' 'NZNatural' must translate to composition,
 both witnessed by 'powNZNatural'.

 This requires that @(x '*' y) '*' z = x '*' (y '*' z)@. Furthermore,
 'powNZNatural' must follow these laws:

 * @'powNZNatural' x 'one'@ @=@ @x@
 * @'powNZNatural' x (n '+' m)@ @=@ @'powNZNatural' x n '*' 'powNZNatural' x m@
 * @'powNZNatural' x (n '*' m)@ @=@ @'powNZNatural' ('powNZNatural' x n) m@

 @since 1.0
-}
class MultiplicativeSemigroup a where
  {-# MINIMAL (*) #-}

  -- | @since 1.0
  (*) :: a -> a -> a

{-
  -- This uses a default \'exponentiation by squaring\' implementation.
  --
  -- @since 1.0
  powNZNatural :: a -> NZNatural -> a
  powNZNatural x (NZN.NZNatural n) = getMultiplicative . stimes n . Multiplicative $ x
-}

-- | @since 1.0
instance MultiplicativeSemigroup Integer where
  {-# INLINEABLE (*) #-}
  (*) = (Prelude.*)
{-
{-# INLINEABLE powNZNatural #-}
  powNZNatural x (NZN.NZNatural n) = x Prelude.^ n
-}

-- | @since 1.0
deriving via Integer instance (MultiplicativeSemigroup NZInteger)

-- | @since 1.0
deriving via Integer instance (MultiplicativeSemigroup Natural)

-- | @since 1.0
deriving via Integer instance (MultiplicativeSemigroup NZNatural)

-- | @since 1.0
instance MultiplicativeSemigroup (Term s PInteger) where
  {-# INLINEABLE (*) #-}
  (*) = (Prelude.*)

-- | @since 1.0
instance MultiplicativeSemigroup (Term s PNatural) where
  {-# INLINEABLE (*) #-}
  x * y = punsafeBuiltin PLC.MultiplyInteger # x # y

-- | @since 1.0
instance MultiplicativeSemigroup (Term s PNZNatural) where
  {-# INLINEABLE (*) #-}
  x * y = punsafeBuiltin PLC.MultiplyInteger # x # y

-- | @since 1.0
instance MultiplicativeSemigroup (Term s PNZInteger) where
  {-# INLINEABLE (*) #-}
  x * y = punsafeBuiltin PLC.MultiplyInteger # x # y

{- | A 'MultiplicativeSemigroup' extended with a notion of unit.

 = Laws

 Formally, an instance of 'AdditiveMonoid' must be a monoid with 'one' as its
 identity. Furthermore, it must form a left-'Natural' semimodule, witnessed
 by 'powNatural', which must be an extension of the right semigroup action
 described by 'powNZNatural' for nonzero actions.

 This requires that @'one' '*' x = x '*' 'one' = x@. Furthermore, 'powNatural'
 must follow these laws:

 * If @'Just' m = 'removeZero' n@, then @'powNatural' x n = 'powNatural' x m@.
 * @'powNatural' x 'zero'@ @=@ @'one'@.

 Lastly, 'abs' and 'signum' must follow these laws:

 * @'abs' 'one'@ @=@ @'signum' 'one'@ @=@ @'one'@
 * @'abs' x '*' 'signum' x@ @=@ @x@
 * @'abs' (x '*' y)@ @=@ @'abs' x '*' 'abs' y@

 @since 1.0
-}
class (MultiplicativeSemigroup a) => MultiplicativeMonoid a where
  {-# MINIMAL one, abs, signum #-}

  -- | @since 1.0
  one :: a

  -- | @since 1.0
  abs :: a -> a

  -- | @since 1.0
  signum :: a -> a

{-
  -- This uses a default \'exponentiation by squaring\' implementation.
  --
  -- @since 1.0
  powNatural :: a -> Natural -> a
  powNatural x (Nat.Natural n) =
    getMultiplicative . stimesMonoid n . Multiplicative $ x
-}

-- | @since 1.0
instance MultiplicativeMonoid Integer where
  {-# INLINEABLE one #-}
  one = 1
  {-# INLINEABLE abs #-}
  abs = Prelude.abs
  {-# INLINEABLE signum #-}
  signum = Prelude.signum
{-
{-# INLINEABLE powNatural #-}
  powNatural x (Nat.Natural n) = x Prelude.^ n
-}

-- | @since 1.0
deriving via Integer instance MultiplicativeMonoid NZInteger

-- | @since 1.0
deriving via Integer instance MultiplicativeMonoid Natural

-- | @since 1.0
deriving via Integer instance MultiplicativeMonoid NZNatural

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
  signum t = pif (t #== punsafeCoerce (0 :: Term s PInteger)) 
                 (punsafeCoerce (0 :: Term s PInteger)) one

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


