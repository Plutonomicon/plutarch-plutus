{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Trustworthy #-}

{- | Module: Plutarch.Numeric
 Copyright: (C) MLabs 2022
 License: MIT
 Maintainer: Koz Ross <koz@mlabs.city>
 Portability: GHC only
 Stability: Experimental

 Improved numerical hierarchy.
-}
module Plutarch.Numeric (
  -- * Types

  -- ** Haskell
  Natural,
  NZNatural,
  NZInteger,

  -- ** Plutarch
  PNatural,
  PNZNatural,
  PNZInteger,

  -- ** Helpers
  Additive (..),
  Multiplicative (..),

  -- * Type classes

  -- ** Additive
  AdditiveSemigroup (..),
  AdditiveMonoid (..),
  AdditiveGroup (..),
  AdditiveCMM (..),

  -- ** Multiplicative
  MultiplicativeSemigroup (..),
  MultiplicativeMonoid (..),

  -- ** Combination
  Distributive (..),
  Euclidean (..),
  Arithmetical (..),
  Divisible (..),
) where

import Data.Kind (Type)
import Data.Semigroup (stimes, stimesMonoid)
import Plutarch (Term, pcon, plet, (#))
import Plutarch.Bool (pif, (#<), (#<=), (#==))
import Plutarch.Integer (PInteger)
import Plutarch.Lift (pconstant)
import Plutarch.Maybe (PMaybe (PJust, PNothing))
import Plutarch.Numeric.Group (Group (gtimes, inverse))
import Plutarch.Numeric.NZInteger (NZInteger, PNZInteger)
import Plutarch.Numeric.NZInteger qualified as NZI
import Plutarch.Numeric.NZNatural (NZNatural, PNZNatural)
import Plutarch.Numeric.NZNatural qualified as NZN
import Plutarch.Numeric.Natural (Natural, PNatural)
import Plutarch.Numeric.Natural qualified as Nat
import Plutarch.Unsafe (punsafeBuiltin, punsafeCoerce)
import PlutusCore qualified as PLC
import Prelude hiding (
  Integral,
  abs,
  div,
  divMod,
  mod,
  negate,
  product,
  quot,
  quotRem,
  rem,
  signum,
  sum,
  (*),
  (+),
  (-),
 )
import Prelude qualified

{- | A commutative semigroup, meant to be morally equivalent to numerical
 addition.

 = Laws

 Formally, an instance of 'AdditiveSemigroup' must be a commutative semigroup
 with '+' as its operation. Furthermore, 'Additive' 'NZNatural' must be a
 right semigroup action, and [TODO], both witnessed by 'scaleNZNatural'.

 This requires that '+' commutes and associates:

 * @x '+' y@ @=@ @y '+' x@
 * @(x '+' y) '+' z@ @=@ @x '+' (y '+' z)@

 Furthermore, 'scaleNZNatural' must follow these laws:

 * @'scaleNZNatural' x 'one'@ @=@ @x@
 * @'scaleNZNatural' x (n '+' m)@ @=@ @'scaleNZNatural' x n '+' 'scaleNZNatural' x m@
 * @'scaleNZNatural' x (n '*' m)@ @=@ @'scaleNZNatural' ('scaleNZNatural' x n) m@

 @since 1.0
-}
class AdditiveSemigroup a where
  {-# MINIMAL (+) #-}

  -- | @since 1.0
  (+) :: a -> a -> a

  -- | This uses a default \'sum by squaring\' implementation. While this is
  -- reasonably good by default, many types allow a more efficient
  -- implementation; try and define one if you can.
  --
  -- @since 1.0
  {-# INLINEABLE scaleNZNatural #-}
  scaleNZNatural :: a -> NZNatural -> a
  scaleNZNatural x (NZN.NZNatural n) = getAdditive . stimes n . Additive $ x

-- | @since 1.0
instance AdditiveSemigroup Integer where
  {-# INLINEABLE (+) #-}
  (+) = (Prelude.+)
  {-# INLINEABLE scaleNZNatural #-}
  scaleNZNatural i (NZN.NZNatural n) = i Prelude.* n

-- | @since 1.0
deriving via Integer instance (AdditiveSemigroup Natural)

-- | @since 1.0
deriving via Integer instance (AdditiveSemigroup NZNatural)

-- | @since 1.0
instance AdditiveSemigroup (Term s PInteger) where
  {-# INLINEABLE (+) #-}
  x + y = punsafeBuiltin PLC.AddInteger # x # y
  {-# INLINEABLE scaleNZNatural #-}
  scaleNZNatural i (NZN.NZNatural n) =
    punsafeBuiltin PLC.MultiplyInteger # i # pconstant n

-- | @since 1.0
instance AdditiveSemigroup (Term s PNatural) where
  {-# INLINEABLE (+) #-}
  x + y = punsafeBuiltin PLC.AddInteger # x # y
  {-# INLINEABLE scaleNZNatural #-}
  scaleNZNatural i (NZN.NZNatural n) =
    punsafeBuiltin PLC.MultiplyInteger # i # pconstant n

-- | @since 1.0
instance AdditiveSemigroup (Term s PNZNatural) where
  {-# INLINEABLE (+) #-}
  x + y = punsafeBuiltin PLC.AddInteger # x # y
  {-# INLINEABLE scaleNZNatural #-}
  scaleNZNatural i (NZN.NZNatural n) =
    punsafeBuiltin PLC.MultiplyInteger # i # pconstant n

{- | An 'AdditiveSemigroup' extended with a notion of zero.

 = Laws

 Formally, an instance of 'AdditiveMonoid' must be a commutative monoid with
 'zero' as its identity. Furthermore, it must form a left-'Natural'
 semimodule, witnessed by 'scaleNatural', which must be an extension of the
 right semigroup action described by 'scaleNZNatural' for nonzero actions.

 This requires that @'zero' '+' x = x '+' 'zero' = x@. Furthermore,
 'scaleNatural' must follow these laws:

 * If @'Just' m = 'removeZero' n@, then @'scaleNatural' x n = 'scaleNZNatural' x m@.
 * @'scaleNatural' x 'zero'@ @=@ @'zero'@

 @since 1.0
-}
class (AdditiveSemigroup a) => AdditiveMonoid a where
  {-# MINIMAL zero #-}

  -- | @since 1.0
  zero :: a

  -- | This uses a default \'sum by squaring\' implementation. While this is
  -- reasonably good by default, many types allow a more efficient
  -- implementation; try and define one if you can.
  --
  -- @since 1.0
  {-# INLINEABLE scaleNatural #-}
  scaleNatural :: a -> Natural -> a
  scaleNatural x (Nat.Natural n) = getAdditive . stimesMonoid n . Additive $ x

-- | @since 1.0
instance AdditiveMonoid Integer where
  {-# INLINEABLE zero #-}
  zero = 0
  {-# INLINEABLE scaleNatural #-}
  scaleNatural i (Nat.Natural n) = i Prelude.* n

-- | @since 1.0
deriving via Integer instance (AdditiveMonoid Natural)

-- | @since 1.0
instance AdditiveMonoid (Term s PInteger) where
  {-# INLINEABLE zero #-}
  zero = pconstant 0
  {-# INLINEABLE scaleNatural #-}
  scaleNatural i n = punsafeBuiltin PLC.MultiplyInteger # i # pconstant n

-- | @since 1.0
instance AdditiveMonoid (Term s PNatural) where
  {-# INLINEABLE zero #-}
  zero = pconstant . Nat.Natural $ 0
  {-# INLINEABLE scaleNatural #-}
  scaleNatural i n = punsafeBuiltin PLC.MultiplyInteger # i # pconstant n

{- | An 'AdditiveMonoid' extended with a notion of negation.

 = Laws

 Formally, an instance of 'AdditiveGroup' must be an abelian group with
 'negate' as its inverse-constructing operation. Furthermore, it must form a
 left-'Integer' semimodule, witnessed by 'scaleInteger', which must be an
 extension of the left-'Natural' semimodule witnessed by 'scaleNatural' for
 non-negative elements.

 This requires that @x '+' 'negate' x = 'zero'@ and @x '-' y = x '+'
 'negate' y@; the second of these is the default implementation of '-'.

 /TODO:/ Stating 'scaleInteger' agreement is difficult until we define a
 \'constructive absolute value\'.

 @since 1.0
-}
class (AdditiveMonoid a) => AdditiveGroup a where
  {-# MINIMAL negate #-}

  -- | @since 1.0
  (-) :: a -> a -> a
  x - y = x + negate y

  -- | @since 1.0
  negate :: a -> a

  -- | This uses a default \'sum by squaring\' implementation. While this is
  -- reasonably good by default, many types allow a more efficient
  -- implementation; try and define one if you can.
  --
  -- @since 1.0
  {-# INLINEABLE scaleInteger #-}
  scaleInteger :: a -> Integer -> a
  scaleInteger x i = getAdditive . gtimes i . Additive $ x

-- | @since 1.0
instance AdditiveGroup Integer where
  {-# INLINEABLE (-) #-}
  (-) = (Prelude.-)
  {-# INLINEABLE negate #-}
  negate = Prelude.negate
  {-# INLINEABLE scaleInteger #-}
  scaleInteger = (Prelude.*)

-- | @since 1.0
instance AdditiveGroup (Term s PInteger) where
  {-# INLINEABLE (-) #-}
  (-) = (Prelude.-)
  {-# INLINEABLE negate #-}
  negate = Prelude.negate
  {-# INLINEABLE scaleInteger #-}
  scaleInteger x i = x Prelude.* pconstant i

{- | Extends an 'AdditiveMonoid' with a notion of \'difference-or-zero\'.

 = Laws

 Formally, an 'AdditiveCMM' must be a commutative monoid with monus, with '^-'
 as its monus operation. This requires that '^-' follow these laws:

 * @x '+' (y '^-' x)@ @=@ @y '+' (x '^-' y)@
 * @(x '^-' y) '^-' z@ @=@ @x '^-' (y '+' z)@
 * @x '^-' x@ @=@ @'zero'@
 * @'zero' '^-' x@ @=@ @'zero'@

 @since 1.0
-}
class (AdditiveMonoid a) => AdditiveCMM a where
  -- | @since 1.0
  (^-) :: a -> a -> a

-- | @since 1.0
instance AdditiveCMM Natural where
  {-# INLINEABLE (^-) #-}
  Nat.Natural n ^- Nat.Natural n' = Nat.Natural . max 0 $ n Prelude.- n'

-- | @since 1.0
instance AdditiveCMM (Term s PNatural) where
  {-# INLINEABLE (^-) #-}
  t ^- t' =
    plet
      (punsafeBuiltin PLC.SubtractInteger # t # t')
      (\(t'' :: Term s PInteger) -> pif (zero #<= t'') (punsafeCoerce t'') zero)

{- | A restriction of the type being wrapped to the \'additive half\' of its
 capabilities. This allows us to treat such types as 'Semigroup's, 'Monoid's
 or 'Group's when convenient to do so.

 @since 1.0
-}
newtype Additive (a :: Type) = Additive
  { getAdditive :: a
  -- ^ @since 1.0
  }

-- | @since 1.0
instance (AdditiveSemigroup a) => Semigroup (Additive a) where
  {-# INLINEABLE (<>) #-}
  Additive x <> Additive y = Additive $ x + y

-- | @since 1.0
instance (AdditiveMonoid a) => Monoid (Additive a) where
  {-# INLINEABLE mempty #-}
  mempty = Additive zero

-- | @since 1.0
instance (AdditiveGroup a) => Group (Additive a) where
  {-# INLINEABLE inverse #-}
  inverse (Additive x) = Additive . negate $ x

{- | A semigroup, meant to be morally equivalent to numerical multiplication.

 = Laws

 Formally, an instance of 'MultiplicativeSemigroup' must be a semigroup with
 '*' as its operation. Furthermore, 'Additive' 'NZNatural' must be a right
 semigroup action, and [TODO], both witnessed by 'powNZNatural'.

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

  -- This uses a default \'exponentiation by squaring\' implementation.
  --
  -- @since 1.0
  powNZNatural :: a -> NZNatural -> a
  powNZNatural x (NZN.NZNatural n) = getMultiplicative . stimes n . Multiplicative $ x

-- | @since 1.0
instance MultiplicativeSemigroup Integer where
  {-# INLINEABLE (*) #-}
  (*) = (Prelude.*)
  {-# INLINEABLE powNZNatural #-}
  powNZNatural x (NZN.NZNatural n) = x Prelude.^ n

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
 identity. Furthermore, it must form a left-'Natural' semimodule, withnessed
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

  -- This uses a default \'exponentiation by squaring\' implementation.
  --
  -- @since 1.0
  powNatural :: a -> Natural -> a
  powNatural x (Nat.Natural n) =
    getMultiplicative . stimesMonoid n . Multiplicative $ x

-- | @since 1.0
instance MultiplicativeMonoid Integer where
  {-# INLINEABLE one #-}
  one = 1
  {-# INLINEABLE abs #-}
  abs = Prelude.abs
  {-# INLINEABLE signum #-}
  signum = Prelude.signum
  {-# INLINEABLE powNatural #-}
  powNatural x (Nat.Natural n) = x Prelude.^ n

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
  abs t = pif (t #< zero) (Prelude.negate t) t
  {-# INLINEABLE signum #-}
  signum t = pif (t #< zero) (Prelude.negate 1) . pif (t #== zero) zero $ one

-- | @since 1.0
instance MultiplicativeMonoid (Term s PNatural) where
  {-# INLINEABLE one #-}
  one = pconstant (Nat.Natural 1)
  {-# INLINEABLE abs #-}
  abs = id
  {-# INLINEABLE signum #-}
  signum t = pif (t #== zero) zero one

-- | @since 1.0
instance MultiplicativeMonoid (Term s PNZInteger) where
  {-# INLINEABLE one #-}
  one = pconstant (NZI.NZInteger 1)
  {-# INLINEABLE abs #-}
  abs t = pif (t #< one) go t
    where
      go :: Term s PNZInteger
      go = punsafeCoerce . Prelude.negate @(Term s PInteger) . punsafeCoerce $ t
  {-# INLINEABLE signum #-}
  signum t = pif (t #< one) (pconstant . NZI.NZInteger $ (-1)) one

-- | @since 1.0
instance MultiplicativeMonoid (Term s PNZNatural) where
  {-# INLINEABLE one #-}
  one = pconstant (NZN.NZNatural 1)
  {-# INLINEABLE abs #-}
  abs = id
  {-# INLINEABLE signum #-}
  signum _ = one

{- | A restriction of the type being wrapped to the \'multiplicative half\' of
 its capabilities. This allows us to treat such types as 'Semigroup's,
 'Monoid's or 'Group's when convenient to do so.

 @since 1.0
-}
newtype Multiplicative (a :: Type) = Multiplicative
  { getMultiplicative :: a
  -- ^ @since 1.0
  }

-- | @since 1.0
instance (MultiplicativeSemigroup a) => Semigroup (Multiplicative a) where
  {-# INLINEABLE (<>) #-}
  Multiplicative x <> Multiplicative y = Multiplicative $ x * y

-- | @since 1.0
instance (MultiplicativeMonoid a) => Monoid (Multiplicative a) where
  {-# INLINEABLE mempty #-}
  mempty = Multiplicative one

{- | A semirig (semiring without a neutral additive element) formed from an
 'AdditiveSemigroup' and a 'MultiplicativeMonoid'.

 = Laws

 Multiplication left and right must distribute over addition:

 * @x '*' (y '+' z)@ @=@ @(x '*' y) '+' (x '*' z)@
 * @(x '+' y) '*' z@ @=@ @(x '*' z) '+' (y '*' z)@

 Furthermore, 'fromNZNatural' must be the unique semirig homomorphism from
 'NZNatural' to the instance. Thus:

 * @'fromNZNatural' 'one' = 'one'@; and
 * @'fromNZNatural' (x '+' y)@ @=@ @'fromNZNatural' x '+' 'fromNZNatural' y@

 @since 1.0
-}
class (AdditiveSemigroup a, MultiplicativeMonoid a) => Distributive a where
  -- | @since 1.0
  fromNZNatural :: NZNatural -> a

-- | @since 1.0
instance Distributive Integer where
  {-# INLINEABLE fromNZNatural #-}
  fromNZNatural (NZN.NZNatural i) = i

-- | @since 1.0
instance Distributive Natural where
  {-# INLINEABLE fromNZNatural #-}
  fromNZNatural (NZN.NZNatural n) = Nat.Natural n

-- | @since 1.0
instance Distributive NZNatural where
  {-# INLINEABLE fromNZNatural #-}
  fromNZNatural = id

-- | @since 1.0
instance Distributive (Term s PInteger) where
  {-# INLINEABLE fromNZNatural #-}
  fromNZNatural (NZN.NZNatural i) = pconstant i

-- | @since 1.0
instance Distributive (Term s PNatural) where
  {-# INLINEABLE fromNZNatural #-}
  fromNZNatural (NZN.NZNatural n) = pconstant . Nat.Natural $ n

-- | @since 1.0
instance Distributive (Term s PNZNatural) where
  {-# INLINEABLE fromNZNatural #-}
  fromNZNatural = pconstant

{- | A generalization of a Euclidean domain, except that it \'extends\' a
 semiring, not a ring.

 = Laws

 @'zero'@ must annihilate multiplication left and right; specifically, @x '*'
 'zero' = 'zero' '*' x = 'zero'@. Additionally, 'fromNatural' must describe the
 unique semiring homomorphism from 'Natural' to the instance, which must be an
 extension of the unique semirig homomorphism described by 'fromNZNatural' for
 nonzero values. Thus, the following must hold:

 * @'fromNatural' 'zero'@ @=@ @'zero'@
 * If @'Just' m = 'removeZero' n@, then @'fromNatural' n = 'fromNZNatural' m@

 Furthermore, 'removeZero' and 'zeroExtend' should form a partial isomorphism
 between a type and this instance (acting as the zero extension of that type).
 This must be consistent with addition and multiplication, as witnessed by
 '+^' and '*^'. Specifically, all the following must hold:

 * @'removeZero' 'zero'@ @=@ @'Nothing'@
 * If @x '/=' 'zero'@, then @'removeZero' x = 'Just' y@
 * @removeZero '.' zeroExtend@ @=@ @'Just'@
 * @x '+^' y@ @=@ @x '+' 'zeroExtend' y@
 * @x '*^' y@ @=@ @x '*' 'zeroExtend' y@

 Lastly, 'quot' and 'rem' must together be a description of Euclidean division
 for the instance:

 * @'quot' 'zero' x@ @=@ @'rem' 'zero' x@ @=@ @'zero'@
 * @'quot' x 'one'@ @=@ @x@
 * @'rem' x 'one'@ @=@ @'zero'@
 * If @'quot' x y = q@ and @'rem' x y = r@, then @(q '*^' y) '+' r = x@.
 * If @'quot' x y = q@, then @'quot' q y = 'zero'@ and @'rem' q y = q@.
 * If @'rem' x y = r@, then @'quot' r y = 'zero'@ and @'rem' r y = r@.

 @since 1.0
-}
class
  (Distributive a, AdditiveMonoid a, MultiplicativeMonoid nz) =>
  Euclidean a nz res
    | nz -> a
    , a -> nz
    , nz -> res
  where
  {-# MINIMAL removeZero, zeroExtend, quot, rem, fromNatural #-}

  -- | @since 1.0
  removeZero :: a -> res

  -- | @since 1.0
  zeroExtend :: nz -> a

  -- | @since 1.0
  {-# INLINEABLE (+^) #-}
  (+^) :: a -> nz -> a
  x +^ y = x + zeroExtend y

  -- | @since 1.0
  {-# INLINEABLE (*^) #-}
  (*^) :: a -> nz -> a
  x *^ y = x * zeroExtend y

  -- | @since 1.0
  quot :: a -> nz -> a

  -- | @since 1.0
  rem :: a -> nz -> a

  -- | @since 1.0
  fromNatural :: Natural -> a

-- | @since 1.0
instance Euclidean Integer NZInteger (Maybe NZInteger) where
  {-# INLINEABLE removeZero #-}
  removeZero i
    | i == 0 = Nothing
    | otherwise = Just . NZI.NZInteger $ i
  {-# INLINEABLE zeroExtend #-}
  zeroExtend (NZI.NZInteger i) = i
  {-# INLINEABLE (+^) #-}
  x +^ (NZI.NZInteger y) = x Prelude.+ y
  {-# INLINEABLE (*^) #-}
  x *^ (NZI.NZInteger y) = x Prelude.* y
  {-# INLINEABLE quot #-}
  quot x (NZI.NZInteger y) = Prelude.quot x y
  {-# INLINEABLE rem #-}
  rem x (NZI.NZInteger y) = Prelude.rem x y
  {-# INLINEABLE fromNatural #-}
  fromNatural (Nat.Natural i) = i

-- | @since 1.0
instance Euclidean Natural NZNatural (Maybe NZNatural) where
  {-# INLINEABLE removeZero #-}
  removeZero (Nat.Natural i)
    | i == 0 = Nothing
    | otherwise = Just . NZN.NZNatural $ i
  {-# INLINEABLE zeroExtend #-}
  zeroExtend (NZN.NZNatural i) = Nat.Natural i
  {-# INLINEABLE (+^) #-}
  Nat.Natural x +^ NZN.NZNatural y = Nat.Natural $ x Prelude.+ y
  {-# INLINEABLE (*^) #-}
  Nat.Natural x *^ NZN.NZNatural y = Nat.Natural $ x Prelude.* y
  {-# INLINEABLE quot #-}
  quot (Nat.Natural x) (NZN.NZNatural y) = Nat.Natural $ Prelude.quot x y
  {-# INLINEABLE rem #-}
  rem (Nat.Natural x) (NZN.NZNatural y) = Nat.Natural $ Prelude.rem x y
  {-# INLINEABLE fromNatural #-}
  fromNatural = id

-- | @since 1.0
instance Euclidean (Term s PInteger) (Term s PNZInteger) (Term s (PMaybe PNZInteger)) where
  {-# INLINEABLE removeZero #-}
  removeZero t = pif (t #== zero) (pcon PNothing) (pcon . PJust . punsafeCoerce $ t)
  {-# INLINEABLE zeroExtend #-}
  zeroExtend = punsafeCoerce
  {-# INLINEABLE (+^) #-}
  x +^ y = punsafeBuiltin PLC.AddInteger # x # y
  {-# INLINEABLE (*^) #-}
  x *^ y = punsafeBuiltin PLC.MultiplyInteger # x # y
  {-# INLINEABLE quot #-}
  quot x y = punsafeBuiltin PLC.QuotientInteger # x # y
  {-# INLINEABLE rem #-}
  rem x y = punsafeBuiltin PLC.RemainderInteger # x # y
  {-# INLINEABLE fromNatural #-}
  fromNatural (Nat.Natural i) = pconstant i

-- | @since 1.0
instance Euclidean (Term s PNatural) (Term s PNZNatural) (Term s (PMaybe PNZInteger)) where
  {-# INLINEABLE removeZero #-}
  removeZero t = pif (t #== zero) (pcon PNothing) (pcon . PJust . punsafeCoerce $ t)
  {-# INLINEABLE zeroExtend #-}
  zeroExtend = punsafeCoerce
  {-# INLINEABLE (+^) #-}
  x +^ y = punsafeBuiltin PLC.AddInteger # x # y
  {-# INLINEABLE (*^) #-}
  x *^ y = punsafeBuiltin PLC.MultiplyInteger # x # y
  {-# INLINEABLE quot #-}
  quot x y = punsafeBuiltin PLC.QuotientInteger # x # y
  {-# INLINEABLE rem #-}
  rem x y = punsafeBuiltin PLC.RemainderInteger # x # y
  {-# INLINEABLE fromNatural #-}
  fromNatural = pconstant

-- | @since 1.0
class
  (AdditiveGroup a, Euclidean a nz f) =>
  Arithmetical a nz f
    | nz -> a
    , a -> nz
    , nz -> f
  where
  {-# MINIMAL div, mod, fromInteger, fromNZInteger #-}

  -- | @since 1.0
  {-# INLINEABLE (-^) #-}
  (-^) :: a -> nz -> a
  x -^ y = x - zeroExtend y

  -- | @since 1.0
  div :: a -> nz -> a

  -- | @since 1.0
  mod :: a -> nz -> a

  -- | @since 1.0
  fromInteger :: Integer -> a

  -- | @since 1.0
  fromNZInteger :: NZInteger -> nz

-- | @since 1.0
class
  (Euclidean a nz f) =>
  Divisible a nz f
    | nz -> a
    , a -> nz
    , nz -> f
  where
  {-# MINIMAL reciprocal #-}

  -- | @since 1.0
  (/) :: a -> nz -> a
  x / y = x *^ reciprocal y

  -- | @since 1.0
  reciprocal :: nz -> nz

  -- | @since 1.0
  powInteger :: nz -> Integer -> nz
  powInteger x i = getMultiplicative . gtimes i . Multiplicative $ x

-- | @since 1.0
instance (Divisible a nz f) => Group (Multiplicative nz) where
  {-# INLINEABLE inverse #-}
  inverse (Multiplicative x) = Multiplicative . reciprocal $ x
  {-# INLINEABLE gtimes #-}
  gtimes i x = case Prelude.signum i of
    0 -> mempty
    1 -> stimesMonoid i x
    _ -> inverse . stimesMonoid (Prelude.negate i) $ x
