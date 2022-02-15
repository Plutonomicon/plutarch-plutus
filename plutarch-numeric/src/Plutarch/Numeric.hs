{-# LANGUAGE FunctionalDependencies #-}

module Plutarch.Numeric (
  -- * Types

  -- ** Plutarch
  PNatural,
  PNZNatural,
  PNZInteger,

  -- ** Haskell
  Natural,
  NZNatural,
  NZInteger,

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
  Integral (..),
  Euclidean (..),
  Arithmetical (..),
  Divisible (..),

  -- * Functions

  -- ** Reductions
  sum1,
  product1,
  sum,
  product,
  sumNZ,
  productNZ,

  -- ** Other
  mod,
) where

import Data.Foldable (foldl')
import Data.Kind (Type)
import Data.Semigroup (stimes, stimesMonoid)
import Data.Semigroup.Foldable (Foldable1 (foldMap1))
import Plutarch (Term)
import Plutarch.Integer (PInteger)
import Plutarch.Lift (pconstant)
import Plutarch.Numeric.Group (Group (gtimes, inverse))
import Plutarch.Numeric.NZInteger (NZInteger, PNZInteger)
import Plutarch.Numeric.NZInteger qualified as NZI
import Plutarch.Numeric.NZNatural (NZNatural, PNZNatural)
import Plutarch.Numeric.NZNatural qualified as NZN
import Plutarch.Numeric.Natural (Natural, PNatural)
import Plutarch.Numeric.Natural qualified as Nat
import Prelude hiding (
  Integral,
  divMod,
  mod,
  negate,
  product,
  signum,
  sum,
  (*),
  (+),
 )
import Prelude qualified

{- | A commutative semigroup, meant to be morally equivalent to numerical
 addition.

 = Laws

 Formally, an instance of 'AdditiveSemigroup' must be a commutative semigroup
 with '+' as its operation. Furthermore, 'Additive' 'NZNatural' must be a
 right semigroup action, and [TODO], both witnessed by 'scaleNZNatural'.

 This requires that '+' commutes and associates:

 * @x '+' y = y '+' x@
 * @(x '+' y) '+' z = x '+' (y '+' z)@

 Furthermore, 'scaleNZNatural' must follow these laws:

 * @'scaleNZNatural' x 'one' = x@
 * @'scaleNZNatural' x (n '+' m) = 'scaleNZNatural' x n '+' 'scaleNZNatural' x m@
 * @'scaleNZNatural' x (n '*' m) = 'scaleNZNatural' ('scaleNZNatural' x n) m@

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
deriving via Integer instance AdditiveSemigroup Natural

-- | @since 1.0
deriving via Integer instance AdditiveSemigroup NZNatural

-- | @since 1.0
instance AdditiveSemigroup (Term s PInteger) where
  {-# INLINEABLE (+) #-}
  (+) = (Prelude.+)
  {-# INLINEABLE scaleNZNatural #-}
  scaleNZNatural i (NZN.NZNatural n) = i Prelude.* pconstant n

{-
-- | @since 1.0
instance AdditiveSemigroup (Term s PNatural) where
  {-# INLINEABLE (+) #-}
  (+) = _
  {-# INLINEABLE scaleNZNatural #-}
  scaleNZNatural = _

-- | @since 1.0
instance AdditiveSemigroup (Term s PNZNatural) where
  {-# INLINEABLE (+) #-}
  (+) = _
  {-# INLINEABLE scaleNZNatural #-}
  scaleNZNatural = _
-}

{- | An 'AdditiveSemigroup' extended with a notion of zero.

 = Laws

 Formally, an instance of 'AdditiveMonoid' must be a commutative monoid with
 'zero' as its identity. Furthermore, it must form a left-'Natural'
 semimodule, witnessed by 'scaleNatural', which must be an extension of the
 right semigroup action described by 'scaleNZNatural' for nonzero actions.

 This requires that @'zero' '+' x = x '+' 'zero' = x@. Furthermore,
 'scaleNatural' must follow these laws:

 * If @'Just' m = 'removeZero' n@, then @'scaleNatural' x n = 'scaleNZNatural' x m@.
 * @'scaleNatural x 'zero' = 'zero'@

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
instance AdditiveMonoid (Term s PInteger) where
  {-# INLINEABLE zero #-}
  zero = pconstant 0
  {-# INLINEABLE scaleNatural #-}
  scaleNatural i (Nat.Natural n) = i Prelude.* pconstant n

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

 * @x '+' (y '^-' x) = y '+' (x '^-' y)@
 * @(x '^-' y) '^-' z = x '^-' (y '+' z)@
 * @x '^-' x = 'zero'@
 * @'zero' '^-' x = 'zero'@

 @since 1.0
-}
class (AdditiveMonoid a) => AdditiveCMM a where
  -- | @since 1.0
  (^-) :: a -> a -> a

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

{- | \'Add up\' a non-empty collection of values.

 @since 1.0
-}
sum1 ::
  forall (a :: Type) (f :: Type -> Type).
  (Foldable1 f, AdditiveSemigroup a) =>
  f a ->
  a
sum1 = getAdditive . foldMap1 Additive

{- | As 'sum1', but allows empty collections.

 @since 1.0
-}
sum ::
  forall (a :: Type) (f :: Type -> Type).
  (Foldable f, AdditiveMonoid a) =>
  f a ->
  a
sum = getAdditive . foldMap Additive

{- | A semigroup, meant to be morally equivalent to numerical multiplication.

 = Laws

 Formally, an instance of 'MultiplicativeSemigroup' must be a semigroup with
 '*' as its operation. Furthermore, 'Additive' 'NZNatural' must be a right
 semigroup action, and [TODO], both witnessed by 'powNZNatural'.

 This requires that @(x '*' y) '*' z = x '*' (y '*' z)@. Furthermore,
 'powNZNatural' must follow these laws:

 * @'powNZNatural' x 'one' = x@
 * @'powNZNatural' x (n '+' m) = 'powNZNatural' x n '*' 'powNZNatural' x m@
 * @'powNZNatural' x (n '*' m) = 'powNZNatural' ('powNZNatural' x n) m@

 @since 1.0
-}
class (Eq a) => MultiplicativeSemigroup a where
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

-- | @since 1.0
deriving via Integer instance (MultiplicativeSemigroup NZInteger)

{- | A 'MultiplicativeSemigroup' extended with a notion of unit.

 = Laws

 Formally, an instance of 'AdditiveMonoid' must be a monoid with 'one' as its
 identity. Furthermore, it must form a left-'Natural' semimodule, withnessed
 by 'powNatural', which must be an extension of the right semigroup action
 described by 'powNZNatural' for nonzero actions.

 This requires that @'one' '*' x = x '*' 'one' = x@. Furthermore, 'powNatural'
 must follow these laws:

 * If @'Just' m = 'removeZero' n@, then @'powNatural' x n = 'powNatural' x m@.
 * @'powNatural x 'zero' = 'one'@.

 Lastly, 'abs' and 'signum' must follow these laws:

 * @'abs' 'one' = 'signum' 'one' = 'one'@
 * @'abs' x '*' 'signum' 'x' = x@
 * @'abs' (x '*' y) = 'abs' x '*' 'abs' y@

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

-- | @since 1.0
deriving via Integer instance MultiplicativeMonoid NZInteger

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

{- | Multiply together a non-empty collection of values.

 @since 1.0
-}
product1 ::
  forall (a :: Type) (f :: Type -> Type).
  (Foldable1 f, MultiplicativeSemigroup a) =>
  f a ->
  a
product1 = getMultiplicative . foldMap1 Multiplicative

{- | As 'product1', but allows empty collections.

 @since 1.0
-}
product ::
  forall (a :: Type) (f :: Type -> Type).
  (Foldable f, MultiplicativeMonoid a) =>
  f a ->
  a
product = getMultiplicative . foldMap Multiplicative

{- | A semirig (semiring without a neutral additive element) formed from an
 'AdditiveSemigroup' and a 'MultiplicativeMonoid'.

 = Laws

 Multiplication left and right must distribute over addition:

 * @x '*' (y '+' z) = (x '*' y) '+' (x '*' z)@
 * @(x '+' y) '*' z = (x '*' z) '+' (y '*' z)@

 Furthermore, 'fromNZNatural' must be the unique semirig homomorphism from
 'NZNatural' to the instance. Thus:

 * @'fromNZNatural' 'one' = 'one'@; and
 * @'fromNZNatural' (x '+' y) = 'fromNZNatural' x '+' 'fromNZNatural' y@

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
class
  (Distributive a, AdditiveMonoid a, MultiplicativeMonoid nz) =>
  Integral a nz
    | nz -> a
    , a -> nz
  where
  {-# MINIMAL removeZero, zeroExtend, div, fromNatural #-}

  -- | @since 1.0
  removeZero :: a -> Maybe nz

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
  div :: a -> nz -> a

  -- | @since 1.0
  fromNatural :: Natural -> a

-- | @since 1.0
instance Integral Integer NZInteger where
  {-# INLINEABLE removeZero #-}
  removeZero i
    | i == 0 = Nothing
    | otherwise = Just . NZI.NZInteger $ i
  {-# INLINEABLE zeroExtend #-}
  zeroExtend (NZI.NZInteger i) = i
  {-# INLINEABLE (+^) #-}
  x +^ NZI.NZInteger y = x Prelude.+ y
  {-# INLINEABLE (*^) #-}
  x *^ NZI.NZInteger y = x Prelude.* y
  {-# INLINEABLE div #-}
  x `div` NZI.NZInteger y = x `Prelude.quot` y -- TODO: Needs thought due to positivity
  {-# INLINEABLE fromNatural #-}
  fromNatural (Nat.Natural i) = i

{- | A version of 'sum' for zerofree values.

 @since 1.0
-}
sumNZ ::
  forall (a :: Type) (f :: Type -> Type) (nz :: Type).
  (Foldable f, Integral a nz) =>
  f nz ->
  a
sumNZ = foldl' (+^) zero

{- | A version of 'product' for zerofree values.

 @since 1.0
-}
productNZ ::
  forall (a :: Type) (f :: Type -> Type) (nz :: Type).
  (Foldable f, Integral a nz) =>
  f nz ->
  a
productNZ = foldl' (*^) one

-- | @since 1.0
class (Integral a nz) => Euclidean a nz | nz -> a, a -> nz where
  -- | @since 1.0
  divMod :: a -> nz -> (a, a)

-- | @since 1.0
instance Euclidean Integer NZInteger where
  {-# INLINEABLE divMod #-}
  divMod x (NZI.NZInteger y) = Prelude.quotRem x y -- TODO: Needs thought due to positivity

{- | Return only the remainder from a 'divMod'.

 @since 1.0
-}
mod ::
  forall (a :: Type) (nz :: Type).
  (Euclidean a nz) =>
  a ->
  nz ->
  a
mod x = snd . divMod x

-- | @since 1.0
class (AdditiveGroup a, Integral a nz) => Arithmetical a nz | nz -> a, a -> nz where
  -- | @since 1.0
  fromInteger :: Integer -> a

  -- | @since 1.0
  fromNZInteger :: NZInteger -> nz

-- | @since 1.0
instance Arithmetical Integer NZInteger where
  {-# INLINEABLE fromInteger #-}
  fromInteger = id
  {-# INLINEABLE fromNZInteger #-}
  fromNZInteger = id

-- | @since 1.0
class (Integral a nz) => Divisible a nz | nz -> a, a -> nz where
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
instance (Divisible a nz) => Group (Multiplicative nz) where
  {-# INLINEABLE inverse #-}
  inverse (Multiplicative x) = Multiplicative . reciprocal $ x
  {-# INLINEABLE gtimes #-}
  gtimes i x = case Prelude.signum i of
    0 -> mempty
    1 -> stimesMonoid i x
    _ -> inverse . stimesMonoid (Prelude.negate i) $ x
