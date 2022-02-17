{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module Plutarch.Numeric.Combination (
  Distributive (..),
  Euclidean (..),
  Arithmetical (..),
  Divisible (..),
  (^-^)
  ) where

import Data.Kind (Type)
import Plutarch.Numeric.Additive (AdditiveSemigroup ((+)), AdditiveMonoid (zero),
  AdditiveGroup ((-)), AdditiveCMM ((^-)))
import Plutarch.Numeric.Multiplicative (MultiplicativeSemigroup ((*)), MultiplicativeMonoid)
import Plutarch.Numeric.NZNatural (NZNatural (NZNatural), PNZNatural)
import Plutarch.Numeric.Natural (Natural (Natural), PNatural)
import Plutarch.Numeric.NZInteger (NZInteger (NZInteger), PNZInteger)
import Plutarch (Term, pcon, (#))
import Plutarch.Integer (PInteger)
import Plutarch.Maybe (PMaybe (PNothing, PJust))
import Plutarch.Lift (pconstant)
import Plutarch.Bool (pif, (#==))
import qualified PlutusCore as PLC
import Plutarch.Unsafe (punsafeCoerce, punsafeBuiltin)
import Prelude hiding ((+), (*), (-))
import qualified Prelude

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
  fromNZNatural (NZNatural i) = i

-- | @since 1.0
instance Distributive Natural where
  {-# INLINEABLE fromNZNatural #-}
  fromNZNatural (NZNatural n) = Natural n

-- | @since 1.0
instance Distributive NZNatural where
  {-# INLINEABLE fromNZNatural #-}
  fromNZNatural = id

-- | @since 1.0
instance Distributive (Term s PInteger) where
  {-# INLINEABLE fromNZNatural #-}
  fromNZNatural (NZNatural i) = pconstant i

-- | @since 1.0
instance Distributive (Term s PNatural) where
  {-# INLINEABLE fromNZNatural #-}
  fromNZNatural (NZNatural n) = pconstant . Natural $ n

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
  Euclidean a nz
    | nz -> a
    , a -> nz
  where
  {-# MINIMAL removeZero, zeroExtend, quot, rem, fromNatural #-}

  -- | This is \'morally\' @'Maybe' nz@. We need this rather awkward hack
  -- because of the way 'PMaybe' works in Plutarch.
  --
  -- @since 1.0
  type RemovalResult nz :: Type

  -- | @since 1.0
  removeZero :: a -> RemovalResult nz

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
instance Euclidean Integer NZInteger where
  type RemovalResult NZInteger = Maybe NZInteger
  {-# INLINEABLE removeZero #-}
  removeZero i
    | i == 0 = Nothing
    | otherwise = Just . NZInteger $ i
  {-# INLINEABLE zeroExtend #-}
  zeroExtend (NZInteger i) = i
  {-# INLINEABLE (+^) #-}
  x +^ (NZInteger y) = x Prelude.+ y
  {-# INLINEABLE (*^) #-}
  x *^ (NZInteger y) = x Prelude.* y
  {-# INLINEABLE quot #-}
  quot x (NZInteger y) = Prelude.quot x y
  {-# INLINEABLE rem #-}
  rem x (NZInteger y) = Prelude.rem x y
  {-# INLINEABLE fromNatural #-}
  fromNatural (Natural i) = i

-- | @since 1.0
instance Euclidean Natural NZNatural where
  type RemovalResult NZNatural = Maybe NZNatural
  {-# INLINEABLE removeZero #-}
  removeZero (Natural i)
    | i == 0 = Nothing
    | otherwise = Just . NZNatural $ i
  {-# INLINEABLE zeroExtend #-}
  zeroExtend (NZNatural i) = Natural i
  {-# INLINEABLE (+^) #-}
  Natural x +^ NZNatural y = Natural $ x Prelude.+ y
  {-# INLINEABLE (*^) #-}
  Natural x *^ NZNatural y = Natural $ x Prelude.* y
  {-# INLINEABLE quot #-}
  quot (Natural x) (NZNatural y) = Natural . Prelude.quot x $ y
  {-# INLINEABLE rem #-}
  rem (Natural x) (NZNatural y) = Natural . Prelude.rem x $ y
  {-# INLINEABLE fromNatural #-}
  fromNatural = id

-- | @since 1.0
instance Euclidean (Term s PInteger) (Term s PNZInteger) where
  type RemovalResult (Term s PNZInteger) = Term s (PMaybe PNZInteger)
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
  fromNatural (Natural i) = pconstant i

-- | @since 1.0
instance Euclidean (Term s PNatural) (Term s PNZNatural) where
  type RemovalResult (Term s PNZNatural) = Term s (PMaybe PNZNatural)
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
{-# INLINEABLE (^-^) #-}
(^-^) ::
  forall (a :: Type) (nz :: Type).
  (AdditiveCMM a, Euclidean a nz) =>
  a ->
  nz ->
  a
x ^-^ y = x ^- zeroExtend y

{- | A 'Euclidean' extended with a notion of signedness (and subtraction). This
 is /actually/ a Euclidean domain (and thus, a ring also).

 = Laws

 'div' and 'mod' must be extensions of the description of Euclidean division
 provided by 'quot' and 'rem'. Thus:

 * @'div' ('abs' x) ('abs' y)@ @=@ @'quot' ('abs' x) ('abs' y)@
 * @'mod' ('abs' x) ('abs' y)@ @=@ @'rem' ('abs' x) ('abs' y)@
 * If @'div' x y = q@ and @'mod' x y = r@, then @(q '*^' y) '+' r = x@.

 /TODO:/ Spell out precisely how 'div' and 'mod' differ on negatives.

 Furthermore, @'removeZero'@ and @'zeroExtend'@ must be consistent with
 additive inverses:

 * @x '-^' y@ @=@ @x '-' 'zeroExtend' y@

 Lastly, 'fromInteger' must describe the unique ring homomorphism from
 'Integer' to the instance, which must be an extension of the unique semiring
 homomorphism described by 'fromNatural'. It also must agree with
 'fromNZInteger' on the nonzero part of the instance. Specifically, we must have:

 * If @'Just' m = 'toNatural' n@, then @'fromInteger' n
 = 'fromNatural' m@
 * If @'toNatural' n = 'Nothing@, then @'fromInteger' n
 = 'negate' '.' 'fromInteger' . 'abs' '$' n@.
 * @'fromInteger' '.' 'zeroExtend' '$' x@ @=@ @'zeroExtend' '.' 'fromNZInteger'
  '$' x@
 * If @'removeZero' x = 'Just' y@, then @'removeZero' '.' 'fromInteger' '$' x =
 'Just' . 'fromNZInteger' '$' y@.

 @since 1.0
-}
class
  (AdditiveGroup a, Euclidean a nz) =>
  Arithmetical a nz
    | nz -> a
    , a -> nz
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
instance Arithmetical Integer NZInteger where
  {-# INLINEABLE (-^) #-}
  x -^ NZInteger y = x Prelude.- y
  {-# INLINEABLE div #-}
  div x (NZInteger y) = Prelude.div x y
  {-# INLINEABLE mod #-}
  mod x (NZInteger y) = Prelude.mod x y
  {-# INLINEABLE fromInteger #-}
  fromInteger = id
  {-# INLINEABLE fromNZInteger #-}
  fromNZInteger = id

-- | @since 1.0
instance Arithmetical (Term s PInteger) (Term s PNZInteger) where
  {-# INLINEABLE (-^) #-}
  x -^ y = punsafeBuiltin PLC.SubtractInteger # x # y
  {-# INLINEABLE div #-}
  div x y = punsafeBuiltin PLC.DivideInteger # x # y
  {-# INLINEABLE mod #-}
  mod x y = punsafeBuiltin PLC.ModInteger # x # y
  {-# INLINEABLE fromInteger #-}
  fromInteger = pconstant
  {-# INLINEABLE fromNZInteger #-}
  fromNZInteger = pconstant

{- | A 'Euclidean' extended with a notion of proper division. Basically a field,
 but without requiring additive inverses.

 = Laws

 @'reciprocal'@ has to act as a multiplicative inverse on the nonzero part of
 the instance, with division defined as multiplication by the reciprocal.
 Thus, we have:

 * @'reciprocal' '.' 'reciprocal'@ @=@ @'id'@
 * @'reciprocal' x '*' x@ @=@ @x '*' 'reciprocal' x@ @=@ @'one'@
 * @x '/' y@ @=@ @x '*' 'reciprocal' y@

 Furthermore, any instance must form a left-'Integer' semimodule over its
 nonzero part (witnessed by 'powInteger'), which must be an extension of the
 left-'Natural' semimodule witnessed by 'powNatural'. Thus, we must have:

 * If @'Just' m = 'toNatural' n@, then @'powInteger' x
 n = 'powNatural' x m@
 * If @'toNatural' n = 'Nothing'@., then @'powInteger'
 x n = 'reciprocal' '.' 'powInteger' x . 'abs' '$' n@.

 @since 1.0
-}
class
  (Euclidean a nz) =>
  Divisible a nz
    | nz -> a
    , a -> nz
  where
  {-# MINIMAL reciprocal #-}

  -- | @since 1.0
  (/) :: a -> nz -> a
  x / y = x *^ reciprocal y

  -- | @since 1.0
  reciprocal :: nz -> nz

{-
  -- | @since 1.0
  powInteger :: nz -> Integer -> nz
  powInteger x i = getMultiplicative . gtimes i . Multiplicative $ x
-}
