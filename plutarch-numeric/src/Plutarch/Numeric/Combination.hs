{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}

module Plutarch.Numeric.Combination (
  Distributive (..),
  RemoveZero (..),
  PRemoveZero (..),
  Euclidean (..),
  Arithmetical (..),
  Divisible (..),
) where

import Plutarch (Term, pcon, plet, pmatch, (#))
import Plutarch.Bool (pif, (#<=), (#==))
import Plutarch.Integer (PInteger)
import Plutarch.Lift (pconstant)
import Plutarch.Maybe (PMaybe (PJust, PNothing))
import Plutarch.Numeric.Additive (
  AdditiveGroup (negate, (-)),
  AdditiveMonoid (zero),
  AdditiveSemigroup ((+)),
 )
import Plutarch.Numeric.Fractional (
  Fractionable (scale),
  PFractionable (pscale),
 )
import Plutarch.Numeric.Multiplicative (
  MultiplicativeMonoid (one),
  (*),
 )
import Plutarch.Numeric.NZInteger (NZInteger (NZInteger), PNZInteger)
import Plutarch.Numeric.NZNatural (NZNatural (NZNatural), PNZNatural)
import Plutarch.Numeric.Natural (Natural (Natural), PNatural)
import Plutarch.Numeric.Ratio (
  PRatio,
  Ratio (Ratio),
  pconRatio,
  pmatchRatio,
  ratio,
 )
import Plutarch.Pair (PPair (PPair))
import Plutarch.Unsafe (punsafeBuiltin, punsafeCoerce)
import PlutusCore qualified as PLC
import Prelude hiding (abs, div, mod, negate, quot, rem, (*), (+), (-), (/))
import Prelude qualified

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
instance (Fractionable a, Distributive a) => Distributive (Ratio a) where
  {-# INLINEABLE fromNZNatural #-}
  fromNZNatural n = Ratio (fromNZNatural n, one)

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

-- | @since 1.0
instance
  (PFractionable a, Distributive (Term s a)) =>
  Distributive (Term s (PRatio a))
  where
  {-# INLINEABLE fromNZNatural #-}
  fromNZNatural n =
    punsafeCoerce . pcon $
      PPair
        (fromNZNatural n :: Term s a)
        (one :: Term s PNZNatural)

{- | Describes a bijective relation between a numeric type and an equivalent
 type which is zerofree (that is, has no counterpart to 'zero').

 = Laws

 'removeZero' and 'zeroExtend' should form a partial isomorphism, such that
 'zero' is not an inverse of any element. More precisely, the following must
 hold:

 * @'removeZero' '.' 'zeroExtend'@ @=@ @'id'@
 * @'removeZero' 'zero'@ @=@ @'Nothing'@

 @since 1.0
-}
class (AdditiveMonoid a) => RemoveZero a nz | a -> nz, nz -> a where
  {-# MINIMAL removeZero, zeroExtend #-}

  -- | @since 1.0
  removeZero :: a -> Maybe nz

  -- | @since 1.0
  zeroExtend :: nz -> a

-- | @since 1.0
instance RemoveZero Integer NZInteger where
  {-# INLINEABLE removeZero #-}
  removeZero i
    | i == zero = Nothing
    | otherwise = pure . NZInteger $ i
  {-# INLINEABLE zeroExtend #-}
  zeroExtend (NZInteger i) = i

-- | @since 1.0
instance RemoveZero Natural NZNatural where
  {-# INLINEABLE removeZero #-}
  removeZero (Natural i)
    | i == zero = Nothing
    | otherwise = pure . NZNatural $ i
  {-# INLINEABLE zeroExtend #-}
  zeroExtend (NZNatural i) = Natural i

-- | @since 1.0
instance
  (Fractionable a, RemoveZero a nz) =>
  RemoveZero (Ratio a) (Ratio nz)
  where
  {-# INLINEABLE removeZero #-}
  removeZero (Ratio (num, den)) = do
    num' <- removeZero num
    pure . Ratio $ (num', den)
  {-# INLINEABLE zeroExtend #-}
  zeroExtend (Ratio (num, den)) = Ratio (zeroExtend num, den)

{- | This is \'morally equivalent\' to 'RemoveZero', except that it's designed
 for convenient use with Plutarch 'Term's.

 = Laws

 The laws are \'morally the same\' as those for 'RemoveZero', but we restate
 them here for clarity.

 * @'premoveZero' '.' 'pzeroExtend'@ @=@ @'id'@
 * @'premoveZero' 'zero'@ @=@ @'pcon' 'PNothing'@

 @since 1.0
-}
class
  (forall s. AdditiveMonoid (Term s a)) =>
  PRemoveZero a nz
    | a -> nz
    , nz -> a
  where
  -- | @since 1.0
  premoveZero :: Term s a -> Term s (PMaybe nz)

  -- | @since 1.0
  pzeroExtend :: Term s nz -> Term s a

-- | @since 1.0
instance PRemoveZero PInteger PNZInteger where
  {-# INLINEABLE premoveZero #-}
  premoveZero t =
    pif
      (t #== zero)
      (pcon PNothing)
      (pcon . PJust . punsafeCoerce $ t)
  {-# INLINEABLE pzeroExtend #-}
  pzeroExtend = punsafeCoerce

-- | @since 1.0
instance PRemoveZero PNatural PNZNatural where
  {-# INLINEABLE premoveZero #-}
  premoveZero t =
    pif
      (t #== zero)
      (pcon PNothing)
      (pcon . PJust . punsafeCoerce $ t)
  {-# INLINEABLE pzeroExtend #-}
  pzeroExtend = punsafeCoerce

-- | @since 1.0
instance
  (PFractionable a, PRemoveZero a nz) =>
  PRemoveZero (PRatio a) (PRatio nz)
  where
  {-# INLINEABLE premoveZero #-}
  premoveZero t = pmatchRatio t $ \num den ->
    plet (premoveZero num) $ \numMay ->
      pmatch numMay $ \case
        PNothing -> pcon PNothing
        PJust t' -> pcon . PJust . punsafeCoerce . pcon $ PPair t' den
  {-# INLINEABLE pzeroExtend #-}
  pzeroExtend = punsafeCoerce

{- | A generalization of a Euclidean domain, except that it \'extends\' a
 semiring, not a ring.

 = Note

 Morally speaking, 'RemoveZero' or 'PRemoveZero' should be superclass
 constraints for 'Euclidean'. However, as these would be different for
 different types, we cannot actually choose one or the other, as this
 would then make half of our desired instances unwritable.

 Therefore, any instance of this type class should /also/ be an instance of
 either 'RemoveZero' or 'PRemoveZero'. The laws are stated in terms of
 'RemoveZero' being that choice, but the 'PRemoveZero' equivalents should
 follow similarly.

 = Laws

 @'zero'@ must annihilate multiplication left and right; specifically, @x '*'
 'zero' = 'zero' '*' x = 'zero'@. Additionally, 'fromNatural' must describe the
 unique semiring homomorphism from 'Natural' to the instance, which must be an
 extension of the unique semirig homomorphism described by 'fromNZNatural' for
 zerofree values. Thus, the following must hold:

 * @'fromNatural' 'zero'@ @=@ @'zero'@
 * If @'Just' m = 'removeZero' n@, then @'fromNatural' n = 'fromNZNatural' m@

 Furthermore, '+^' and '*^' must be consistent with '+' and '*', but for
 zerofree inputs. Specifically:

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
  -- | @since 1.0
  (+^) :: a -> nz -> a

  -- | @since 1.0
  (*^) :: a -> nz -> a

  -- | @since 1.0
  quot :: a -> nz -> a

  -- | @since 1.0
  rem :: a -> nz -> a

  -- | @since 1.0
  fromNatural :: Natural -> a

infixl 6 +^
infixl 7 *^
infixl 7 `quot`
infixl 7 `rem`

-- | @since 1.0
instance Euclidean Integer NZInteger where
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
instance Euclidean (Ratio Integer) (Ratio NZInteger) where
  {-# INLINEABLE (+^) #-}
  r +^ Ratio (NZInteger num, den) = r + Ratio (num, den)
  {-# INLINEABLE (*^) #-}
  r *^ Ratio (NZInteger num, den) = r * Ratio (num, den)

  -- This is equivalent to '/'.
  {-# INLINEABLE quot #-}
  quot (Ratio (num, NZNatural den)) (Ratio (NZInteger num', den')) =
    let newNum = scale num den'
        newDen = num' * den
     in case Prelude.signum newDen of
          (-1) -> ratio (Prelude.negate newNum) (NZNatural . Prelude.abs $ newDen)
          _ -> ratio newNum (NZNatural newDen)

  -- Always 'zero'.
  {-# INLINEABLE rem #-}
  rem _ _ = zero
  {-# INLINEABLE fromNatural #-}
  fromNatural (Natural i) = Ratio (i, one)

-- | @since 1.0
instance Euclidean (Ratio Natural) (Ratio NZNatural) where
  {-# INLINEABLE (+^) #-}
  r +^ Ratio (NZNatural num, den) = r + Ratio (Natural num, den)
  {-# INLINEABLE (*^) #-}
  r *^ Ratio (NZNatural num, den) = r * Ratio (Natural num, den)

  -- This is equivalent to '/'.
  {-# INLINEABLE quot #-}
  quot (Ratio (num, den)) (Ratio (num', den')) =
    let newNum = scale num den'
        newDen = scale num' den
     in ratio newNum newDen

  -- Always 'zero'.
  {-# INLINEABLE rem #-}
  rem _ _ = zero
  {-# INLINEABLE fromNatural #-}
  fromNatural n = Ratio (n, one)

-- | @since 1.0
instance Euclidean (Term s PInteger) (Term s PNZInteger) where
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
instance Euclidean (Term s (PRatio PInteger)) (Term s (PRatio PNZInteger)) where
  {-# INLINEABLE (+^) #-}
  t +^ t' = pmatchRatio t $ \num den ->
    pmatchRatio t' $ \num' den' ->
      plet (den * den') $ \newDen ->
        plet (pscale num den' + punsafeCoerce (pscale num' den)) $ \newNum ->
          pconRatio newNum newDen
  {-# INLINEABLE (*^) #-}
  t *^ t' = pmatchRatio t $ \num den ->
    pmatchRatio t' $ \num' den' ->
      plet (den * den') $ \newDen ->
        plet (num * punsafeCoerce num') $ \newNum ->
          pconRatio newNum newDen

  -- Equivalent to '/'.
  {-# INLINEABLE quot #-}
  quot t t' = pmatchRatio t $ \num den ->
    pmatchRatio t' $ \num' den' ->
      plet (pscale num den') $ \newNum ->
        plet (num' * punsafeCoerce den) $ \newDen ->
          pif
            (one #<= newDen)
            (pconRatio newNum (punsafeCoerce newDen))
            ( pconRatio
                (negate newNum)
                ( punsafeCoerce
                    . negate @(Term s PInteger)
                    . punsafeCoerce
                    $ newDen
                )
            )

  -- Always 'zero'.
  {-# INLINEABLE rem #-}
  rem _ _ = zero
  {-# INLINEABLE fromNatural #-}
  fromNatural (Natural i) = pconRatio (pconstant i) one

-- | @since 1.0
instance Euclidean (Term s (PRatio PNatural)) (Term s (PRatio PNZNatural)) where
  {-# INLINEABLE (+^) #-}
  t +^ t' = pmatchRatio t $ \num den ->
    pmatchRatio t' $ \num' den' ->
      plet (den * den') $ \newDen ->
        plet (pscale num den' + pscale (punsafeCoerce num') den) $ \newNum ->
          pconRatio newNum newDen
  {-# INLINEABLE (*^) #-}
  t *^ t' = pmatchRatio t $ \num den ->
    pmatchRatio t' $ \num' den' ->
      plet (den * den') $ \newDen ->
        plet (num * punsafeCoerce num') $ \newNum ->
          pconRatio newNum newDen

  -- Equivalent to '/'.
  {-# INLINEABLE quot #-}
  quot t t' = pmatchRatio t $ \num den ->
    pmatchRatio t' $ \num' den' ->
      plet (pscale num den') $ \newNum ->
        plet (num' * den) $ \newDen ->
          pconRatio newNum newDen

  -- Always 'zero'.
  {-# INLINEABLE rem #-}
  rem _ _ = zero
  {-# INLINEABLE fromNatural #-}
  fromNatural n = pconRatio (pconstant n) one

{- | A 'Euclidean' extended with a notion of signedness (and subtraction). This
 is /actually/ a Euclidean domain (and thus, a ring also).

 = Note

 The caveat regarding 'RemoveZero' and 'PRemoveZero' as \'moral superclasses\' of
 'Euclidean' also applies here.

 = Laws

 'div' and 'mod' must be extensions of the description of Euclidean division
 provided by 'quot' and 'rem'. Thus:

 * @'div' ('Plutarch.Numeric.Multiplicative.abs' x)
 ('Plutarch.Numeric.Multiplicative.abs' y)@ @=@ @'quot'
 ('Plutarch.Numeric.Multiplicative.abs' x) ('Plutarch.Numeric.Multiplicative.abs'
 y)@
 * @'mod' ('Plutarch.Numeric.Multiplicative.abs' x)
 ('Plutarch.Numeric.Multiplicative.abs' y)@ @=@ @'rem'
 ('Plutarch.Numeric.Multiplicative.abs' x)
 ('Plutarch.Numeric.Multiplicative.abs' y)@
 * If @'div' x y = q@ and @'mod' x y = r@, then @(q '*^' y) '+' r = x@.

 /TODO:/ Spell out precisely how 'div' and 'mod' differ on negatives.

 Furthermore, '-^' must be consistent with '-', but for zerofree inputs.
 Specifically:

 * @x '-^' y@ @=@ @x '-' 'zeroExtend' y@

 Lastly, 'Plutarch.Numeric.Combination.fromInteger' must describe the unique
 ring homomorphism from 'Integer' to the instance, which must be an extension
 of the unique semiring homomorphism described by 'fromNatural'. It also must
 agree with 'fromNZInteger' on the zerofree part of the instance. Specifically,
 we must have:

 * If @'Just' m = 'Plutarch.Numeric.Natural.toNatural' n@, then
 @'Plutarch.Numeric.Combination.fromInteger' n = 'fromNatural' m@
 * If @'Plutarch.Numeric.Natural.toNatural' n = 'Nothing@, then
 @'Plutarch.Numeric.Combination.fromInteger' n = 'negate' '.'
 'Plutarch.Numeric.Combination.fromInteger' .
 'Plutarch.Numeric.Multiplicative.abs' '$' n@.
 * @'Plutarch.Numeric.Combination.fromInteger' '.' 'zeroExtend' '$' x@ @=@
 @'zeroExtend' '.' 'fromNZInteger'
  '$' x@
 * If @'removeZero' x = 'Just' y@, then @'removeZero' '.'
 'Plutarch.Numeric.Combination.fromInteger' '$' x = 'Just' . 'fromNZInteger'
 '$' y@.

 @since 1.0
-}
class
  (AdditiveGroup a, Euclidean a nz) =>
  Arithmetical a nz
    | nz -> a
    , a -> nz
  where
  -- | @since 1.0
  (-^) :: a -> nz -> a

  -- | @since 1.0
  div :: a -> nz -> a

  -- | @since 1.0
  mod :: a -> nz -> a

  -- | @since 1.0
  fromInteger :: Integer -> a

  -- | @since 1.0
  fromNZInteger :: NZInteger -> nz

infixl 6 -^
infixl 7 `div`
infixl 7 `mod`

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
instance Arithmetical (Ratio Integer) (Ratio NZInteger) where
  {-# INLINEABLE (-^) #-}
  r -^ Ratio (NZInteger num, den) = r - Ratio (num, den)

  -- Equivalent to '/'.
  {-# INLINEABLE div #-}
  div = quot

  -- Always 'zero'.
  {-# INLINEABLE mod #-}
  mod = rem
  {-# INLINEABLE fromInteger #-}
  fromInteger i = Ratio (i, one)
  {-# INLINEABLE fromNZInteger #-}
  fromNZInteger i = Ratio (i, one)

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

-- | @since 1.0
instance Arithmetical (Term s (PRatio PInteger)) (Term s (PRatio PNZInteger)) where
  {-# INLINEABLE (-^) #-}
  t -^ t' = pmatchRatio t $ \num den ->
    pmatchRatio t' $ \num' den' ->
      plet (den * den') $ \newDen ->
        plet (pscale num den' - punsafeCoerce (pscale num' den)) $ \newNum ->
          pconRatio newNum newDen

  -- Equivalent to '/'.
  {-# INLINEABLE div #-}
  div = quot

  -- Equivalent to 'zero'.
  {-# INLINEABLE mod #-}
  mod = rem
  {-# INLINEABLE fromInteger #-}
  fromInteger i = pconRatio (pconstant i) one
  {-# INLINEABLE fromNZInteger #-}
  fromNZInteger i = pconRatio (pconstant i) one

{- | A 'Euclidean' extended with a notion of proper division. Basically a field,
 but without requiring additive inverses.

 = Note

 The caveat regarding 'RemoveZero' and 'PRemoveZero' as \'moral superclasses\' of
 'Euclidean' also applies here.

 = Laws

 @'reciprocal'@ has to act as a multiplicative inverse on the zerofree part of
 the instance, with division defined as multiplication by the reciprocal.
 Thus, we have:

 * @'reciprocal' '.' 'reciprocal'@ @=@ @'id'@
 * @'reciprocal' x '*' x@ @=@ @x '*' 'reciprocal' x@ @=@ @'one'@
 * @x '/' y@ @=@ @x '*' 'reciprocal' y@

 @since 1.0
-}
class
  (Euclidean a nz) =>
  Divisible a nz
    | nz -> a
    , a -> nz
  where
  -- | @since 1.0
  (/) :: a -> nz -> a

  -- | @since 1.0
  reciprocal :: nz -> nz

infixl 7 /

-- | @since 1.0
instance Divisible (Ratio Integer) (Ratio NZInteger) where
  {-# INLINEABLE (/) #-}
  (/) = quot
  {-# INLINEABLE reciprocal #-}
  reciprocal (Ratio (NZInteger num, NZNatural den)) =
    Ratio $ case Prelude.signum num of
      (-1) -> (NZInteger . Prelude.negate $ den, NZNatural . Prelude.negate $ num)
      _ -> (NZInteger den, NZNatural num)

-- | @since 1.0
instance Divisible (Ratio Natural) (Ratio NZNatural) where
  {-# INLINEABLE (/) #-}
  (/) = quot
  {-# INLINEABLE reciprocal #-}
  reciprocal (Ratio (num, den)) = Ratio (den, num)

-- | @since 1.0
instance Divisible (Term s (PRatio PInteger)) (Term s (PRatio PNZInteger)) where
  {-# INLINEABLE (/) #-}
  (/) = quot
  {-# INLINEABLE reciprocal #-}
  reciprocal t = pmatchRatio t $ \num den ->
    pif
      (one #<= num)
      (pconRatio (punsafeCoerce den) (punsafeCoerce num))
      ( pconRatio
          (punsafeCoerce . negate @(Term s PInteger) . punsafeCoerce $ den)
          (punsafeCoerce . negate @(Term s PInteger) . punsafeCoerce $ num)
      )

-- | @since 1.0
instance Divisible (Term s (PRatio PNatural)) (Term s (PRatio PNZNatural)) where
  {-# INLINEABLE (/) #-}
  (/) = quot
  {-# INLINEABLE reciprocal #-}
  reciprocal t = pmatchRatio t $ \num den ->
    pconRatio (punsafeCoerce den) (punsafeCoerce num)
