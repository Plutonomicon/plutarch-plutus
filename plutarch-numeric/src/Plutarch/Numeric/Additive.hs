module Plutarch.Numeric.Additive (
  AdditiveSemigroup (..),
  AdditiveMonoid (..),
  AdditiveGroup (..),
  AdditiveCMM (..),
) where

import Data.Kind (Type)
import Plutarch (S, Term, pcon, plet, (#))
import Plutarch.Bool (pif, (#<=))
import Plutarch.Integer (PInteger)
import Plutarch.Lift (pconstant)
import Plutarch.Numeric.Fractional (
  Fractionable (scale),
  PFractionable (pscale),
 )
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
import Prelude hiding (negate, (+), (-))
import Prelude qualified

{- | A commutative semigroup, meant to be morally equivalent to numerical
 addition.

 = Laws

 Formally, an instance of 'AdditiveSemigroup' must be a commutative semigroup
 with '+' as its operation. This requires that '+' commutes and associates:

 * @x '+' y@ @=@ @y '+' x@
 * @(x '+' y) '+' z@ @=@ @x '+' (y '+' z)@

 @since 1.0
-}
class AdditiveSemigroup a where
  {-# MINIMAL (+) #-}

  -- | @since 1.0
  (+) :: a -> a -> a

-- | @since 1.0
instance AdditiveSemigroup Integer where
  {-# INLINEABLE (+) #-}
  (+) = (Prelude.+)

-- | @since 1.0
deriving via Integer instance (AdditiveSemigroup Natural)

-- | @since 1.0
deriving via Integer instance (AdditiveSemigroup NZNatural)

-- | @since 1.0
instance
  (Fractionable a, AdditiveSemigroup a) =>
  AdditiveSemigroup (Ratio a)
  where
  {-# INLINEABLE (+) #-}
  (+) = viaRatio (+)

-- | @since 1.0
instance AdditiveSemigroup (Term s PInteger) where
  {-# INLINEABLE (+) #-}
  x + y = punsafeBuiltin PLC.AddInteger # x # y

-- | @since 1.0
instance AdditiveSemigroup (Term s PNatural) where
  {-# INLINEABLE (+) #-}
  x + y = punsafeBuiltin PLC.AddInteger # x # y

-- | @since 1.0
instance
  (PFractionable a, AdditiveSemigroup (Term s a)) =>
  AdditiveSemigroup (Term s (PRatio a))
  where
  {-# INLINEABLE (+) #-}
  (+) = viaPRatio (+)

-- | @since 1.0
instance AdditiveSemigroup (Term s PNZNatural) where
  {-# INLINEABLE (+) #-}
  x + y = punsafeBuiltin PLC.AddInteger # x # y

{- | An 'AdditiveSemigroup' extended with a notion of zero.

 = Laws

 Formally, an instance of 'AdditiveMonoid' must be a commutative monoid with
 'zero' as its identity. This requires that @'zero' '+' x = x '+' 'zero' = x@.

 @since 1.0
-}
class (AdditiveSemigroup a) => AdditiveMonoid a where
  {-# MINIMAL zero #-}

  -- | @since 1.0
  zero :: a

-- | @since 1.0
instance AdditiveMonoid Integer where
  {-# INLINEABLE zero #-}
  zero = 0

-- | @since 1.0
deriving via Integer instance (AdditiveMonoid Natural)

-- | @since 1.0
instance
  (Fractionable a, AdditiveMonoid a) =>
  AdditiveMonoid (Ratio a)
  where
  {-# INLINEABLE zero #-}
  zero = Ratio (zero, NZNatural 1)

-- | @since 1.0
instance AdditiveMonoid (Term s PInteger) where
  {-# INLINEABLE zero #-}
  zero = pconstant 0

-- | @since 1.0
instance AdditiveMonoid (Term s PNatural) where
  {-# INLINEABLE zero #-}
  zero = pconstant . Natural $ 0

-- | @since 1.0
instance
  (PFractionable a, AdditiveMonoid (Term s a)) =>
  AdditiveMonoid (Term s (PRatio a))
  where
  {-# INLINEABLE zero #-}
  zero =
    punsafeCoerce . pcon $
      PPair
        (zero :: Term s a)
        (punsafeCoerce (1 :: Term s PInteger))

{- | An 'AdditiveMonoid' extended with a notion of negation.

 = Laws

 Formally, an instance of 'AdditiveGroup' must be an abelian group with
 'negate' as its inverse-constructing operation. This requires that @x '+'
 'negate' x = 'zero'@ and @x '-' y = x '+' 'negate' y@.

 @since 1.0
-}
class (AdditiveMonoid a) => AdditiveGroup a where
  {-# MINIMAL negate | (-) #-}

  -- | @since 1.0
  (-) :: a -> a -> a
  x - y = x + negate y

  -- | @since 1.0
  negate :: a -> a
  negate x = zero - x

-- | @since 1.0
instance AdditiveGroup Integer where
  {-# INLINEABLE (-) #-}
  (-) = (Prelude.-)
  {-# INLINEABLE negate #-}
  negate = Prelude.negate

-- | @since 1.0
instance
  (Fractionable a, AdditiveGroup a) =>
  AdditiveGroup (Ratio a)
  where
  {-# INLINEABLE (-) #-}
  (-) = viaRatio (-)
  {-# INLINEABLE negate #-}
  negate (Ratio (num, den)) = Ratio (negate num, den)

-- | @since 1.0
instance AdditiveGroup (Term s PInteger) where
  {-# INLINEABLE (-) #-}
  (-) = (Prelude.-)
  {-# INLINEABLE negate #-}
  negate = Prelude.negate

-- | @since 1.0
instance
  (PFractionable a, AdditiveGroup (Term s a)) =>
  AdditiveGroup (Term s (PRatio a))
  where
  {-# INLINEABLE (-) #-}
  (-) = viaPRatio (-)
  {-# INLINEABLE negate #-}
  negate t = pmatchRatio t $ \num den ->
    punsafeCoerce . pcon $ PPair (negate num) den

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
  Natural n ^- Natural n' = Natural . max 0 $ n Prelude.- n'

-- | @since 1.0
instance (Fractionable a, AdditiveCMM a) => AdditiveCMM (Ratio a) where
  {-# INLINEABLE (^-) #-}
  (^-) = viaRatio (^-)

-- | @since 1.0
instance AdditiveCMM (Term s PNatural) where
  {-# INLINEABLE (^-) #-}
  t ^- t' =
    plet
      (punsafeBuiltin PLC.SubtractInteger # t # t')
      (\(t'' :: Term s PInteger) -> pif (zero #<= t'') (punsafeCoerce t'') zero)

-- | @since 1.0
instance
  (PFractionable a, AdditiveCMM (Term s a)) =>
  AdditiveCMM (Term s (PRatio a))
  where
  {-# INLINEABLE (^-) #-}
  (^-) = viaPRatio (^-)

-- Helpers

viaRatio ::
  forall (a :: Type).
  (Fractionable a) =>
  (a -> a -> a) ->
  Ratio a ->
  Ratio a ->
  Ratio a
viaRatio f (Ratio (num, den)) (Ratio (num', den')) =
  let newDen = scale den den'
      newNum = f (scale num den') (scale num' den)
   in ratio newNum newDen

viaPRatio ::
  forall (a :: S -> Type) (s :: S).
  (PFractionable a) =>
  (Term s a -> Term s a -> Term s a) ->
  Term s (PRatio a) ->
  Term s (PRatio a) ->
  Term s (PRatio a)
viaPRatio f t t' = pmatchRatios t t' $ \num num' den den' ->
  plet (pscale den den') $ \newDen ->
    plet (f (pscale num den') (pscale num' den)) $ \newNum ->
      pconRatio newNum newDen
