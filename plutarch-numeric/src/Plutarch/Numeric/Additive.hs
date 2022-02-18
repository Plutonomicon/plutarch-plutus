module Plutarch.Numeric.Additive (
  AdditiveSemigroup (..),
  AdditiveMonoid (..),
  AdditiveGroup (..),
  AdditiveCMM (..),
) where

import Plutarch (Term, plet, (#))
import Plutarch.Bool (pif, (#<=))
import Plutarch.Integer (PInteger)
import Plutarch.Lift (pconstant)
import Plutarch.Numeric.NZNatural (NZNatural (NZNatural), PNZNatural)
import Plutarch.Numeric.Natural (Natural (Natural), PNatural)
import Plutarch.Unsafe (punsafeBuiltin, punsafeCoerce)
import PlutusCore qualified as PLC
import Prelude hiding (negate, (+))
import Prelude qualified

{- | A commutative semigroup, meant to be morally equivalent to numerical
 addition.

 = Laws

 Formally, an instance of 'AdditiveSemigroup' must be a commutative semigroup
 with '+' as its operation. Furthermore, 'Additive' 'NZNatural' must be a
 right semigroup action, and 'Multiplicative' 'NZNatural' must translate to composition,
 both witnessed by 'scaleNZNatural'.

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

{-
  -- | This uses a default \'sum by squaring\' implementation. While this is
  -- reasonably good by default, many types allow a more efficient
  -- implementation; try and define one if you can.
  --
  -- @since 1.0
  {-# INLINEABLE scaleNZNatural #-}
  scaleNZNatural :: a -> NZNatural -> a
  scaleNZNatural x (NZNatural n) = getAdditive . stimes n . Additive $ x
-}

-- | @since 1.0
instance AdditiveSemigroup Integer where
  {-# INLINEABLE (+) #-}
  (+) = (Prelude.+)

{-
{-# INLINEABLE scaleNZNatural #-}
  scaleNZNatural i (NZNatural n) = i Prelude.* n
-}

-- | @since 1.0
deriving via Integer instance (AdditiveSemigroup Natural)

-- | @since 1.0
deriving via Integer instance (AdditiveSemigroup NZNatural)

-- | @since 1.0
instance AdditiveSemigroup (Term s PInteger) where
  {-# INLINEABLE (+) #-}
  x + y = punsafeBuiltin PLC.AddInteger # x # y

{-
{-# INLINEABLE scaleNZNatural #-}
  scaleNZNatural i (NZNatural n) =
    punsafeBuiltin PLC.MultiplyInteger # i # pconstant n
-}

-- | @since 1.0
instance AdditiveSemigroup (Term s PNatural) where
  {-# INLINEABLE (+) #-}
  x + y = punsafeBuiltin PLC.AddInteger # x # y

{-
{-# INLINEABLE scaleNZNatural #-}
  scaleNZNatural i (NZNatural n) =
    punsafeBuiltin PLC.MultiplyInteger # i # pconstant n
-}

-- | @since 1.0
instance AdditiveSemigroup (Term s PNZNatural) where
  {-# INLINEABLE (+) #-}
  x + y = punsafeBuiltin PLC.AddInteger # x # y

{-
  {-# INLINEABLE scaleNZNatural #-}
  scaleNZNatural i (NZNatural n) =
    punsafeBuiltin PLC.MultiplyInteger # i # pconstant n
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
 * @'scaleNatural' x 'zero'@ @=@ @'zero'@

 @since 1.0
-}
class (AdditiveSemigroup a) => AdditiveMonoid a where
  {-# MINIMAL zero #-}

  -- | @since 1.0
  zero :: a

{-
  -- | This uses a default \'sum by squaring\' implementation. While this is
  -- reasonably good by default, many types allow a more efficient
  -- implementation; try and define one if you can.
  --
  -- @since 1.0
  {-# INLINEABLE scaleNatural #-}
  scaleNatural :: a -> Natural -> a
  scaleNatural x (Natural n) = getAdditive . stimesMonoid n . Additive $ x
-}

-- | @since 1.0
instance AdditiveMonoid Integer where
  {-# INLINEABLE zero #-}
  zero = 0

{-
{-# INLINEABLE scaleNatural #-}
  scaleNatural i (Natural n) = i Prelude.* n
-}

-- | @since 1.0
deriving via Integer instance (AdditiveMonoid Natural)

-- | @since 1.0
instance AdditiveMonoid (Term s PInteger) where
  {-# INLINEABLE zero #-}
  zero = pconstant 0

{-
{-# INLINEABLE scaleNatural #-}
  scaleNatural i n = punsafeBuiltin PLC.MultiplyInteger # i # pconstant n
-}

-- | @since 1.0
instance AdditiveMonoid (Term s PNatural) where
  {-# INLINEABLE zero #-}
  zero = pconstant . Natural $ 0

{-
{-# INLINEABLE scaleNatural #-}
  scaleNatural i n = punsafeBuiltin PLC.MultiplyInteger # i # pconstant n
-}

{- | An 'AdditiveMonoid' extended with a notion of negation.

 = Laws

 Formally, an instance of 'AdditiveGroup' must be an abelian group with
 'negate' as its inverse-constructing operation. Furthermore, it must form a
 left-'Integer' semimodule, witnessed by 'scaleInteger', which must be an
 extension of the left-'Natural' semimodule witnessed by 'scaleNatural' for
 non-negative elements.

 This requires that @x '+' 'negate' x = 'zero'@ and @x '-' y = x '+'
 'negate' y@; the second of these is the default implementation of '-'.
 Furthermore, we must have:

 * If @'Just' m = 'toNatural' n@, then @'scaleInteger' x n
 = 'scaleNatural' x m@.
 * If @'toNatural' n = 'Nothing'@, then @'scaleInteger'
 x n = 'negate' '.' 'scaleInteger' x . 'abs' '$' n@.

 @since 1.0
-}
class (AdditiveMonoid a) => AdditiveGroup a where
  {-# MINIMAL negate #-}

  -- | @since 1.0
  (-) :: a -> a -> a
  x - y = x + negate y

  -- | @since 1.0
  negate :: a -> a

{-
  -- | This uses a default \'sum by squaring\' implementation. While this is
  -- reasonably good by default, many types allow a more efficient
  -- implementation; try and define one if you can.
  --
  -- @since 1.0
  {-# INLINEABLE scaleInteger #-}
  scaleInteger :: a -> Integer -> a
  scaleInteger x i = getAdditive . gtimes i . Additive $ x
-}

-- | @since 1.0
instance AdditiveGroup Integer where
  {-# INLINEABLE (-) #-}
  (-) = (Prelude.-)
  {-# INLINEABLE negate #-}
  negate = Prelude.negate

{-
{-# INLINEABLE scaleInteger #-}
scaleInteger = (Prelude.*)
-}

-- | @since 1.0
instance AdditiveGroup (Term s PInteger) where
  {-# INLINEABLE (-) #-}
  (-) = (Prelude.-)
  {-# INLINEABLE negate #-}
  negate = Prelude.negate

{-
{-# INLINEABLE scaleInteger #-}
scaleInteger x i = x Prelude.* pconstant i
-}

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
instance AdditiveCMM (Term s PNatural) where
  {-# INLINEABLE (^-) #-}
  t ^- t' =
    plet
      (punsafeBuiltin PLC.SubtractInteger # t # t')
      (\(t'' :: Term s PInteger) -> pif (zero #<= t'') (punsafeCoerce t'') zero)
