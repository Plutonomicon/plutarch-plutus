{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Internal.Numeric.Multiplicative (
  -- * Type classes
  PMultiplicativeSemigroup (..),
  PMultiplicativeMonoid (..),
) where

import Data.Kind (Type)
import Plutarch.Builtin.Integer (
  PInteger,
  pconstantInteger,
  pmultiplyInteger,
 )
import Plutarch.Internal.Numeric.Additive (
  PPositive,
  pbySquaringDefault,
 )
import Plutarch.Internal.Other (pto)
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.PlutusType (PInner)
import Plutarch.Internal.Term (
  S,
  Term,
  phoistAcyclic,
  punsafeCoerce,
  (#),
  (:-->),
 )
import Plutarch.Unsafe (punsafeDowncast)

{- | The multiplication operation.

= Laws

1. @x #* (y #* z)@ @=@ @(x #* y) #* z@ (associativity of @#*@)

If you define a custom @ppowPositive@, ensure the following also hold:

3. @ppowPositive # x # pone@ @=@ @x@
4. @(ppowPositive # x # n) #* (ppowPositive # x # m)@ @=@
   @pscalePositive # x # (n #+ m)@
5. @ppowPositive # (ppowPositive # x # n) # m@ @=@
   @pscalePositive # x # (n #* m)@

The default implementation ensures these laws are satisfied.

= Note

Unlike 'PAdditiveSemigroup', the multiplication operation doesn't need to be
commutative. Currently, all Plutarch-provided instances are, but this need
not be true for other instances.

@since WIP
-}
class PMultiplicativeSemigroup (a :: S -> Type) where
  {-# INLINEABLE (#*) #-}
  (#*) :: forall (s :: S). Term s a -> Term s a -> Term s a
  default (#*) ::
    forall (s :: S).
    PMultiplicativeSemigroup (PInner a) =>
    Term s a ->
    Term s a ->
    Term s a
  x #* y = punsafeDowncast $ pto x #* pto y
  {-# INLINEABLE ppowPositive #-}
  ppowPositive ::
    forall (s :: S).
    Term s (a :--> PPositive :--> a)
  ppowPositive = phoistAcyclic $ plam $ \x p ->
    pbySquaringDefault (#*) # x # p

-- | @since WIP
infix 6 #*

-- | @since WIP
instance PMultiplicativeSemigroup PPositive where
  {-# INLINEABLE (#*) #-}
  x #* y = punsafeCoerce $ pmultiplyInteger # pto x # pto y

-- | @since WIP
instance PMultiplicativeSemigroup PInteger where
  {-# INLINEABLE (#*) #-}
  x #* y = pmultiplyInteger # x # y

{- | The notion of one (multiplicative identity).

= Laws

1. @pone #* x@ @=@ @x@ (@pone@ is the left identity of @#*@)
2. @x #* pone@ @=@ @x@ (@pone@ is the right identity of @#*@)

@since WIP
-}
class PMultiplicativeSemigroup a => PMultiplicativeMonoid (a :: S -> Type) where
  pone :: forall (s :: S). Term s a

-- | @since WIP
instance PMultiplicativeMonoid PPositive where
  {-# INLINEABLE pone #-}
  pone = punsafeCoerce $ pconstantInteger 1

-- | @since WIP
instance PMultiplicativeMonoid PInteger where
  {-# INLINEABLE pone #-}
  pone = pconstantInteger 1
