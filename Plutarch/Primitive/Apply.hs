{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Primitive.Apply (
  PMatch (..),
  PCon (..),
  pmatch,
  pcon,
  PMatchFundamental (..),
  PMatchRepresentation (..),
) where

import Data.Kind (Type)
import Data.Vector.NonEmpty qualified as NEVector
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (
  Term,
  plam',
  punsafeCase,
  punsafeCoerce,
  toSomeTerm,
 )
import Plutarch.Primitive.Bool (PBool)
import Plutarch.Primitive.ByteString (PByteString)
import Plutarch.Primitive.Data (PAsData, PData)
import Plutarch.Primitive.Function ((:-->))
import Plutarch.Primitive.List (PBList (PBCons, PBNil))
import Plutarch.Primitive.Numeric (
  PByte,
  PInteger,
  PNatural,
  PPositive,
 )
import Plutarch.Primitive.Pair (PBPair (PBPair))
import Plutarch.Primitive.Representation (
  PIsFundamental,
  PIsNotFundamental,
  PRepresentation,
 )

{- | = Laws

1. @('pmatch' x f) g@ @=@ @'pmatch' x (\x' -> 'pmatch' (f x) g)@

@since wip
-}
class PMatch (a :: S -> Type) where
  pmatch' ::
    forall (b :: S -> Type) (s :: S).
    Term s (PRepresentation a) -> (a s -> Term s b) -> Term s b

-- | @since wip
instance PMatch (PBPair a b) where
  pmatch' x f = punsafeCase x . NEVector.singleton . toSomeTerm $
    plam' $
      \x -> plam' $ \y -> f (PBPair x y)

-- | @since wip
instance PMatch (PBList a) where
  pmatch' ::
    forall (b :: S -> Type) (s :: S).
    Term s (PRepresentation (PBList a)) -> (PBList a s -> Term s b) -> Term s b
  pmatch' x f = punsafeCase x . NEVector.cons (toSomeTerm whenCons) . NEVector.singleton . toSomeTerm $ whenNil
    where
      whenNil :: Term s b
      whenNil = f PBNil
      whenCons :: Term s (a :--> PBList a :--> b)
      whenCons = plam' $ \y -> plam' $ \ys -> f (PBCons y ys)

-- | @since wip
instance PMatch (a :--> b) where
  pmatch' ::
    forall (c :: S -> Type) (s :: S).
    Term s (PRepresentation (a :--> b)) -> ((a :--> b) s -> Term s c) -> Term s c
  pmatch' x _ = punsafeCoerce x

{- | = Laws

1. @'pmatch' ('pcon' x) f@ @=@ @f x@
2. @'pmatch' x 'pcon'@ @=@ @x@

@since wip
-}
class PMatch a => PCon (a :: S -> Type) where
  pcon' :: forall (s :: S). a s -> Term s (PRepresentation a)

{- | Convenience wrapper to avoid having to refer to 'PRepresentation's.

@since wip
-}
pcon ::
  forall (a :: S -> Type) (s :: S).
  PCon a =>
  a s -> Term s a
pcon = punsafeCoerce . pcon'

{- | Convenience wrapper to avoid having to refer to 'PRepresentation's.

@since wip
-}
pmatch ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  PMatch a =>
  Term s a -> (a s -> Term s b) -> Term s b
pmatch x = pmatch' (punsafeCoerce x)

{- | A derivation helper for 'PMatch', for use with @deriving via@. Such a
derivation can be used for any fundamental type (that is, any type for which
@'PRepresentation' a ~ a@.

@since wip
-}
newtype PMatchFundamental (a :: S -> Type) (s :: S) = PMatchFundamental (a s)

-- | @since wip
type instance PRepresentation (PMatchFundamental a) = a

-- | @since wip
instance PIsFundamental a => PMatch (PMatchFundamental a) where
  pmatch' x _ = punsafeCoerce x

-- | @since wip
deriving via (PMatchFundamental PInteger) instance PMatch PInteger

-- | @since wip
deriving via (PMatchFundamental PBool) instance PMatch PBool

-- | @since wip
deriving via (PMatchFundamental PByteString) instance PMatch PByteString

-- | @since wip
deriving via (PMatchFundamental PData) instance PMatch PData

{- | A derivation helper for 'PMatch', for use with @deriving via@. Such a
derivation can be used for any type that is /not/ fundamental (that is, any
type where @'PRepresentation' a@ and @a@ are different types).

Semantically, such an instance delegates 'pmatch' logic to the representation
of @a@. This should be used for anything \'@newtype@-like\'.

@since wip
-}
newtype PMatchRepresentation (a :: S -> Type) (s :: S) = PMatchRepresentation (a s)

-- | @since wip
type instance PRepresentation (PMatchRepresentation a) = PRepresentation a

-- | @since wip
instance PIsNotFundamental a => PMatch (PMatchRepresentation a) where
  pmatch' x = pmatch (punsafeCoerce x)

-- | @since wip
deriving via (PMatchRepresentation PNatural) instance PMatch PNatural

-- | @since wip
deriving via (PMatchRepresentation PPositive) instance PMatch PPositive

-- | @since wip
deriving via (PMatchRepresentation PByte) instance PMatch PByte

-- | @since wip
deriving via (PMatchRepresentation (PAsData a)) instance PMatch (PAsData a)
