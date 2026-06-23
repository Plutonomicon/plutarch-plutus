{-# LANGUAGE UndecidableSuperClasses #-}

module Plutarch.Primitive.Match (
  PMatch (..),
  pmatch,
) where

import Data.Kind (Type)
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEVector
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (
  SomeTerm,
  Term,
  plam',
  punsafeCase,
  toSomeTerm,
 )
import Plutarch.Primitive.Apply (
  PlutarchType (PRepresentation),
  pcoerce,
 )
import Plutarch.Primitive.Function ((:-->))
import Plutarch.Primitive.List (PBList (PBCons, PBNil))
import Plutarch.Primitive.Pair (PBPair (PBPair))

{- | = Laws

1. @'pmatch' ('pmatch' x f) g@ @=@ @'pmatch' x (\x' -> 'pmatch' (f x) g)@
2. @f '#' ('pmatch' x g)@ @=@ @'pmatch' x (\x' -> f # g x')@

@since wip
-}
class PlutarchType a => PMatch a where
  pmatch' ::
    forall (b :: S -> Type) (s :: S).
    Term s (PRepresentation a) -> (a s -> Term s b) -> Term s b

-- | @since wip
instance PlutarchType a => PMatch (PBList a) where
  pmatch' ::
    forall (b :: S -> Type) (s :: S).
    Term s (PRepresentation (PBList a)) -> (PBList a s -> Term s b) -> Term s b
  pmatch' x f = punsafeCase x handlers
    where
      handlers :: NonEmptyVector (SomeTerm s)
      handlers = NEVector.cons (toSomeTerm whenCons) . NEVector.singleton . toSomeTerm $ whenNil
      whenNil :: Term s b
      whenNil = f PBNil
      whenCons :: Term s (a :--> PBList a :--> b)
      whenCons = plam' $ \h -> plam' $ \t -> f (PBCons h t)

-- | @since wip
instance (PlutarchType a, PlutarchType b) => PMatch (PBPair a b) where
  pmatch' ::
    forall (c :: S -> Type) (s :: S).
    Term s (PRepresentation (PBPair a b)) -> (PBPair a b s -> Term s c) -> Term s c
  pmatch' x f = punsafeCase x . NEVector.singleton . toSomeTerm $ handler
    where
      handler :: Term s (a :--> b :--> c)
      handler = plam' $ \x -> plam' $ \y -> f (PBPair x y)

-- | @since wip
pmatch ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  PMatch a => Term s a -> (a s -> Term s b) -> Term s b
pmatch x = pmatch' (pcoerce x)
