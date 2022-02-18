{-# LANGUAGE TypeFamilies #-}

module Plutarch.Numeric.Ratio (
  PRatio (..),
  Ratio (..),
  numerator,
  pnumerator,
  denominator,
  pdenominator,
  pmatchRatio,
  pmatchRatios,
) where

import Data.Kind (Type)
import Plutarch (DerivePNewtype (DerivePNewtype), PMatch (pmatch), S, Term)
import Plutarch.Bool (PEq ((#==)), (#&&))
import Plutarch.Lift (
  PConstant (
    PConstantRepr,
    PConstanted,
    pconstantFromRepr,
    pconstantToRepr
  ),
  PUnsafeLiftDecl (PLifted),
 )
import Plutarch.Numeric.NZNatural (NZNatural (NZNatural), PNZNatural)
import Plutarch.Pair (PPair (PPair))

{- | Plutarch version of 'Ratio'.

 @since 1.0
-}
newtype PRatio (a :: S -> Type) (s :: S) = PRatio (Term s (PPair a PNZNatural))

-- | @since 1.0
deriving via
  (DerivePNewtype (PRatio a) (PPair a PNZNatural))
  instance
    PMatch (PRatio a)

-- | @since 1.0
instance (PEq a) => PEq (PRatio a) where
  {-# INLINEABLE (#==) #-}
  t #== t' = pmatchRatios t t' $ \num num' den den' ->
    (num #== num') #&& (den #== den')

{- | A ratio whose numerator is the specified type.

 @since 1.0
-}
newtype Ratio (a :: Type) = Ratio (a, NZNatural)
  deriving
    ( -- | @since 1.0
      Eq
    )
    via (a, NZNatural)

-- | @since 1.0
instance (PUnsafeLiftDecl a) => PUnsafeLiftDecl (PRatio a) where
  type PLifted (PRatio a) = Ratio (PLifted a)

-- | @since 1.0
instance (PConstant a) => PConstant (Ratio a) where
  type PConstantRepr (Ratio a) = (PConstantRepr a, Integer)
  type PConstanted (Ratio a) = PRatio (PConstanted a)
  {-# INLINEABLE pconstantToRepr #-}
  pconstantToRepr (Ratio (num, NZNatural den)) = (pconstantToRepr num, den)
  {-# INLINEABLE pconstantFromRepr #-}
  pconstantFromRepr (num, den)
    | den < 1 = Nothing
    | otherwise = case pconstantFromRepr num of
        Nothing -> Nothing
        Just num' -> Just . Ratio $ (num', NZNatural den)

-- | @since 1.0
numerator ::
  forall (a :: Type).
  Ratio a ->
  a
numerator (Ratio (x, _)) = x

-- | @since 1.0
denominator ::
  forall (a :: Type).
  Ratio a ->
  NZNatural
denominator (Ratio (_, y)) = y

-- | @since 1.0
pnumerator ::
  forall (a :: S -> Type) (s :: S).
  PRatio a s ->
  Term s a
pnumerator (PRatio t) = pmatch t $ \(PPair t' _) -> t'

-- | @since 1.0
pdenominator ::
  forall (a :: S -> Type) (s :: S).
  PRatio a s ->
  Term s PNZNatural
pdenominator (PRatio t) = pmatch t $ \(PPair _ t') -> t'

-- | @since 1.0
pmatchRatio ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s (PRatio a) ->
  (Term s a -> Term s PNZNatural -> Term s b) ->
  Term s b
pmatchRatio t f =
  pmatch t $ \(PRatio pp) ->
    pmatch pp $ \(PPair num den) ->
      f num den

-- | @since 1.0
pmatchRatios ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s (PRatio a) ->
  Term s (PRatio a) ->
  (Term s a -> Term s a -> Term s PNZNatural -> Term s PNZNatural -> Term s b) ->
  Term s b
pmatchRatios t t' f =
  pmatchRatio t $ \num den ->
    pmatchRatio t' $ \num' den' ->
      f num num' den den'
