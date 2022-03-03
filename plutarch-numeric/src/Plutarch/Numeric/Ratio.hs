{-# LANGUAGE TypeFamilies #-}

module Plutarch.Numeric.Ratio (
  PRatio (..),
  Ratio (..),
  ratio,
  pconRatio,
  numerator,
  pnumerator,
  denominator,
  pdenominator,
  pmatchRatio,
  pmatchRatios,
) where

import Data.Kind (Type)
import Plutarch (
  DerivePNewtype (DerivePNewtype),
  PMatch (pmatch),
  S,
  Term,
  pcon,
  plet,
 )
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
import Plutarch.Numeric.Fractional (
  Fractionable (findScale, unscale),
  PFractionable (pfindScale, punscale),
 )
import Plutarch.Numeric.NZNatural (NZNatural (NZNatural), PNZNatural)
import Plutarch.Pair (PPair (PPair))
import Plutarch.Unsafe (punsafeCoerce)

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

{- | As 'ratio', but for Plutarch 'Term's.

 @since 1.0
-}
pconRatio ::
  forall (a :: S -> Type) (s :: S).
  (PFractionable a) =>
  Term s a ->
  Term s PNZNatural ->
  Term s (PRatio a)
pconRatio num den = plet (pfindScale num den) $ \scaledown ->
  punsafeCoerce . pcon $ PPair (punscale num scaledown) (punscale den scaledown)

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

{- | Given a numerator and a denominator, construct a 'Ratio'.

 @since 1.0
-}
ratio ::
  forall (a :: Type).
  (Fractionable a) =>
  a ->
  NZNatural ->
  Ratio a
ratio num den =
  let scaledown = findScale num den
   in Ratio (unscale num scaledown, unscale den scaledown)

{- | Retrieves the numerator.

 @since 1.0
-}
numerator ::
  forall (a :: Type).
  Ratio a ->
  a
numerator (Ratio (x, _)) = x

{- | Retrieves the denominator.

 @since 1.0
-}
denominator ::
  forall (a :: Type).
  Ratio a ->
  NZNatural
denominator (Ratio (_, y)) = y

{- | As 'numerator', but for a Plutarch 'Term' (and ratio).

 @since 1.0
-}
pnumerator ::
  forall (a :: S -> Type) (s :: S).
  PRatio a s ->
  Term s a
pnumerator (PRatio t) = pmatch t $ \(PPair t' _) -> t'

{- | As 'denominator', but for a Plutarch 'Term' (and ratio).

 @since 1.0
-}
pdenominator ::
  forall (a :: S -> Type) (s :: S).
  PRatio a s ->
  Term s PNZNatural
pdenominator (PRatio t) = pmatch t $ \(PPair _ t') -> t'

{- | Helper for \'deconstructing\' a @'Term' s ('PRatio' a)@ into its numerator
 and denominator parts.

 @since 1.0
-}
pmatchRatio ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s (PRatio a) ->
  (Term s a -> Term s PNZNatural -> Term s b) ->
  Term s b
pmatchRatio t f =
  pmatch t $ \(PRatio pp) ->
    pmatch pp $ \(PPair num den) ->
      f num den

{- | Helper for \'deconstructing\' two @'Term' s ('PRatio' a)@s into their
 respective numerators and denominators.

 @since 1.0
-}
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
