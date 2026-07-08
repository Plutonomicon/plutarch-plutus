{-# LANGUAGE UndecidableSuperClasses #-}

module Plutarch.Primitive.Ord (
  POrd (..),
  (#<=),
  (#<),
  (#>=),
  (#>),
  pmin,
  pmax,
) where

import Data.Kind (Type)
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (
  Term,
  plam',
  punsafeCoerce,
 )
import Plutarch.Primitive.Apply (
  PlutarchType (PRepresentation),
  (#),
 )
import Plutarch.Primitive.Bool (PBool, pfalse, pif, ptrue)
import Plutarch.Primitive.BuiltinFun (
  plessThanByteString,
  plessThanEqualsByteString,
  plessThanEqualsInteger,
  plessThanInteger,
 )
import Plutarch.Primitive.ByteString (PByteString)
import Plutarch.Primitive.Eq (PEq)
import Plutarch.Primitive.Function ((:-->))
import Plutarch.Primitive.Match (pmatch)
import Plutarch.Primitive.Numeric (PByte, PInteger, PNatural, PPositive)
import Plutarch.Primitive.Pair (PBPair (PBPair))

-- | @since wip
class PEq a => POrd (a :: S -> Type) where
  pleq :: forall (s :: S). Term s (a :--> a :--> PBool)
  default pleq ::
    forall (s :: S).
    POrd (PRepresentation a) =>
    Term s (a :--> a :--> PBool)
  pleq = punsafeCoerce (pleq @(PRepresentation a))
  plt :: forall (s :: S). Term s (a :--> a :--> PBool)
  default plt ::
    forall (s :: S).
    POrd (PRepresentation a) =>
    Term s (a :--> a :--> PBool)
  plt = punsafeCoerce (plt @(PRepresentation a))

-- | @since wip
instance POrd PBool where
  pleq = plam' $ \x -> plam' $ \y ->
    pif x y ptrue
  plt = plam' $ \x -> plam' $ \y ->
    pif x pfalse y

{- | = Note on performance

Due to current costing parameters, 'plt' for 'PInteger' (and anything that
has 'PInteger' as a representation) is less efficient than 'pleq'.

@since wip
-}
instance POrd PInteger where
  pleq = plessThanEqualsInteger
  plt = plessThanInteger

-- | @since wip
instance POrd PNatural

-- | @since wip
instance POrd PPositive

-- | @since wip
instance POrd PByte

-- | @since wip
instance POrd PByteString where
  pleq = plessThanEqualsByteString
  plt = plessThanByteString

-- | @since wip
instance (POrd a, POrd b) => POrd (PBPair a b) where
  pleq = plam' $ \xs -> plam' $ \ys ->
    pmatch xs $ \(PBPair x1 x2) ->
      pmatch ys $ \(PBPair y1 y2) ->
        pif
          (pleq # x1 # y1)
          (pleq # x2 # y2)
          pfalse
  plt = plam' $ \xs -> plam' $ \ys ->
    pmatch xs $ \(PBPair x1 x2) ->
      pmatch ys $ \(PBPair y1 y2) ->
        pif
          (plt # x1 # y1)
          (plt # x2 # y2)
          pfalse

-- | @since wip
pmin ::
  forall (a :: S -> Type) (s :: S).
  POrd a =>
  Term s a -> Term s a -> Term s a
pmin x y = pif (pleq # x # y) x y

-- | @since wip
pmax ::
  forall (a :: S -> Type) (s :: S).
  POrd a =>
  Term s a -> Term s a -> Term s a
pmax x y = pif (pleq # x # y) y x

-- | @since wip
(#<=) ::
  forall (a :: S -> Type) (s :: S).
  POrd a =>
  Term s a -> Term s a -> Term s PBool
x #<= y = pleq # x # y

-- | @since wip
infix 4 #<=

-- | @since wip
(#<) ::
  forall (a :: S -> Type) (s :: S).
  POrd a =>
  Term s a -> Term s a -> Term s PBool
x #< y = plt # x # y

-- | @since wip
infix 4 #<

-- | @since wip
(#>=) ::
  forall (a :: S -> Type) (s :: S).
  POrd a =>
  Term s a -> Term s a -> Term s PBool
x #>= y = y #<= x

-- | @since wip
infix 4 #>=

-- | @since wip
(#>) ::
  forall (a :: S -> Type) (s :: S).
  POrd a =>
  Term s a -> Term s a -> Term s PBool
x #> y = y #< x

-- | @since wip
infix 4 #>
