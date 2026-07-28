{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Primitive.Encoding (
  Encoding (..),
  PEncodingRep (..),
  RepOf,
  PAppropriate,
  mapEncodingRep,
) where

import Data.Kind (Constraint, Type)
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (Term)
import Plutarch.Primitive.Apply (PlutarchType (PRepresentation), (#))
import Plutarch.Primitive.CanData (PCanData (pfromData, ptoData))
import Plutarch.Primitive.Con (PCon (pcon'), pcon)
import Plutarch.Primitive.Data (PAsData)
import Plutarch.Primitive.Function ((:-->))
import Plutarch.Primitive.Match (PMatch (pmatch'), pmatch)

-- | @since wip
data Encoding
  = SOPE
  | DataPlutusE
  | DataListE
  | EnumE
  | MSE
  deriving stock
    ( -- | @since wip
      Eq
    , -- | @since wip
      Show
    )

-- | @since wip
data PEncodingRep (enc :: Encoding) (a :: S -> Type) (s :: S) where
  PRepSOP :: Term s a -> PEncodingRep 'SOPE a s
  PRepDataPlutus :: Term s (PAsData a) -> PEncodingRep 'DataPlutusE a s
  PRepDataList :: Term s (PAsData a) -> PEncodingRep 'DataListE a s
  PRepMS :: Term s a -> PEncodingRep 'MSE a s

-- | @since wip
instance
  (PAppropriate enc a, PlutarchType (RepOf enc a)) =>
  PlutarchType (PEncodingRep enc a)
  where
  type PRepresentation (PEncodingRep enc a) = RepOf enc a

-- | @since wip
instance PlutarchType a => PMatch (PEncodingRep 'SOPE a) where
  pmatch' t f = f (PRepSOP t)

-- | @since wip
instance PCanData a => PMatch (PEncodingRep 'DataPlutusE a) where
  pmatch' t f = f (PRepDataPlutus t)

-- | @since wip
instance PCanData a => PMatch (PEncodingRep 'DataListE a) where
  pmatch' t f = f (PRepDataList t)

-- | @since wip
instance PlutarchType a => PMatch (PEncodingRep 'MSE a) where
  pmatch' t f = f (PRepMS t)

-- | @since wip
instance
  (PMatch (PEncodingRep enc a), PAppropriate enc a) =>
  PCon (PEncodingRep enc a)
  where
  pcon' = \case
    PRepSOP t -> t
    PRepDataPlutus t -> t
    PRepDataList t -> t
    PRepMS t -> t

-- @since wip
type family RepOf (enc :: Encoding) (a :: S -> Type) :: S -> Type where
  RepOf 'SOPE a = a
  RepOf 'DataPlutusE a = PAsData a
  RepOf 'DataListE a = PAsData a
  RepOf 'MSE a = a

-- | @since wip
type family PAppropriate (enc :: Encoding) (a :: S -> Type) :: Constraint where
  PAppropriate 'SOPE a = PlutarchType a
  PAppropriate 'DataPlutusE a = PCanData a
  PAppropriate 'DataListE a = PCanData a
  PAppropriate 'MSE a = PlutarchType a

-- | @since wip
mapEncodingRep ::
  forall (a :: S -> Type) (b :: S -> Type) (enc :: Encoding) (s :: S).
  (PMatch (PEncodingRep enc a), PAppropriate enc a, PAppropriate enc b) =>
  Term s (a :--> b) ->
  Term s (PEncodingRep enc a) ->
  Term s (PEncodingRep enc b)
mapEncodingRep f t = pmatch t $ \case
  PRepSOP t' -> pcon . PRepSOP $ f # t'
  PRepDataPlutus t' -> pcon . PRepDataPlutus . ptoData $ f # pfromData t'
  PRepDataList t' -> pcon . PRepDataList . ptoData $ f # pfromData t'
  PRepMS t' -> pcon . PRepMS $ f # t'
