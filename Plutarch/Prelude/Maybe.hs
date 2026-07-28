{-# LANGUAGE FlexibleInstances #-}

module Plutarch.Prelude.Maybe (
  PMaybe (..),
  pisJust,
  pmapMaybe,
) where

import Data.Kind (Type)
import Data.Vector qualified as Vector
import Data.Vector.NonEmpty as NEVector
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (
  Term,
  plam',
  punsafeCase,
  punsafeCoerce,
  punsafeConstant,
  punsafeConstr,
  toSomeTerm,
 )
import Plutarch.Primitive.Apply (
  PlutarchType (PRepresentation),
  pcoerce,
  (#),
  (#$),
 )
import Plutarch.Primitive.Bool (PBool, pfalse, ptrue)
import Plutarch.Primitive.BuiltinFun (
  pconstrData,
  pheadList,
  pmkCons,
  pnilData,
  punConstrData,
 )
import Plutarch.Primitive.CanData (PCanData)
import Plutarch.Primitive.Con (PCon (pcon'), pcon)
import Plutarch.Primitive.Data (PData)
import Plutarch.Primitive.Encoding (
  Encoding (DataPlutusE, SOPE),
  PAppropriate,
  PEncodingRep (PRepDataPlutus, PRepSOP),
  mapEncodingRep,
 )
import Plutarch.Primitive.Function ((:-->))
import Plutarch.Primitive.List (PBList)
import Plutarch.Primitive.Match (PMatch (pmatch'), pmatch)
import Plutarch.Primitive.Numeric (PInteger)
import Plutarch.Primitive.Pair (PBPair (PBPair))
import Plutarch.Primitive.SOP (PSOP)
import PlutusCore qualified as PLC

-- | @since wip
data PMaybe (enc :: Encoding) (a :: S -> Type) (s :: S)
  = PNothing
  | PJust (Term s (PEncodingRep enc a))

-- | @since wip
instance PlutarchType a => PlutarchType (PMaybe 'SOPE a) where
  type PRepresentation (PMaybe 'SOPE a) = PSOP

-- | @since wip
instance PCanData a => PlutarchType (PMaybe 'DataPlutusE a) where
  type PRepresentation (PMaybe 'DataPlutusE a) = PData

-- | @since wip
instance PlutarchType a => PMatch (PMaybe 'SOPE a) where
  pmatch' ::
    forall (b :: S -> Type) (s :: S).
    Term s PSOP -> (PMaybe 'SOPE a s -> Term s b) -> Term s b
  pmatch' t f =
    punsafeCase t
      . NEVector.cons (toSomeTerm whenNothing)
      . NEVector.singleton
      $ toSomeTerm whenJust
    where
      whenNothing :: Term s b
      whenNothing = f PNothing
      whenJust :: Term s (a :--> b)
      whenJust = plam' $ \x -> f . PJust . pcon . PRepSOP $ x

-- | @since wip
instance PCanData a => PMatch (PMaybe 'DataPlutusE a) where
  pmatch' ::
    forall (b :: S -> Type) (s :: S).
    Term s PData -> (PMaybe 'DataPlutusE a s -> Term s b) -> Term s b
  pmatch' t f = pmatch (punConstrData # t) $ \(PBPair tag fields) ->
    punsafeCase tag
      . NEVector.cons (toSomeTerm whenNothing)
      . NEVector.singleton
      $ toSomeTerm (whenJust fields)
    where
      whenNothing :: Term s b
      whenNothing = f PNothing
      whenJust :: Term s (PBList PData) -> Term s b
      whenJust fields = punsafeCase fields . NEVector.singleton . toSomeTerm $ go fields
      go :: Term s (PBList PData) -> Term s b
      go fields = f . PJust . pcon . PRepDataPlutus . punsafeCoerce $ pheadList # fields

-- | @since wip
instance PlutarchType a => PCon (PMaybe 'SOPE a) where
  pcon' = \case
    PNothing -> punsafeConstr 0 Vector.empty
    PJust t -> punsafeConstr 1 . Vector.singleton . toSomeTerm $ t

-- | @since wip
instance PCanData a => PCon (PMaybe 'DataPlutusE a) where
  pcon' :: forall (s :: S). PMaybe 'DataPlutusE a s -> Term s PData
  pcon' = \case
    PNothing -> pconstrData # pizero # pnilData
    PJust t -> pconstrData # pione #$ pmkCons # pcoerce (pcoerce t) # pnilData
    where
      pizero :: Term s PInteger
      pizero = punsafeConstant . PLC.someValue @Integer $ 0
      pione :: Term s PInteger
      pione = punsafeConstant . PLC.someValue @Integer $ 1

-- | @since wip
pisJust ::
  forall (enc :: Encoding) (a :: S -> Type) (s :: S).
  PMatch (PMaybe enc a) =>
  Term s (PMaybe enc a :--> PBool)
pisJust = plam' $ \x -> pmatch x $ \case
  PNothing -> pfalse
  PJust _ -> ptrue

-- | @since wip
pmapMaybe ::
  forall (enc :: Encoding) (a :: S -> Type) (b :: S -> Type) (s :: S).
  ( PMatch (PMaybe enc a)
  , PCon (PMaybe enc b)
  , PAppropriate enc a
  , PAppropriate enc b
  , PMatch (PEncodingRep enc a)
  ) =>
  Term s (a :--> b) ->
  Term s (PMaybe enc a :--> PMaybe enc b)
pmapMaybe f = plam' $ \x -> pmatch x $ \case
  PNothing -> pcon PNothing
  PJust t -> pcon . PJust . mapEncodingRep f $ t
