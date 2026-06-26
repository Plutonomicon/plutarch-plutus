{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Primitive.List (
  PBList (..),
) where

import Data.Kind (Type)
import Plutarch.Backend.Evaluate (peval)
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (Term, punsafeConstant)
import Plutarch.Primitive.Apply (PlutarchType (PRepresentation))
import Plutarch.Primitive.Liftable (
  LiftError (DidNotEvaluate, NotAConstant, WrongConstantType),
  PLiftable (
    PAsHaskell,
    PAsPlutus,
    haskToRepr,
    plutToRepr,
    reprToHask,
    reprToPlut
  ),
 )
import PlutusCore qualified as PLC
import PlutusCore.Builtin (geqL)

-- | @since wip
data PBList (a :: S -> Type) (s :: S)
  = PBNil
  | PBCons (Term s a) (Term s (PBList a))

-- | @since wip
instance PlutarchType a => PlutarchType (PBList a) where
  type PRepresentation (PBList a) = PBList (PRepresentation a)

-- | @since wip
instance PLiftable a => PLiftable (PBList a) where
  type PAsHaskell (PBList a) = [PAsHaskell a]
  type PAsPlutus (PBList a) = [PAsPlutus a]
  haskToRepr = fmap (haskToRepr @a)
  reprToHask = traverse (reprToHask @a)
  reprToPlut xs = punsafeConstant $ PLC.someValue @[PAsPlutus a] xs
  plutToRepr t = case peval t of
    Left err -> Left . DidNotEvaluate $ err
    Right (Right _) -> Left NotAConstant
    Right (Left c) -> case c of
      PLC.Some (PLC.ValueOf actualProof xs) -> do
        let expectedProof = PLC.knownUni @_ @PLC.DefaultUni @[PAsPlutus a]
        case geqL expectedProof actualProof of
          PLC.EvaluationSuccess PLC.Refl -> pure xs
          PLC.EvaluationFailure -> Left WrongConstantType
