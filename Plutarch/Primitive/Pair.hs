module Plutarch.Primitive.Pair (
  PBPair (..),
) where

import Data.Bifunctor (bimap)
import Data.Bitraversable (bitraverse)
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
data PBPair (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PBPair (Term s a) (Term s b)

-- | @since wip
instance (PlutarchType a, PlutarchType b) => PlutarchType (PBPair a b) where
  type PRepresentation (PBPair a b) = PBPair (PRepresentation a) (PRepresentation b)

-- | @since wip
instance (PLiftable a, PLiftable b) => PLiftable (PBPair a b) where
  type PAsHaskell (PBPair a b) = (PAsHaskell a, PAsHaskell b)
  type PAsPlutus (PBPair a b) = (PAsPlutus a, PAsPlutus b)
  haskToRepr = bimap (haskToRepr @a) (haskToRepr @b)
  reprToHask = bitraverse (reprToHask @a) (reprToHask @b)
  plutToRepr t = case peval t of
    Left err -> Left . DidNotEvaluate $ err
    Right (Right _) -> Left NotAConstant
    Right (Left c) -> case c of
      PLC.Some (PLC.ValueOf actualProof p) -> do
        let expectedProof = PLC.knownUni @_ @PLC.DefaultUni @(PAsPlutus a, PAsPlutus b)
        case geqL expectedProof actualProof of
          PLC.EvaluationSuccess PLC.Refl -> pure p
          PLC.EvaluationFailure -> Left WrongConstantType
  reprToPlut = punsafeConstant . PLC.someValue @(PAsPlutus a, PAsPlutus b)
