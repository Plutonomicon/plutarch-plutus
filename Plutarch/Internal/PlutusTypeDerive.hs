{-# LANGUAGE UndecidableSuperClasses #-}

module Plutarch.Internal.PlutusTypeDerive where

import Data.Kind
import GHC.TypeError
import Plutarch.Internal.Term

class PlutusTypeStrat (strategy :: Type) where
  type PlutusTypeStratConstraint strategy :: PType -> Constraint
  type DerivedPInner strategy (a :: PType) :: PType
  derivedPCon :: forall a s. (DerivePlutusType a, DPTStrat a ~ strategy) => a s -> Term s (DerivedPInner strategy a)
  derivedPMatch :: forall a s b. (DerivePlutusType a, DPTStrat a ~ strategy) => Term s (DerivedPInner strategy a) -> (a s -> Term s b) -> Term s b

class
  ( -- PInner a ~ DerivedPInner (DPTStrat a) a
    PlutusTypeStrat (DPTStrat a)
  , PlutusTypeStratConstraint (DPTStrat a) a
  --  , PlutusType a
  ) =>
  DerivePlutusType (a :: PType)
  where
  type DPTStrat a :: Type
  type DPTStrat a = TypeError ('Text "Please specify a strategy for deriving PlutusType for type " ':<>: 'ShowType a)
