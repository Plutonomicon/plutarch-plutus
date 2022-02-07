{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Api.V1.Slot (PSlot (PSlot), SlotRange) where

import Plutarch.Api.V1.Interval
import Plutarch.Lift (DerivePConstantViaNewtype (DerivePConstantViaNewtype), PLifted, PUnsafeLiftDecl)
import Plutarch.Prelude
import qualified Plutus.V1.Ledger.Slot as PlutusSlot

newtype PSlot (s :: S) = PSlot (Term s PInteger)
  deriving (PlutusType, PIsData, PEq) via (DerivePNewtype PSlot PInteger)

instance PUnsafeLiftDecl PSlot where type PLifted PSlot = PlutusSlot.Slot
deriving via (DerivePConstantViaNewtype PlutusSlot.Slot PSlot PInteger) instance (PConstant PlutusSlot.Slot)

type SlotRange = PInterval PSlot
