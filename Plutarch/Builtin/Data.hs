{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Builtin.Data (
  ) where

import Data.Data
import Plutarch
import Plutarch.Builtin
import qualified Plutarch.Builtin as B
import Plutarch.Builtin.Data.Type
import Plutarch.Builtin.List.Type
import qualified PlutusCore.Data as PLC
import qualified PlutusCore.Default as PLC

instance PlutusType PData where
  type PInner PData _ = POpaque
  pcon' = \case
    PDataConstr pair ->
      B.ConstrData #£ pair
    _ ->
      undefined
  pmatch' x f =
    pforce $
      B.ChooseData #£ x
        £ pdelay (f (PDataConstr (B.UnConstrData #£ x)))
        £ pdelay perror
        £ pdelay perror
        £ pdelay perror
        £ pdelay perror

instance ListElemUni PData where
  type ListElemType PData = PLC.Data
  listElemUni Proxy = PLC.DefaultUniData
