{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Builtin.Data (
  ) where

import Data.Data
import Plutarch
import Plutarch.Bool ( PEq(..) )
import Plutarch.Builtin ( (#£) )
import qualified Plutarch.Builtin as B
import Plutarch.Builtin.Data.Type
import Plutarch.Builtin.List.Type
import qualified PlutusCore.Data as PLC
import qualified PlutusCore.Default as PLC

instance PlutusType PData where
  type PInner PData _ = PData
  pcon' = \case
    PDataConstr pair ->
      B.ConstrData #£ pair
    PDataMap m ->
      B.MapData #£ m
    PDataList l ->
      B.ListData #£ l
    PDataInteger i ->
      B.IData #£ i
    PDataByteString bs ->
      B.BData #£ bs
  pmatch' x f =
    pforce $
      B.ChooseData #£ x
        £ pdelay (f (PDataConstr (B.UnConstrData #£ x)))
        £ pdelay (f (PDataMap (B.UnMapData #£ x)))
        £ pdelay (f (PDataList (B.UnListData #£ x)))
        £ pdelay (f (PDataInteger (B.UnIData #£ x)))
        £ pdelay (f (PDataByteString (B.UnBData #£ x)))

instance ListElemUni PData where
  type ListElemType PData = PLC.Data
  listElemUni Proxy = PLC.DefaultUniData

instance PEq PData where
  a £== b = B.EqualsData #£ a £ b
