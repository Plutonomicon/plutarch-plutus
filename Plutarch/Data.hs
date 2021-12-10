module Plutarch.Data (PData(..)) where

import Plutarch.Prelude
import Plutarch.Opaque (POpaque)
import Plutarch.Integer (PInteger)
--import Plutarch (PlutusType(PInner, pCon', pMatch'))
--import qualified PlutusCore as PLC

data PData
  = PDataConstr (Term PInteger) (Term POpaque)
  | PDataMap (Term POpaque)
  | PDataList (Term POpaque)
  | PDataInteger (Term PInteger)
  | PDataByteString (Term POpaque)

-- instance PlutusType PData where
