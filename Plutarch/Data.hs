module Plutarch.Data (PData(..)) where

import Plutarch.Prelude
import Plutarch.Opaque (POpaque)
import Plutarch.Integer (PInteger)
--import Plutarch (PlutusType(PInner, pCon', pMatch'))
--import qualified PlutusCore as PLC

data PData s
  = PDataConstr (Term s PInteger) (Term s POpaque)
  | PDataMap (Term s POpaque)
  | PDataList (Term s POpaque)
  | PDataInteger (Term s PInteger)
  | PDataByteString (Term s POpaque)

-- instance PlutusType PData where
