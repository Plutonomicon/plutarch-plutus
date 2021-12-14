module Plutarch.Data (PData (..)) where

import Plutarch (POpaque)
import Plutarch.Integer (PInteger)
import Plutarch.Prelude

--import Plutarch (PlutusType(PInner, pcon', pmatch'))
--import qualified PlutusCore as PLC

data PData s
  = PDataConstr (Term s PInteger) (Term s POpaque)
  | PDataMap (Term s POpaque)
  | PDataList (Term s POpaque)
  | PDataInteger (Term s PInteger)
  | PDataByteString (Term s POpaque)

-- instance PlutusType PData where
