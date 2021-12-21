module Plutarch.Builtin.Data.Type (
  PData (..),
) where

import Plutarch
import Plutarch.Builtin.List.Type
import Plutarch.Builtin.Pair.Type
import Plutarch.ByteString (PByteString)
import Plutarch.Integer (PInteger)

data PData s
  = PDataConstr (Term s (PPair PInteger (PList PData)))
  | PDataMap (Term s (PList (PPair PData PData)))
  | PDataList (Term s (PList PData))
  | PDataInteger (Term s PInteger)
  | PDataByteString (Term s PByteString)
