module Plutarch.Builtin.List.Type (
  PList (..),
  ListElemUni (..),
) where

import Data.Proxy
import Plutarch
import Plutarch.Integer (PInteger)
import Plutarch.Prelude
import qualified PlutusCore as PLC
import qualified PlutusCore.Data as PLC

-- | A builtin list of `Data`.
data PList a s
  = PNil
  | PCons (Term s a) (Term s (PList a))

class ListElemUni (a :: k -> Type) where
  type ListElemType a :: Type
  listElemUni :: Proxy a -> PLC.DefaultUni (PLC.Esc (ListElemType a))

instance ListElemUni PInteger where
  type ListElemType PInteger = Integer
  listElemUni Proxy = PLC.DefaultUniInteger

instance ListElemUni POpaque where
  type ListElemType POpaque = PLC.Data
  listElemUni Proxy = PLC.DefaultUniData
