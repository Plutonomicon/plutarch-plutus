module Plutarch.Builtin.List.Type (
  PList (..),
  ListElemUni (..),
) where

import qualified Data.ByteString as BS
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Plutarch (POpaque, Term)
import Plutarch.Bool (PBool)
import Plutarch.ByteString (PByteString)
import Plutarch.Integer (PInteger)
import Plutarch.Prelude (Type)
import Plutarch.String (PString)
import Plutarch.Unit (PUnit)
import qualified PlutusCore as PLC
import qualified PlutusCore.Data as PLC

data PList a s
  = PNil
  | PCons (Term s a) (Term s (PList a))

class ListElemUni (a :: k -> Type) where
  type ListElemType a :: Type
  listElemUni :: Proxy a -> PLC.DefaultUni (PLC.Esc (ListElemType a))

instance ListElemUni POpaque where
  type ListElemType POpaque = PLC.Data
  listElemUni Proxy = PLC.DefaultUniData
instance ListElemUni PInteger where
  type ListElemType PInteger = Integer
  listElemUni Proxy = PLC.DefaultUniInteger

instance ListElemUni PString where
  type ListElemType PString = Text
  listElemUni Proxy = PLC.DefaultUniString

instance ListElemUni PByteString where
  type ListElemType PByteString = BS.ByteString
  listElemUni Proxy = PLC.DefaultUniByteString

instance ListElemUni PUnit where
  type ListElemType PUnit = ()
  listElemUni Proxy = PLC.DefaultUniUnit

instance ListElemUni PBool where
  type ListElemType PBool = Bool
  listElemUni Proxy = PLC.DefaultUniBool
