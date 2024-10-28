module Plutarch.Script (Script (..), serialiseScript, deserialiseScript) where

import Data.ByteString.Short (ShortByteString)
import GHC.Generics (Generic)
import PlutusLedgerApi.Common (serialiseUPLC, uncheckedDeserialiseUPLC)
import UntypedPlutusCore qualified as UPLC

newtype Script = Script {unScript :: UPLC.Program UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()}
  deriving newtype (Eq)
  deriving stock (Show, Generic)

serialiseScript :: Script -> ShortByteString
serialiseScript = serialiseUPLC . unScript

deserialiseScript :: ShortByteString -> Script
deserialiseScript = Script . uncheckedDeserialiseUPLC
