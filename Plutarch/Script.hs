module Plutarch.Script (Script (..), NamedScript (..), programMapNames, serialiseScript, deserialiseScript) where

import Data.ByteString.Short (ShortByteString)
import GHC.Generics (Generic)
import PlutusLedgerApi.Common (serialiseUPLC, uncheckedDeserialiseUPLC)
import UntypedPlutusCore qualified as UPLC

newtype Script = Script {unScript :: UPLC.Program UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()}
  deriving newtype (Eq)
  deriving stock (Show, Generic)

programMapNames :: (a -> b) -> UPLC.Program a UPLC.DefaultUni UPLC.DefaultFun () -> UPLC.Program b UPLC.DefaultUni UPLC.DefaultFun ()
programMapNames f (UPLC.Program a b c) = UPLC.Program a b (UPLC.termMapNames f c)

serialiseScript :: Script -> ShortByteString
serialiseScript = serialiseUPLC . unScript

deserialiseScript :: ShortByteString -> Script
deserialiseScript = Script . uncheckedDeserialiseUPLC

newtype NamedScript = NamedScript {unNamedScript :: UPLC.Program UPLC.NamedDeBruijn UPLC.DefaultUni UPLC.DefaultFun ()}
  deriving newtype (Eq)
  deriving stock (Show, Generic)
