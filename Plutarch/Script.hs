module Plutarch.Script (Script (..), serialiseScript) where

import Data.ByteString.Short (ShortByteString)
import PlutusLedgerApi.Common (serialiseUPLC)
import UntypedPlutusCore qualified as UPLC

newtype Script = Script {unScript :: UPLC.Program UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()}
  deriving newtype (Eq)
  deriving stock (Show)

serialiseScript :: Script -> ShortByteString
serialiseScript = serialiseUPLC . unScript
