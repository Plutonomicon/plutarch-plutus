module Plutarch.Script (Script(..)) where

import qualified UntypedPlutusCore as UPLC

newtype Script = Script { unScript :: UPLC.Program UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun () }
