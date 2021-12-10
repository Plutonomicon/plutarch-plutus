module Plutarch.ScriptContext (PScriptContext(..)) where

import Plutarch.Prelude
--import Plutarch (PlutusType(PInner, pCon', pMatch'))
import Plutarch.Opaque (POpaque)

data PScriptContext = PScriptContext
  { txInfo :: Term POpaque
  , purpose :: Term POpaque
  }

-- instance PlutusType PScriptContext where
