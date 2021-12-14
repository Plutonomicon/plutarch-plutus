module Plutarch.ScriptContext (PScriptContext (..)) where

import Plutarch.Prelude

--import Plutarch (PlutusType(PInner, pcon', pmatch'))
import Plutarch (POpaque)

data PScriptContext s = PScriptContext
  { txInfo :: Term s POpaque
  , purpose :: Term s POpaque
  }

-- instance PlutusType PScriptContext where
