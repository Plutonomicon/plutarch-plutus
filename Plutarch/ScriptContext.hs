module Plutarch.ScriptContext (PScriptContext (..), PScriptPurpose, PTxInfo) where

import Plutarch (PlutusType (PInner, pcon', pmatch'))
import Plutarch.BuiltinHList (PBuiltinHList)
import Plutarch.DataRepr (PDataRepr)

data PTxInfo s

data PScriptPurpose s

{-
newtype PScriptPurpose s = PScriptPurpose (
    PDataRepr
      '[
        '[ PCurrencySymbol -- minting
        ]
        '[ PTxOutRef -- spending
        ]
        '[ PStakingCredential -- rewarding
        ]
        '[ DCert -- certifying
        ]
      ]
      s
  )
  -}

newtype PScriptContext s = PScriptContext (PBuiltinHList '[PTxInfo, PScriptPurpose] s)

instance PlutusType PScriptContext where
  type PInner PScriptContext _ = PDataRepr '[ '[PTxInfo, PScriptPurpose]]
  pcon' _ = error "FIXME"
  pmatch' _ _ = error "FIXME"
