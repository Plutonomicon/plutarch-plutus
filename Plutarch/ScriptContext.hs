module Plutarch.ScriptContext () where

import Plutarch.Prelude
import Plutarch.DataRepr (PDataRepr, )
import Plutarch.BuiltinHList (PBuiltinHList, pmatchDataRepr, DataReprHandlers(DRHNil, DRHCons))
import Plutarch (PlutusType(pcon', pmatch', PInner))

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
  type PInner PScriptContext _ = PDataRepr '[ '[ PTxInfo, PScriptPurpose ] ]
  pcon' _ = error "FIXME"
  pmatch' _ _ = error "FIXME"
