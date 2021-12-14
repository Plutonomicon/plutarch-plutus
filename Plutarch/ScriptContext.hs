module Plutarch.ScriptContext () where

{-
import Plutarch (POpaque)

newtype PTxInfo s = PTxInfo (Term s POpaque)

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

newtype PScriptContext s = PScriptContext (
    PDataRepr
      '[
        '[
          PTxInfo
          PScriptPurpose
        ]
      ]
      s
  )

--import Plutarch.Prelude

--import Plutarch (PlutusType(PInner, pcon', pmatch'))
--import Plutarch (POpaque)

--data PScriptContext s = PScriptContext
--  { txInfo :: Term s POpaque
--  , purpose :: Term s POpaque
--  }

-- type PScriptContext = PBuiltinHList '[PTxInfo, PScriptPurpose]

{-
data PScriptPurpose (s :: k) = PMinting (Term s (PBuiltinHList '[POpaque])) | PSpending (Term s (PBuiltinHList '[PTxOutRef]))
instance PlutusType PScriptPurpose where
    PInner PScriptPurpose _ = PData
    pcon' = undefined
    pmatch' p f = pif (0 £== (pfst $ punsafeBuiltin PLC.UnConstrData £ p))
      (f (PMinting $ psnd $ punsafeBuiltin PLC.UnConstrData £ p))
      ---
 -}
 -}
