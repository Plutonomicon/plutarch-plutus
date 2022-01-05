{-# LANGUAGE UndecidableInstances #-}

-- Must correspond to V1 of Plutus.
-- See https://staging.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Api.html

{- |

  Plutus V1 'ScriptContext' and 'TxInfo' encoded as 'PDataList',
  matching the 'ToData'/'FromData' representation.
-}
module Plutarch.ScriptContext (PScriptContext (..), PScriptPurpose (..), PTxInfo (..)) where

import Plutarch (PMatch, POpaque)
import Plutarch.Builtin (PBuiltinList, PIsData)
import Plutarch.DataRepr (DataReprHandlers (DRHCons, DRHNil), PDataList, PIsDataRepr, PIsDataReprInstances (PIsDataReprInstances), PIsDataReprRepr, pmatchDataRepr, pmatchRepr)
import Plutarch.Lift
import Plutarch.Prelude
import qualified Plutus.V1.Ledger.Api as Ledger

-- | Tag for 'TxInInfo'
data PTxInInfo s

-- | Tag for 'TxOut'
data PTxOut s

-- | Tag for 'Value'
data PValue s

-- | 'TxInfo', encoded using 'PDataList'
data PTxInfo s
  = PTxInfo
      ( Term
          s
          ( PDataList
              '[ PBuiltinList PTxInInfo
               , PBuiltinList PTxOut
               , PValue -- fee
               , PValue -- mint
               , PBuiltinList POpaque -- dcert
               , POpaque -- withdrawals
               , POpaque -- range
               , PBuiltinList POpaque -- signatures
               , POpaque -- data map
               , POpaque -- tx id
               ]
          )
      )

-- | 'ScriptPurpose' encoded as a Sum of 'PDataList' values
data PScriptPurpose s
  = PMinting (Term s (PDataList '[POpaque]))
  | PSpending (Term s (PDataList '[POpaque]))
  | PRewarding (Term s (PDataList '[POpaque]))
  | PCertifying (Term s (PDataList '[POpaque]))
  deriving (PMatch, PIsData, PLift) via (PIsDataReprInstances PScriptPurpose Ledger.ScriptPurpose)

instance PIsDataRepr PScriptPurpose where
  type PIsDataReprRepr PScriptPurpose = '[ '[POpaque], '[POpaque], '[POpaque], '[POpaque]]
  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PMinting) $ DRHCons (f . PSpending) $ DRHCons (f . PRewarding) $ DRHCons (f . PCertifying) DRHNil

data PScriptContext s = PScriptContext (Term s (PDataList '[PTxInfo, PScriptPurpose]))
  deriving (PMatch, PIsData, PLift) via (PIsDataReprInstances PScriptContext Ledger.ScriptContext)

instance PIsDataRepr PScriptContext where
  type PIsDataReprRepr PScriptContext = '[ '[PTxInfo, PScriptPurpose]]
  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PScriptContext) DRHNil
