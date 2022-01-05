{-# LANGUAGE DerivingVia #-}

-- Must correspond to V1 of Plutus.
-- See https://staging.plutus.iohkdev.io/doc/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Api.html
module Plutarch.ScriptContext (PScriptContext (..), PScriptPurpose (..), PTxInfo (..)) where

import Plutarch (PMatch, POpaque, punsafeCoerce)
import Plutarch.Builtin (PBuiltinList, PData, PIsData)
import Plutarch.DataRepr (DataReprHandlers (DRHCons, DRHNil), PDataList, PIsDataRepr, PIsDataReprInstances (PIsDataReprInstances), PIsDataReprRepr, pmatchDataRepr, pmatchRepr)
import Plutarch.Lift
import Plutarch.Prelude
import qualified Plutus.V1.Ledger.Api as Ledger

data PTxInInfo s

data PTxOut s

data PValue s

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

data PScriptPurpose s
  = PMinting (Term s (PDataList '[POpaque]))
  | PSpending (Term s (PDataList '[POpaque]))
  | PRewarding (Term s (PDataList '[POpaque]))
  | PCertifying (Term s (PDataList '[POpaque]))
  deriving (PMatch, PIsData) via (PIsDataReprInstances PScriptPurpose)

instance {-# OVERLAPPING #-} PLift PScriptPurpose Ledger.ScriptPurpose where
  pconstant = punsafeCoerce . pconstant @PData . Ledger.toData
  plift' t = do
    h <- plift' @PData (punsafeCoerce t)
    maybeToRight "Failed to decode data" $ Ledger.fromData h
    where
      maybeToRight e = maybe (Left e) Right

instance PIsDataRepr PScriptPurpose where
  type PIsDataReprRepr PScriptPurpose = '[ '[POpaque], '[POpaque], '[POpaque], '[POpaque]]
  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PMinting) $ DRHCons (f . PSpending) $ DRHCons (f . PRewarding) $ DRHCons (f . PCertifying) DRHNil

data PScriptContext s = PScriptContext (Term s (PDataList '[PTxInfo, PScriptPurpose]))
  deriving (PMatch, PIsData) via (PIsDataReprInstances PScriptContext)

instance PIsDataRepr PScriptContext where
  type PIsDataReprRepr PScriptContext = '[ '[PTxInfo, PScriptPurpose]]
  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PScriptContext) DRHNil
