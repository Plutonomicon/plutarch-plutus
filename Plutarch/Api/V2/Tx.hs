{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Api.V2.Tx (
  V1.PTxOutRef (PTxOutRef),
  PTxOut (PTxOut),
  V1.PTxId (PTxId),
  PTxInInfo (PTxInInfo),
  POutputDatum (POutputDatumHash, PNoOutputDatum, POutputDatum),
) where

import Plutarch.Api.V1.Address qualified as V1
import Plutarch.Api.V1.Maybe qualified as V1
import Plutarch.Api.V1.Scripts qualified as V1
import Plutarch.Api.V1.Tx qualified as V1
import Plutarch.Api.V1.Value qualified as V1
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
 )
import Plutarch.Lift (
  PConstantDecl,
  PLifted,
  PUnsafeLiftDecl,
 )
import Plutarch.Prelude
import PlutusLedgerApi.V2 qualified as Plutus

-- | A transaction output. This consists of a target address, value and maybe a datum hash
newtype PTxOut (s :: S)
  = PTxOut
      ( Term
          s
          ( PDataRecord
              '[ "address" ':= V1.PAddress
               , -- negative values may appear in a future Cardano version
                 "value" ':= V1.PValue 'V1.Sorted 'V1.Positive
               , "datum" ':= POutputDatum
               , "referenceScript" ':= V1.PMaybeData V1.PScriptHash
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq, PShow)

instance DerivePlutusType PTxOut where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PTxOut where type PLifted PTxOut = Plutus.TxOut
deriving via (DerivePConstantViaData Plutus.TxOut PTxOut) instance PConstantDecl Plutus.TxOut

-- | A input of the pending transaction.
newtype PTxInInfo (s :: S)
  = PTxInInfo
      ( Term
          s
          ( PDataRecord
              '[ "outRef" ':= V1.PTxOutRef
               , "resolved" ':= PTxOut
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq, PShow)

instance DerivePlutusType PTxInInfo where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PTxInInfo where type PLifted PTxInInfo = Plutus.TxInInfo
deriving via (DerivePConstantViaData Plutus.TxInInfo PTxInInfo) instance PConstantDecl Plutus.TxInInfo

-- | The datum attached to an output: either nothing, a datum hash or an inline datum (CIP 32)
data POutputDatum (s :: S)
  = PNoOutputDatum (Term s (PDataRecord '[]))
  | POutputDatumHash (Term s (PDataRecord '["datumHash" ':= V1.PDatumHash]))
  | POutputDatum (Term s (PDataRecord '["outputDatum" ':= V1.PDatum]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType POutputDatum where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl POutputDatum where type PLifted POutputDatum = Plutus.OutputDatum
deriving via (DerivePConstantViaData Plutus.OutputDatum POutputDatum) instance PConstantDecl Plutus.OutputDatum
