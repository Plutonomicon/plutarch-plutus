{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Api.V2.Tx (
  PTxOutRef (PTxOutRef),
  PTxOut (PTxOut),
  PTxId (PTxId),
  PTxInInfo (PTxInInfo),
  POutputDatum (POutputDatumHash, PNoOutputDatum, POutputDatum),
) where

import qualified Plutarch.Api.V1.Address as V1
import qualified Plutarch.Api.V1.Maybe as V1
import qualified Plutarch.Api.V1.Scripts as V1
import qualified Plutarch.Api.V1.Value as V1
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
import qualified PlutusLedgerApi.V2 as Plutus

-- | Reference to a transaction output with a index referencing which of the outputs is being referred to.
newtype PTxOutRef (s :: S)
  = PTxOutRef
      ( Term
          s
          ( PDataRecord
              '[ "id" ':= PTxId
               , "idx" ':= PInteger
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq, POrd)

instance DerivePlutusType PTxOutRef where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PTxOutRef where type PLifted PTxOutRef = Plutus.TxOutRef
deriving via (DerivePConstantViaData Plutus.TxOutRef PTxOutRef) instance PConstantDecl Plutus.TxOutRef

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
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PTxOut where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PTxOut where type PLifted PTxOut = Plutus.TxOut
deriving via (DerivePConstantViaData Plutus.TxOut PTxOut) instance PConstantDecl Plutus.TxOut

newtype PTxId (s :: S)
  = PTxId (Term s (PDataRecord '["_0" ':= PByteString]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq, POrd)
instance DerivePlutusType PTxId where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PTxId where type PLifted PTxId = Plutus.TxId
deriving via (DerivePConstantViaData Plutus.TxId PTxId) instance PConstantDecl Plutus.TxId

-- | A input of the pending transaction.
newtype PTxInInfo (s :: S)
  = PTxInInfo
      ( Term
          s
          ( PDataRecord
              '[ "outRef" ':= PTxOutRef
               , "resolved" ':= PTxOut
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PTxInInfo where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PTxInInfo where type PLifted PTxInInfo = Plutus.TxInInfo
deriving via (DerivePConstantViaData Plutus.TxInInfo PTxInInfo) instance PConstantDecl Plutus.TxInInfo

-- | The datum attached to an output: either nothing, a datum hash or an inline datum (CIP 32)
data POutputDatum (s :: S)
  = PNoOutputDatum (Term s (PDataRecord '[]))
  | POutputDatumHash (Term s (PDataRecord '["datumHash" ':= V1.PDatumHash]))
  | POutputDatum (Term s (PDataRecord '["outputDatum" ':= V1.PDatum]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq)

instance DerivePlutusType POutputDatum where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl POutputDatum where type PLifted POutputDatum = Plutus.OutputDatum
deriving via (DerivePConstantViaData Plutus.OutputDatum POutputDatum) instance PConstantDecl Plutus.OutputDatum
