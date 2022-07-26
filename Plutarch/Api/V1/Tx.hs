{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Api.V1.Tx (
  PTxOutRef (PTxOutRef),
  PTxOut (PTxOut),
  PTxId (PTxId),
  PTxInInfo (PTxInInfo),
) where

import qualified PlutusLedgerApi.V1 as Plutus

import Plutarch.Api.V1.Address (PAddress)
import Plutarch.Api.V1.Maybe (PMaybeData)
import Plutarch.Api.V1.Scripts (PDatumHash)
import Plutarch.Api.V1.Value (
  AmountGuarantees (Positive),
  KeyGuarantees (Sorted),
  PValue,
 )
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

newtype PTxId (s :: S)
  = PTxId (Term s (PDataRecord '["_0" ':= PByteString]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq, PPartialOrd, POrd)
instance DerivePlutusType PTxId where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PTxId where type PLifted PTxId = Plutus.TxId
deriving via (DerivePConstantViaData Plutus.TxId PTxId) instance PConstantDecl Plutus.TxId

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
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq, PPartialOrd, POrd)

instance DerivePlutusType PTxOutRef where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PTxOutRef where type PLifted PTxOutRef = Plutus.TxOutRef
deriving via (DerivePConstantViaData Plutus.TxOutRef PTxOutRef) instance PConstantDecl Plutus.TxOutRef

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

-- | A transaction output. This consists of a target address, value and maybe a datum hash
newtype PTxOut (s :: S)
  = PTxOut
      ( Term
          s
          ( PDataRecord
              '[ "address" ':= PAddress
               , "value" ':= PValue 'Sorted 'Positive -- negative values may appear in a future Cardano version
               , "datumHash" ':= PMaybeData PDatumHash
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PTxOut where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PTxOut where type PLifted PTxOut = Plutus.TxOut
deriving via (DerivePConstantViaData Plutus.TxOut PTxOut) instance PConstantDecl Plutus.TxOut
