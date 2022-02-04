{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Api.V1.Tx (
  PTxOutRef (PTxOutRef),
  PTxOut (PTxOut),
  PTxId (PTxId),
  PTxInInfo (PTxInInfo),
) where

import qualified GHC.Generics as GHC
import Generics.SOP (Generic, I (I))

import qualified Plutus.V1.Ledger.Api as Plutus

import Plutarch.Api.V1.Address (PAddress)
import Plutarch.Api.V1.Maybe (PMaybeData)
import Plutarch.Api.V1.Scripts (PDatumHash)
import Plutarch.Api.V1.Value (PValue)
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
  PIsDataReprInstances (PIsDataReprInstances),
  PLabeledType ((:=)),
 )
import Plutarch.Lift (
  PLifted,
  PUnsafeLiftDecl,
 )
import Plutarch.Prelude

newtype PTxId (s :: S)
  = PTxId (Term s (PDataRecord '["_0" ':= PByteString]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via PIsDataReprInstances PTxId

instance PUnsafeLiftDecl PTxId where type PLifted PTxId = Plutus.TxId
deriving via (DerivePConstantViaData Plutus.TxId PTxId) instance (PConstant Plutus.TxId)

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
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via PIsDataReprInstances PTxOutRef

instance PUnsafeLiftDecl PTxOutRef where type PLifted PTxOutRef = Plutus.TxOutRef
deriving via (DerivePConstantViaData Plutus.TxOutRef PTxOutRef) instance (PConstant Plutus.TxOutRef)

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
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via PIsDataReprInstances PTxInInfo

instance PUnsafeLiftDecl PTxInInfo where type PLifted PTxInInfo = Plutus.TxInInfo
deriving via (DerivePConstantViaData Plutus.TxInInfo PTxInInfo) instance (PConstant Plutus.TxInInfo)

newtype PTxOut (s :: S)
  = PTxOut
      ( Term
          s
          ( PDataRecord
              '[ "address" ':= PAddress
               , "value" ':= PValue
               , "datumHash" ':= PMaybeData PDatumHash
               ]
          )
      )
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData, PDataFields)
    via (PIsDataReprInstances PTxOut)

instance PUnsafeLiftDecl PTxOut where type PLifted PTxOut = Plutus.TxOut
deriving via (DerivePConstantViaData Plutus.TxOut PTxOut) instance (PConstant Plutus.TxOut)
