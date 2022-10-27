{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Api.V1.Tx (
  PTxOutRef (PTxOutRef),
  PTxOut (PTxOut),
  PTxId (PTxId),
  PTxInInfo (PTxInInfo),
) where

import PlutusLedgerApi.V1 qualified as Plutus

import Data.Bifunctor (first)
import Plutarch.Api.V1.Address (PAddress)
import Plutarch.Api.V1.Maybe (PMaybeData)
import Plutarch.Api.V1.Scripts (PDatumHash)
import Plutarch.Api.V1.Value (
  AmountGuarantees (Positive),
  KeyGuarantees (Sorted),
  PValue,
 )
import Plutarch.Builtin (pasConstr)
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
import Plutarch.TryFrom (PTryFrom (PTryFromExcess, ptryFrom'))
import Plutarch.Unsafe (punsafeCoerce)

newtype Flip f a b = Flip (f b a) deriving stock (Generic)

newtype PTxId (s :: S)
  = PTxId (Term s (PDataRecord '["_0" ':= PByteString]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq, PPartialOrd, POrd, PShow)
instance DerivePlutusType PTxId where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PTxId where type PLifted PTxId = Plutus.TxId
deriving via (DerivePConstantViaData Plutus.TxId PTxId) instance PConstantDecl Plutus.TxId

instance PTryFrom PData PTxId where
  type PTryFromExcess PData PTxId = Flip Term PByteString
  ptryFrom' opq cont = ptryFrom @(PAsData PTxId) opq (cont . first punsafeCoerce)

instance PTryFrom PData (PAsData PTxId) where
  type PTryFromExcess PData (PAsData PTxId) = Flip Term PByteString
  ptryFrom' opq = runTermCont $ do
    opq' <- tcont . plet $ pasConstr # opq
    tcont $ \f ->
      pif (pfstBuiltin # opq' #== 0) (f ()) $ ptraceError "ptryFrom(TxId): invalid constructor id"
    flds <- tcont . plet $ psndBuiltin # opq'
    let dataBs = phead # flds
    tcont $ \f ->
      pif (pnil #== ptail # flds) (f ()) $ ptraceError "ptryFrom(TxId): constructor fields len > 1"
    unwrapped <- tcont . plet $ ptryFrom @(PAsData PByteString) dataBs snd
    tcont $ \f ->
      pif (plengthBS # unwrapped #== 32) (f ()) $ ptraceError "ptryFrom(TxId): must be 32 bytes long"
    pure (punsafeCoerce opq, unwrapped)

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
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq, PPartialOrd, POrd, PTryFrom PData, PShow)

instance DerivePlutusType PTxOutRef where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PTxOutRef where type PLifted PTxOutRef = Plutus.TxOutRef
deriving via (DerivePConstantViaData Plutus.TxOutRef PTxOutRef) instance PConstantDecl Plutus.TxOutRef
instance PTryFrom PData (PAsData PTxOutRef)

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
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq, PShow)

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
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq, PShow)

instance DerivePlutusType PTxOut where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PTxOut where type PLifted PTxOut = Plutus.TxOut
deriving via (DerivePConstantViaData Plutus.TxOut PTxOut) instance PConstantDecl Plutus.TxOut
