{-# OPTIONS_GHC -Wno-orphans #-}

-- Mirrors the equivalent V1 module in plutus-ledger-api
module Plutarch.LedgerApi.V1.Tx (
  PTxId (..),
  PTxOutRef (..),
) where

import Plutarch.Builtin (pasConstr)
import Plutarch.DataRepr (PDataFields)
import Plutarch.Internal.Lift (DeriveDataPLiftable)
import Plutarch.LedgerApi.Utils (Mret)
import Plutarch.Prelude
import Plutarch.TryFrom (PTryFrom (PTryFromExcess, ptryFrom'))
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V1 qualified as Plutus

{- | Hashed with @BLAKE2b-256@.

@since 3.1.0
-}
newtype PTxId (s :: S) = PTxId (Term s (PDataRecord '["_0" ':= PByteString]))
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PPartialOrd
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PShow
    )

-- | @since 3.1.0
instance DerivePlutusType PTxId where
  type DPTStrat _ = PlutusTypeData

-- | @since WIP
deriving via
  DeriveDataPLiftable PTxId Plutus.TxId
  instance
    PLiftable PTxId

-- | @since 3.1.0
instance PTryFrom PData PTxId where
  type PTryFromExcess PData PTxId = Mret PTxId
  ptryFrom' opq = runTermCont $ do
    unwrapped <- tcont . plet $ pasConstr # opq
    let ix = pfstBuiltin # unwrapped
    tcont $ \f ->
      pif
        (ix #== 0)
        (f ())
        (ptraceInfoError "ptryFrom(PTxId): constructor index must be 0")
    let inner = psndBuiltin # unwrapped
    let h = phead # inner
    let t = ptail # inner
    tcont $ \f ->
      pif
        (pnull # t)
        (f ())
        (ptraceInfoError "ptryFrom(PTxId): must have exactly 1 field")
    unwrapped' <- tcont . plet $ ptryFrom @(PAsData PByteString) h snd
    tcont $ \f ->
      pif
        (plengthBS # unwrapped' #== 32)
        (f ())
        (ptraceInfoError "ptryFrom(PTxId): must be 32 bytes long")
    pure (punsafeCoerce opq, pcon . PTxId $ pdcons # pdata unwrapped' # pdnil)

-- | @since 3.1.0
instance PTryFrom PData (PAsData PTxId) where
  type PTryFromExcess PData (PAsData PTxId) = Mret PTxId
  ptryFrom' opq = runTermCont $ do
    unwrapped <- tcont . plet $ pasConstr # opq
    let ix = pfstBuiltin # unwrapped
    tcont $ \f ->
      pif
        (ix #== 0)
        (f ())
        (ptraceInfoError "ptryFrom(PTxId): constructor index must be 0")
    let inner = psndBuiltin # unwrapped
    let h = phead # inner
    let t = ptail # inner
    tcont $ \f ->
      pif
        (pnull # t)
        (f ())
        (ptraceInfoError "ptryFrom(PTxId): must have exactly 1 field")
    unwrapped' <- tcont . plet $ ptryFrom @(PAsData PByteString) h snd
    tcont $ \f ->
      pif
        (plengthBS # unwrapped' #== 32)
        (f ())
        (ptraceInfoError "ptryFrom(PTxId): must be 32 bytes long")
    pure (punsafeCoerce opq, pcon . PTxId $ pdcons # pdata unwrapped' # pdnil)

{- | Reference to a transaction output, with an index referencing which exact
output we mean.

@since 2.0.0
-}
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
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PDataFields
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PPartialOrd
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PTryFrom PData
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType PTxOutRef where
  type DPTStrat _ = PlutusTypeData

-- | @since WIP
deriving via
  DeriveDataPLiftable PTxOutRef Plutus.TxOutRef
  instance
    PLiftable PTxOutRef

-- | @since 3.1.0
instance PTryFrom PData (PAsData PTxOutRef)
