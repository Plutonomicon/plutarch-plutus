{-# OPTIONS_GHC -Wno-orphans #-}

-- Mirrors the equivalent V1 module in plutus-ledger-api
module Plutarch.LedgerApi.V1.Tx (
  PTxId (..),
  PTxOutRef (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.Utils (Mret)
import Plutarch.Prelude
import Plutarch.Repr.Data (DeriveAsDataStruct (DeriveAsDataStruct))
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V1 qualified as Plutus

{- | Hashed with @BLAKE2b-256@.

@since 3.1.0
-}
newtype PTxId (s :: S) = PTxId (Term s (PAsData PByteString))
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.3.0
      SOP.Generic
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PEq
    , -- , -- | @since 2.0.0
      --   POrd

      -- | @since 2.0.0
      PShow
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    )
    via (DeriveAsDataStruct PTxId)

-- | @since 3.3.0
deriving via
  DeriveDataPLiftable PTxId Plutus.TxId
  instance
    PLiftable PTxId

-- | @since 3.3.1
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

-- | @since 3.3.1
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
data PTxOutRef (s :: S) = PTxOutRef
  { ptxOutRef'id :: Term s PTxId
  , ptxOutRef'idx :: Term s (PAsData PInteger)
  }
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.3.0
      SOP.Generic
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PEq
    , -- , -- | @since 2.0.0
      --   POrd

      -- | @since 2.0.0
      PShow
    , -- | @since 3.3.1
      PTryFrom PData
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    )
    via (DeriveAsDataStruct PTxOutRef)

-- | @since 3.3.0
deriving via
  DeriveDataPLiftable PTxOutRef Plutus.TxOutRef
  instance
    PLiftable PTxOutRef

-- | @since 3.3.1
instance PTryFrom PData (PAsData PTxOutRef)
