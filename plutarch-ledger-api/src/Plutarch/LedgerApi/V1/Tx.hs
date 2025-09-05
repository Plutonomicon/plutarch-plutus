{-# OPTIONS_GHC -Wno-orphans #-}

-- Mirrors the equivalent V1 module in plutus-ledger-api
module Plutarch.LedgerApi.V1.Tx (
  PTxId (..),
  PTxOutRef (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Prelude
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

-- | @since 3.4.0
instance PTryFrom PData (PAsData PTxId)

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

-- | @since 3.4.0
instance PTryFrom PData (PAsData PTxOutRef)
