{-# OPTIONS_GHC -Wno-orphans #-}

-- Mirrors the equivalent V4 module in plutus-ledger-api
module Plutarch.LedgerApi.V4.Address (
  PAccountId (..),
  PAddress (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.Utils (PMaybeData)
import Plutarch.LedgerApi.V1.Credential (PCredential)
import Plutarch.Prelude
import PlutusLedgerApi.V4 qualified as Plutus

-- | @since 3.8.0
newtype PAccountId (s :: S) = PAccountId (Term s PCredential)
  deriving stock
    ( -- | @since 3.8.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.8.0
      SOP.Generic
    , -- | @since 3.8.0
      PIsData
    , -- | @since 3.8.0
      PEq
    , -- | @since 3.8.0
      PShow
    )
  deriving
    ( -- | @since 3.8.0
      PlutusType
    )
    via (DeriveNewtypePlutusType PAccountId)
  deriving
    ( -- | @since 3.8.0
      PValidateData
    )
    via (DeriveNewtypePValidateData PAccountId PCredential)

-- | @since 3.8.0
deriving via
  DeriveDataPLiftable PAccountId Plutus.AccountId
  instance
    PLiftable PAccountId

-- | @since 3.8.0
instance PTryFrom PData (PAsData PAccountId)

-- | @since 3.8.0
data PAddress (s :: S) = PAddress
  { paddress'credential :: Term s (PAsData PCredential)
  , paddress'stakingAccountId :: Term s (PMaybeData PAccountId)
  }
  deriving stock
    ( -- | @since 3.8.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.8.0
      SOP.Generic
    , -- | @since 3.8.0
      PIsData
    , -- | @since 3.8.0
      PEq
    , -- | @since 3.8.0
      PShow
    )
  deriving
    ( -- | @since 3.8.0
      PlutusType
    , -- | @since 3.8.0
      PValidateData
    )
    via (DeriveAsDataStruct PAddress)

-- | @since 3.8.0
deriving via
  DeriveDataPLiftable PAddress Plutus.Address
  instance
    PLiftable PAddress

-- | @since 3.8.0
instance PTryFrom PData (PAsData PAddress)
