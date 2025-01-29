{-# OPTIONS_GHC -Wno-orphans #-}

-- Mirrors the equivalent V1 module in plutus-ledger-api
module Plutarch.LedgerApi.V1.Address (
  PAddress (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.Utils (PMaybeData)
import Plutarch.LedgerApi.V1.Credential (PCredential, PStakingCredential)
import Plutarch.Prelude
import Plutarch.Repr.Data (DeriveAsDataStruct (DeriveAsDataStruct))
import PlutusLedgerApi.V1 qualified as Plutus

-- | @since 2.0.0
data PAddress (s :: S) = PAddress
  { paddress'credential :: Term s PCredential
  , paddress'stakingCredential :: Term s (PMaybeData PStakingCredential)
  }
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since WIP
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
    ( -- | @since WIP
      PlutusType
    )
    via (DeriveAsDataStruct PAddress)

-- | @since WIP
deriving via
  DeriveDataPLiftable PAddress Plutus.Address
  instance
    PLiftable PAddress
