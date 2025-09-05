{-# OPTIONS_GHC -Wno-orphans #-}

-- Mirrors the equivalent V1 module in plutus-ledger api
module Plutarch.LedgerApi.V1.Credential (
  PCredential (..),
  PStakingCredential (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.V1.Crypto (PPubKeyHash)
import Plutarch.LedgerApi.V1.Scripts (PScriptHash)
import Plutarch.Prelude
import PlutusLedgerApi.V1 qualified as Plutus

-- | @since 2.0.0
data PCredential (s :: S)
  = PPubKeyCredential (Term s (PAsData PPubKeyHash))
  | PScriptCredential (Term s (PAsData PScriptHash))
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
    via (DeriveAsDataStruct PCredential)

-- | @since 3.3.0
deriving via
  DeriveDataPLiftable PCredential Plutus.Credential
  instance
    PLiftable PCredential

-- | @since 3.4.0
instance PTryFrom PData (PAsData PCredential)

-- | @since 2.0.0
data PStakingCredential (s :: S)
  = PStakingHash (Term s PCredential)
  | PStakingPtr (Term s (PAsData PInteger)) (Term s (PAsData PInteger)) (Term s (PAsData PInteger))
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
    via (DeriveAsDataStruct PStakingCredential)

-- | @since 3.3.0
deriving via
  DeriveDataPLiftable PStakingCredential Plutus.StakingCredential
  instance
    PLiftable PStakingCredential

-- | @since 3.4.0
instance PTryFrom PData (PAsData PStakingCredential)
