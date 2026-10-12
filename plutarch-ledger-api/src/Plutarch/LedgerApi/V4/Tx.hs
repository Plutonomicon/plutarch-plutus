{-# OPTIONS_GHC -Wno-orphans #-}

-- Mirrors the equivalent V4 module in plutus-ledger-api
module Plutarch.LedgerApi.V4.Tx (
  PTxOut (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.Utils (PMaybeData)
import Plutarch.LedgerApi.V1.Scripts (PScriptHash)
import Plutarch.LedgerApi.V2.Tx (POutputDatum)
import Plutarch.LedgerApi.V4.Address (PAddress)
import Plutarch.LedgerApi.Value (PLedgerValue)
import Plutarch.Prelude
import PlutusLedgerApi.V4 qualified as Plutus

-- | @since 3.8.0
data PTxOut (s :: S) = PTxOut
  { ptxOut'address :: Term s (PAsData PAddress)
  , ptxOut'value :: Term s (PAsData PLedgerValue)
  , ptxOut'datum :: Term s (PAsData POutputDatum)
  , ptxOut'referenceScript :: Term s (PMaybeData PScriptHash)
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
    via (DeriveAsDataStruct PTxOut)

-- | @since 3.8.0
deriving via
  DeriveDataPLiftable PTxOut Plutus.TxOut
  instance
    PLiftable PTxOut

-- | @since 3.8.0
instance PTryFrom PData (PAsData PTxOut)
