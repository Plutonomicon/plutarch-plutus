{-# OPTIONS_GHC -Wno-orphans #-}

-- Mirrors the equivalent V1 module in plutus-ledger-api
module Plutarch.LedgerApi.V1.Crypto (
  PPubKeyHash (..),
) where

import GHC.Generics (Generic)
import Plutarch.LedgerApi.Utils (Mret)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V1 qualified as Plutus

-- | @since 2.0.0
newtype PPubKeyHash (s :: S) = PPubKeyHash (Term s (PDataNewtype PByteString))
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
      POrd
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType PPubKeyHash where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
deriving via
  DeriveDataPLiftable PPubKeyHash Plutus.PubKeyHash
  instance
    PLiftable PPubKeyHash
