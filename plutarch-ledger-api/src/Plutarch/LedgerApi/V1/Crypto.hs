{-# OPTIONS_GHC -Wno-orphans #-}

-- Mirrors the equivalent V1 module in plutus-ledger-api
module Plutarch.LedgerApi.V1.Crypto (
  PPubKeyHash (..),
) where

import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Prelude
import PlutusLedgerApi.V1 qualified as Plutus
import PlutusTx.Builtins.Internal qualified as PlutusTx

-- | @since 2.0.0
newtype PPubKeyHash (s :: S) = PPubKeyHash (Term s PByteString)
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
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PShow
    )
  deriving
    ( -- | @since WIP
      PlutusType
    )
    via (DeriveNewtypePlutusType PPubKeyHash)

-- | @since WIP
instance PLiftable PPubKeyHash where
  type AsHaskell PPubKeyHash = Plutus.PubKeyHash
  type PlutusRepr PPubKeyHash = ByteString
  {-# INLINEABLE haskToRepr #-}
  haskToRepr (Plutus.PubKeyHash (PlutusTx.BuiltinByteString str)) = str
  {-# INLINEABLE reprToHask #-}
  reprToHask = Right . Plutus.PubKeyHash . PlutusTx.BuiltinByteString
  {-# INLINEABLE reprToPlut #-}
  reprToPlut = reprToPlutUni
  {-# INLINEABLE plutToRepr #-}
  plutToRepr = plutToReprUni
