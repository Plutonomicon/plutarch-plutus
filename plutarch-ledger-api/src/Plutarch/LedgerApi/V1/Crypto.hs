{-# OPTIONS_GHC -Wno-orphans #-}

-- Mirrors the equivalent V1 module in plutus-ledger-api
module Plutarch.LedgerApi.V1.Crypto (
  PPubKeyHash (..),
) where

import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Internal.Parse (
  DeriveNewtypePValidateData,
  PValidateData,
 )
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
    ( -- | @since 3.3.0
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
    ( -- | @since 3.3.0
      PlutusType
    )
    via (DeriveNewtypePlutusType PPubKeyHash)
  deriving
    ( -- | @since 3.5.0
      PValidateData
    )
    via (DeriveNewtypePValidateData PPubKeyHash PByteString)

-- | @since 3.3.0
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

-- | @since 3.4.0
instance PTryFrom PData (PAsData PPubKeyHash)
