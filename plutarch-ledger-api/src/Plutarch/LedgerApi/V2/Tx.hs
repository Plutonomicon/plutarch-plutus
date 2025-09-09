{-# OPTIONS_GHC -Wno-orphans #-}

-- Mirrors the equivalent V2 module in plutus-ledger-api
module Plutarch.LedgerApi.V2.Tx (
  POutputDatum (..),
  PTxOut (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Utils (PMaybeData)
import Plutarch.LedgerApi.V1.Address (PAddress)
import Plutarch.LedgerApi.V1.Scripts (PDatum, PDatumHash, PScriptHash)
import Plutarch.LedgerApi.Value qualified as Value
import Plutarch.Prelude
import PlutusLedgerApi.V2 qualified as Plutus

-- | @since 2.0.0
data POutputDatum (s :: S)
  = PNoOutputDatum
  | POutputDatumHash
      { poutputDatum'datumHash :: Term s (PAsData PDatumHash)
      }
  | -- | Inline datum as per
    -- [CIP-0032](https://github.com/cardano-foundation/CIPs/blob/master/CIP-0032/README.md)
    POutputDatum
      { poutputDatum'outputDatum :: Term s PDatum
      }
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      SOP.Generic
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PShow
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    )
    via (DeriveAsDataStruct POutputDatum)

-- | @since 3.3.0
deriving via
  DeriveDataPLiftable POutputDatum Plutus.OutputDatum
  instance
    PLiftable POutputDatum

-- | @since 3.4.0
instance PTryFrom PData (PAsData POutputDatum)

-- | @since 2.0.0
data PTxOut (s :: S) = PTxOut
  { ptxOut'address :: Term s PAddress
  , ptxOut'value :: Term s (PAsData (Value.PValue 'AssocMap.Sorted 'Value.Positive))
  , ptxOut'datum :: Term s POutputDatum
  , ptxOut'referenceScript :: Term s (PMaybeData PScriptHash)
  }
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      SOP.Generic
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PShow
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    )
    via (DeriveAsDataStruct PTxOut)

-- | @since 3.3.0
deriving via
  DeriveDataPLiftable PTxOut Plutus.TxOut
  instance
    PLiftable PTxOut

-- | @since 3.4.0
instance PTryFrom PData (PAsData PTxOut)
