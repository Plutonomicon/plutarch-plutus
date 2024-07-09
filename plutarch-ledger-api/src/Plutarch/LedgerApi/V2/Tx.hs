{-# OPTIONS_GHC -Wno-orphans #-}

-- Mirrors the equivalent V2 module in plutus-ledger-api
module Plutarch.LedgerApi.V2.Tx (
  POutputDatum (..),
  PTxOut (..),
) where

import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
 )
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Utils (PMaybeData)
import Plutarch.LedgerApi.V1.Address (PAddress)
import Plutarch.LedgerApi.V1.Scripts (PDatum, PDatumHash, PScriptHash)
import Plutarch.LedgerApi.Value qualified as Value
import Plutarch.Lift (
  PConstantDecl,
  PUnsafeLiftDecl (PLifted),
 )
import Plutarch.Prelude
import PlutusLedgerApi.V2 qualified as Plutus

-- | @since 2.0.0
data POutputDatum (s :: S)
  = PNoOutputDatum (Term s (PDataRecord '[]))
  | POutputDatumHash (Term s (PDataRecord '["datumHash" ':= PDatumHash]))
  | -- | Inline datum as per
    -- [CIP-0032](https://github.com/cardano-foundation/CIPs/blob/master/CIP-0032/README.md)
    POutputDatum (Term s (PDataRecord '["outputDatum" ':= PDatum]))
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
      PShow
    , -- | @since 3.1.0
      PTryFrom PData
    )

-- | @since 2.0.0
instance DerivePlutusType POutputDatum where
  type DPTStrat _ = PlutusTypeData

-- | @since 2.0.0
instance PUnsafeLiftDecl POutputDatum where
  type PLifted POutputDatum = Plutus.OutputDatum

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData Plutus.OutputDatum POutputDatum)
  instance
    PConstantDecl Plutus.OutputDatum

-- | @since 3.1.0
instance PTryFrom PData (PAsData POutputDatum)

-- | @since 2.0.0
newtype PTxOut (s :: S)
  = PTxOut
      ( Term
          s
          ( PDataRecord
              '[ "address" ':= PAddress
               , "value" ':= Value.PValue 'AssocMap.Unsorted 'Value.NoGuarantees
               , "datum" ':= POutputDatum
               , "referenceScript" ':= PMaybeData PScriptHash
               ]
          )
      )
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
      PDataFields
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PShow
    , -- | @since 3.1.0
      PTryFrom PData
    )

-- | @since 2.0.0
instance DerivePlutusType PTxOut where
  type DPTStrat _ = PlutusTypeData

-- | @since 2.0.0
instance PUnsafeLiftDecl PTxOut where
  type PLifted PTxOut = Plutus.TxOut

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData Plutus.TxOut PTxOut)
  instance
    PConstantDecl Plutus.TxOut

-- | @since 3.1.0
instance PTryFrom PData (PAsData PTxOut)
