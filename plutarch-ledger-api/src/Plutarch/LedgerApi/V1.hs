{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.LedgerApi.V1 (
  Tx.PTxId (..),
  PTxOut (..),
  PTxInInfo (..),
  Tx.PTxOutRef (..),
) where

import Plutarch.DataRepr (DerivePConstantViaData (DerivePConstantViaData))
import Plutarch.LedgerApi.Address (PAddress)
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Scripts (PDatumHash)
import Plutarch.LedgerApi.V1.Tx qualified as Tx
import Plutarch.LedgerApi.Value (PValue)
import Plutarch.LedgerApi.Value qualified as Value
import Plutarch.Lift (
  PConstantDecl,
  PUnsafeLiftDecl (PLifted),
 )
import Plutarch.Prelude
import PlutusLedgerApi.V1 qualified as Plutus

-- | @since 3.1.1
newtype PTxOut (s :: S)
  = PTxOut
      ( Term
          s
          ( PDataRecord
              '[ "address" ':= PAddress
               , "value" ':= PValue 'AssocMap.Sorted 'Value.Positive
               , "datumHash" ':= PDatumHash
               ]
          )
      )
  deriving stock
    ( -- | @since 3.1.1
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.1
      PlutusType
    , -- | @since 3.1.1
      PIsData
    , -- | @since 3.1.1
      PEq
    , -- | @since 3.1.1
      PShow
    , -- | @since 3.1.1
      PTryFrom PData
    )

-- | @since 3.1.1
instance DerivePlutusType PTxOut where
  type DPTStrat _ = PlutusTypeData

-- | @since 3.1.1
instance PUnsafeLiftDecl PTxOut where
  type PLifted PTxOut = Plutus.TxOut

-- | @since 3.1.1
deriving via
  (DerivePConstantViaData Plutus.TxOut PTxOut)
  instance
    PConstantDecl Plutus.TxOut

-- | @since 3.1.1
instance PTryFrom PData (PAsData PTxOut)

-- | @since 3.1.1
newtype PTxInInfo (s :: S)
  = PTxInInfo
      ( Term
          s
          ( PDataRecord
              '[ "outRef" ':= Tx.PTxOutRef
               , "resolved" ':= PTxOut
               ]
          )
      )
  deriving stock
    ( -- | @since 3.1.1
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.1
      PlutusType
    , -- | @since 3.1.1
      PIsData
    , -- | @since 3.1.1
      PEq
    , -- | @since 3.1.1
      PShow
    , -- | @since 3.1.1
      PTryFrom PData
    )

-- | @since 3.1.1
instance DerivePlutusType PTxInInfo where
  type DPTStrat _ = PlutusTypeData

-- | @since 3.1.1
instance PUnsafeLiftDecl PTxInInfo where
  type PLifted PTxInInfo = Plutus.TxInInfo

-- | @since 3.1.1
deriving via
  (DerivePConstantViaData Plutus.TxInInfo PTxInInfo)
  instance
    PConstantDecl Plutus.TxInInfo

-- | @since 3.1.1
instance PTryFrom PData (PAsData PTxInInfo)
