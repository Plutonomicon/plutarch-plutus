{-# OPTIONS_GHC -Wno-orphans #-}

-- Mirrors the equivalent V1 module in plutus-ledger api
module Plutarch.LedgerApi.V1.Credential (
  PCredential (..),
  PStakingCredential (..),
) where

import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
 )
import Plutarch.LedgerApi.V1.Crypto (PPubKeyHash)
import Plutarch.LedgerApi.V1.Scripts (PScriptHash)
import Plutarch.Lift (
  PConstantDecl,
  PUnsafeLiftDecl (PLifted),
 )
import Plutarch.Prelude
import PlutusLedgerApi.V1 qualified as Plutus

-- | @since 2.0.0
data PCredential (s :: S)
  = PPubKeyCredential (Term s (PDataRecord '["_0" ':= PPubKeyHash]))
  | PScriptCredential (Term s (PDataRecord '["_0" ':= PScriptHash]))
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
      PPartialOrd
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PShow
    , -- | @since 2.0.0
      PTryFrom PData
    )

-- | @since 2.0.0
instance DerivePlutusType PCredential where
  type DPTStrat _ = PlutusTypeData

-- | @since 2.0.0
instance PUnsafeLiftDecl PCredential where
  type PLifted PCredential = Plutus.Credential

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData Plutus.Credential PCredential)
  instance
    PConstantDecl Plutus.Credential

-- | @since 2.0.0
instance PTryFrom PData (PAsData PCredential)

-- | @since 2.0.0
data PStakingCredential (s :: S)
  = PStakingHash (Term s (PDataRecord '["_0" ':= PCredential]))
  | PStakingPtr
      ( Term
          s
          ( PDataRecord
              '[ "_0" ':= PInteger
               , "_1" ':= PInteger
               , "_2" ':= PInteger
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
      PEq
    , -- | @since 2.0.0
      PPartialOrd
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PShow
    , -- | @since 2.0.0
      PTryFrom PData
    )

-- | @since 2.0.0
instance DerivePlutusType PStakingCredential where
  type DPTStrat _ = PlutusTypeData

-- | @since 2.0.0
instance PUnsafeLiftDecl PStakingCredential where
  type PLifted PStakingCredential = Plutus.StakingCredential

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData Plutus.StakingCredential PStakingCredential)
  instance
    PConstantDecl Plutus.StakingCredential

-- | @since 2.0.0
instance PTryFrom PData (PAsData PStakingCredential)
