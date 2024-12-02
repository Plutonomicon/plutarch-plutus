{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.LedgerApi.V1.Contexts (
  PScriptPurpose (..),
) where

import GHC.Generics (Generic)
import Plutarch.LedgerApi.V1.Credential qualified as Credential
import Plutarch.LedgerApi.V1.DCert qualified as DCert
import Plutarch.LedgerApi.V1.Tx qualified as Tx
import Plutarch.LedgerApi.Value qualified as Value
import Plutarch.Prelude
import PlutusLedgerApi.V1 qualified as Plutus

-- | @since 3.1.1
data PScriptPurpose (s :: S)
  = PMinting (Term s (PDataRecord '["_0" ':= Value.PCurrencySymbol]))
  | PSpending (Term s (PDataRecord '["_0" ':= Tx.PTxOutRef]))
  | PRewarding (Term s (PDataRecord '["_0" ':= Credential.PStakingCredential]))
  | PCertifying (Term s (PDataRecord '["_0" ':= DCert.PDCert]))
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
instance DerivePlutusType PScriptPurpose where
  type DPTStrat _ = PlutusTypeData

-- | @since WIP
deriving via
  DeriveDataPLiftable PScriptPurpose Plutus.ScriptPurpose
  instance
    PLiftable PScriptPurpose

-- | @since 3.1.1
instance PTryFrom PData (PAsData PScriptPurpose)
