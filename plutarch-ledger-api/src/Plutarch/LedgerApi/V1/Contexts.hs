{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.LedgerApi.V1.Contexts (
  PScriptPurpose (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.V1.Credential qualified as Credential
import Plutarch.LedgerApi.V1.DCert qualified as DCert
import Plutarch.LedgerApi.V1.Tx qualified as Tx
import Plutarch.LedgerApi.Value qualified as Value
import Plutarch.Prelude
import PlutusLedgerApi.V1 qualified as Plutus

-- | @since 3.1.1
data PScriptPurpose (s :: S)
  = PMinting (Term s (PAsData Value.PCurrencySymbol))
  | PSpending (Term s Tx.PTxOutRef)
  | PRewarding (Term s Credential.PStakingCredential)
  | PCertifying (Term s DCert.PDCert)
  deriving stock
    ( -- | @since 3.1.1
      Generic
    )
  deriving anyclass
    ( -- | @since 3.3.0
      SOP.Generic
    , -- | @since 3.1.1
      PIsData
    , -- | @since 3.1.1
      PEq
    , -- | @since 3.1.1
      PShow
    )
  deriving
    ( -- | @since 3.3.0
      PlutusType
    )
    via (DeriveAsDataStruct PScriptPurpose)

-- | @since 3.3.0
deriving via
  DeriveDataPLiftable PScriptPurpose Plutus.ScriptPurpose
  instance
    PLiftable PScriptPurpose

-- | @since 3.4.0
instance PTryFrom PData (PAsData PScriptPurpose)
