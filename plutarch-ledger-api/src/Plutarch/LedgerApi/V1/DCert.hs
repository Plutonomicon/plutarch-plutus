{-# OPTIONS_GHC -Wno-orphans #-}

-- Mirrors the corresponding V1 module in plutus-ledger-api
module Plutarch.LedgerApi.V1.DCert (
  PDCert (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.V1.Credential (PStakingCredential)
import Plutarch.LedgerApi.V1.Crypto (PPubKeyHash)
import Plutarch.Prelude
import PlutusLedgerApi.V1 qualified as Plutus

-- | @since 3.1.1
data PDCert (s :: S)
  = PDCertDelegRegKey (Term s PStakingCredential)
  | PDCertDelegDeRegKey (Term s PStakingCredential)
  | PDCertDelegDelegate (Term s PStakingCredential) (Term s (PAsData PPubKeyHash))
  | PDCertPoolRegister (Term s (PAsData PPubKeyHash)) (Term s (PAsData PPubKeyHash))
  | PDCertPoolRetire (Term s (PAsData PPubKeyHash)) (Term s (PAsData PInteger))
  | PDCertGenesis
  | PDCertMir
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
    via (DeriveAsDataStruct PDCert)

-- | @since 3.3.0
deriving via
  DeriveDataPLiftable PDCert Plutus.DCert
  instance
    PLiftable PDCert

-- | @since 3.4.0
instance PTryFrom PData (PAsData PDCert)
