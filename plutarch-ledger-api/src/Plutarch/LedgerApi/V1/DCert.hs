{-# OPTIONS_GHC -Wno-orphans #-}

-- Mirrors the corresponding V1 module in plutus-ledger-api
module Plutarch.LedgerApi.V1.DCert (
  PDCert (..),
) where

import Plutarch.Prelude

import Plutarch.LedgerApi.V1.Credential (PStakingCredential)
import Plutarch.LedgerApi.V1.Crypto (PPubKeyHash)
import PlutusLedgerApi.V1 qualified as Plutus

-- | @since 3.1.1
data PDCert (s :: S)
  = PDCertDelegRegKey (Term s (PDataRecord '["_0" ':= PStakingCredential]))
  | PDCertDelegDeRegKey (Term s (PDataRecord '["_0" ':= PStakingCredential]))
  | PDCertDelegDelegate (Term s (PDataRecord '["_0" ':= PStakingCredential, "_1" ':= PPubKeyHash]))
  | PDCertPoolRegister (Term s (PDataRecord '["_0" ':= PPubKeyHash, "_1" ':= PPubKeyHash]))
  | PDCertPoolRetire (Term s (PDataRecord '["_0" ':= PPubKeyHash, "_1" ':= PInteger]))
  | PDCertGenesis (Term s (PDataRecord '[]))
  | PDCertMir (Term s (PDataRecord '[]))
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
instance DerivePlutusType PDCert where
  type DPTStrat _ = PlutusTypeData

-- | @since WIP
deriving via
  DeriveDataPLiftable PDCert Plutus.DCert
  instance
    PLiftable PDCert

-- | @since 3.1.0
instance PTryFrom PData (PAsData PDCert)
