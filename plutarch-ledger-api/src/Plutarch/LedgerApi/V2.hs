{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.LedgerApi.V2 (
  -- * Contexts
  Contexts.PScriptPurpose (..),
  PScriptContext (..),

  -- * Certificates
  DCert.PDCert (..),

  -- * Credentials
  Credential.PCredential (..),
  Credential.PStakingCredential (..),

  -- * Value
  Value.PValue (..),
  Value.AmountGuarantees (..),
  Value.PLovelace (..),
  Value.PTokenName (..),
  Value.PCurrencySymbol (..),

  -- * Time
  Time.PPosixTime (..),
  Time.pposixTime,
  Time.unPPosixTime,

  -- * Intervals
  Interval.PExtended (..),
  Interval.PLowerBound (..),
  Interval.PUpperBound (..),
  Interval.PInterval (..),

  -- * Script stuff
  Scripts.PDatum (..),
  Scripts.PRedeemer (..),
  Scripts.PDatumHash (..),
  Scripts.PRedeemerHash (..),
  Scripts.PScriptHash (..),

  -- * Transactions
  Address.PAddress (..),
  Crypto.PPubKeyHash (..),
  V1Tx.PTxId (..),
  PTxInfo (..),
  V2Tx.PTxOut (..),
  V1Tx.PTxOutRef (..),
  PTxInInfo (..),
  V2Tx.POutputDatum (..),

  -- * Helpers
  AssocMap.PMap (..),
  AssocMap.KeyGuarantees (..),
  AssocMap.Commutativity (..),

  -- * Utilities

  -- ** Types
  Utils.PMaybeData (..),
  Utils.PRationalData (..),

  -- ** Utilities
  Utils.pfromDJust,
  Utils.pisDJust,
  Utils.pmaybeData,
  Utils.pdjust,
  Utils.pdnothing,
  Utils.pmaybeToMaybeData,
  Utils.passertPDJust,
  Utils.prationalFromData,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Interval qualified as Interval
import Plutarch.LedgerApi.Utils qualified as Utils
import Plutarch.LedgerApi.V1.Address qualified as Address
import Plutarch.LedgerApi.V1.Contexts qualified as Contexts
import Plutarch.LedgerApi.V1.Credential qualified as Credential
import Plutarch.LedgerApi.V1.Crypto qualified as Crypto
import Plutarch.LedgerApi.V1.DCert qualified as DCert
import Plutarch.LedgerApi.V1.Scripts qualified as Scripts
import Plutarch.LedgerApi.V1.Time qualified as Time
import Plutarch.LedgerApi.V1.Tx qualified as V1Tx
import Plutarch.LedgerApi.V2.Tx qualified as V2Tx
import Plutarch.LedgerApi.Value qualified as Value
import Plutarch.Prelude
import PlutusLedgerApi.V2 qualified as Plutus

-- | @since 3.1.1
data PTxInInfo (s :: S) = PTxInInfo
  { ptxInInfo'outRef :: Term s V1Tx.PTxOutRef
  , ptxInInfo'resolved :: Term s V2Tx.PTxOut
  }
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
    via (DeriveAsDataStruct PTxInInfo)

-- | @since 3.3.0
deriving via
  DeriveDataPLiftable PTxInInfo Plutus.TxInInfo
  instance
    PLiftable PTxInInfo

-- | @since 3.4.0
instance PTryFrom PData (PAsData PTxInInfo)

-- | @since 3.1.1
data PTxInfo (s :: S) = PTxInfo
  { ptxInfo'inputs :: Term s (PAsData (PBuiltinList (PAsData PTxInInfo)))
  , ptxInfo'referenceInputs :: Term s (PAsData (PBuiltinList (PAsData PTxInInfo)))
  , ptxInfo'outputs :: Term s (PAsData (PBuiltinList (PAsData V2Tx.PTxOut)))
  , ptxInfo'fee :: Term s (PAsData (Value.PValue 'AssocMap.Sorted 'Value.Positive))
  , ptxInfo'mint :: Term s (PAsData (Value.PValue 'AssocMap.Sorted 'Value.NoGuarantees)) -- value minted by transaction
  , ptxInfo'dcert :: Term s (PAsData (PBuiltinList (PAsData DCert.PDCert)))
  , ptxInfo'wdrl :: Term s (PAsData (AssocMap.PMap 'AssocMap.Unsorted Credential.PStakingCredential PInteger)) -- Staking withdrawals
  , ptxInfo'validRange :: Term s (Interval.PInterval Time.PPosixTime)
  , ptxInfo'signatories :: Term s (PAsData (PBuiltinList (PAsData Crypto.PPubKeyHash)))
  , ptxInfo'redeemers :: Term s (PAsData (AssocMap.PMap 'AssocMap.Unsorted Contexts.PScriptPurpose Scripts.PRedeemer))
  , ptxInfo'data :: Term s (PAsData (AssocMap.PMap 'AssocMap.Unsorted Scripts.PDatumHash Scripts.PDatum))
  , ptxInfo'id :: Term s V1Tx.PTxId -- hash of the pending transaction
  }
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
    via (DeriveAsDataStruct PTxInfo)

-- | @since 3.3.0
deriving via
  DeriveDataPLiftable PTxInfo Plutus.TxInfo
  instance
    PLiftable PTxInfo

-- | @since 3.4.0
instance PTryFrom PData (PAsData PTxInfo)

-- | @since 3.1.1
data PScriptContext (s :: S) = PScriptContext
  { pscriptContext'txInfo :: Term s PTxInfo
  , pscriptContext'purpose :: Term s Contexts.PScriptPurpose
  }
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
    via (DeriveAsDataStruct PScriptContext)

-- | @since 3.3.0
deriving via
  DeriveDataPLiftable PScriptContext Plutus.ScriptContext
  instance
    PLiftable PScriptContext

-- | @since 3.4.0
instance PTryFrom PData (PAsData PScriptContext)
