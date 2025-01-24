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
newtype PTxInInfo (s :: S)
  = PTxInInfo
      ( Term
          s
          ( PDataRecord
              '[ "outRef" ':= V1Tx.PTxOutRef
               , "resolved" ':= V2Tx.PTxOut
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
      PDataFields
    , -- | @since 3.1.1
      PEq
    , -- | @since 3.1.1
      PShow
    )

-- | @since 3.1.1
instance DerivePlutusType PTxInInfo where
  type DPTStrat _ = PlutusTypeData

-- | @since WIP
deriving via
  DeriveDataPLiftable PTxInInfo Plutus.TxInInfo
  instance
    PLiftable PTxInInfo

-- | @since 3.1.1
newtype PTxInfo (s :: S)
  = PTxInfo
      ( Term
          s
          ( PDataRecord
              '[ "inputs" ':= PBuiltinList (PAsData PTxInInfo)
               , "referenceInputs" ':= PBuiltinList (PAsData PTxInInfo)
               , "outputs" ':= PBuiltinList (PAsData V2Tx.PTxOut)
               , "fee" ':= Value.PValue 'AssocMap.Sorted 'Value.Positive
               , "mint" ':= Value.PValue 'AssocMap.Sorted 'Value.NoGuarantees -- value minted by transaction
               , "dcert" ':= PBuiltinList (PAsData DCert.PDCert)
               , "wdrl" ':= AssocMap.PMap 'AssocMap.Unsorted Credential.PStakingCredential PInteger -- Staking withdrawals
               , "validRange" ':= Interval.PInterval Time.PPosixTime
               , "signatories" ':= PBuiltinList (PAsData Crypto.PPubKeyHash)
               , "redeemers" ':= AssocMap.PMap 'AssocMap.Unsorted Contexts.PScriptPurpose Scripts.PRedeemer
               , "data" ':= AssocMap.PMap 'AssocMap.Unsorted Scripts.PDatumHash Scripts.PDatum
               , "id" ':= V1Tx.PTxId -- hash of the pending transaction
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
      PDataFields
    , -- | @since 3.1.1
      PEq
    , -- | @since 3.1.1
      PShow
    )

-- | @since 3.1.1
instance DerivePlutusType PTxInfo where
  type DPTStrat _ = PlutusTypeData

-- | @since WIP
deriving via
  DeriveDataPLiftable PTxInfo Plutus.TxInfo
  instance
    PLiftable PTxInfo

-- | @since 3.1.1
newtype PScriptContext (s :: S)
  = PScriptContext (Term s (PDataRecord '["txInfo" ':= PTxInfo, "purpose" ':= Contexts.PScriptPurpose]))
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
      PDataFields
    , -- | @since 3.1.1
      PEq
    , -- | @since 3.1.1
      PShow
    )

-- | @since 3.1.1
instance DerivePlutusType PScriptContext where
  type DPTStrat _ = PlutusTypeData

-- | @since WIP
deriving via
  DeriveDataPLiftable PScriptContext Plutus.ScriptContext
  instance
    PLiftable PScriptContext
