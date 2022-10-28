module Plutarch.Api.V1 (
  -- * Contexts
  Contexts.PScriptContext (PScriptContext),
  Contexts.PTxInfo (PTxInfo),
  Contexts.PScriptPurpose (PMinting, PSpending, PRewarding, PCertifying),

  -- ** Script
  Scripts.PDatum (PDatum),
  Scripts.PDatumHash (PDatumHash),
  Scripts.PRedeemer (PRedeemer),
  Scripts.PRedeemerHash (PRedeemerHash),

  -- ** Script Utils
  scriptHash,
  datumHash,
  redeemerHash,
  dataHash,
  PValidator,
  PStakeValidator,
  PMintingPolicy,

  -- ** Value
  Value.PValue (PValue),
  Value.PCurrencySymbol (PCurrencySymbol),
  Value.PTokenName (PTokenName),
  Value.KeyGuarantees (Unsorted, Sorted),
  Value.AmountGuarantees (NoGuarantees, NonZero, Positive),

  -- ** Crypto
  Crypto.PPubKeyHash (PPubKeyHash),
  Crypto.pubKeyHash,

  -- ** DCert
  DCert.PDCert (
    PDCertDelegDelegate,
    PDCertDelegDeRegKey,
    PDCertDelegRegKey,
    PDCertGenesis,
    PDCertMir,
    PDCertPoolRegister,
    PDCertPoolRetire
  ),

  -- ** Time
  Time.PPOSIXTime,
  type Time.PPOSIXTimeRange,

  -- ** Interval
  Interval.PInterval (PInterval),
  Interval.PLowerBound (PLowerBound),
  Interval.PUpperBound (PUpperBound),
  Interval.PExtended (PFinite, PPosInf, PNegInf),
  type Interval.PClosure,

  -- ** Address
  Address.PCredential (PPubKeyCredential, PScriptCredential),
  Address.PStakingCredential (PStakingHash, PStakingPtr),
  Address.PAddress (PAddress),

  -- ** Tx
  Tx.PTxOutRef (PTxOutRef),
  Tx.PTxOut (PTxOut),
  Tx.PTxId (PTxId),
  Tx.PTxInInfo (PTxInInfo),

  -- ** AssocMap
  AssocMap.PMap (PMap),

  -- ** Others
  Maybe.PMaybeData (PDJust, PDNothing),
  type Tuple.PTuple,

  -- ** Utility functions
  Tuple.ptuple,
) where

--------------------------------------------------------------------------------

import Plutarch.Api.V1.Address qualified as Address
import Plutarch.Api.V1.AssocMap qualified as AssocMap
import Plutarch.Api.V1.Contexts qualified as Contexts
import Plutarch.Api.V1.Crypto qualified as Crypto
import Plutarch.Api.V1.DCert qualified as DCert
import Plutarch.Api.V1.Interval qualified as Interval
import Plutarch.Api.V1.Maybe qualified as Maybe
import Plutarch.Api.V1.Scripts qualified as Scripts
import Plutarch.Api.V1.Time qualified as Time
import Plutarch.Api.V1.Tuple qualified as Tuple
import Plutarch.Api.V1.Tx qualified as Tx
import Plutarch.Api.V1.Value qualified as Value

import Plutarch (POpaque, (:-->))
import Plutarch.Builtin (PData)

-- note about V2: This should there are no changes in Scripts or V1 itself that affect this module

import Plutarch.Script qualified as Plutus
import PlutusLedgerApi.V1 qualified as Plutus

import Plutarch.Api.Internal.Hashing (hashData, hashScriptWithPrefix)

import Data.Coerce (coerce)

-- On-chain Script Types

-- | Hash a Script, with the correct prefix for Plutus V1
scriptHash :: Plutus.Script -> Plutus.ScriptHash
scriptHash = hashScriptWithPrefix "\x01"

-- | Hash a Datum.
datumHash :: Plutus.Datum -> Plutus.DatumHash
datumHash = coerce . dataHash

-- | Hash a Redeemer.
redeemerHash :: Plutus.Redeemer -> Plutus.RedeemerHash
redeemerHash = coerce . dataHash

-- | Hash the data encoded representation of given argument.
dataHash :: Plutus.ToData a => a -> Plutus.BuiltinByteString
dataHash = hashData . Plutus.toData

-- | a Validator Term
type PValidator = PData :--> PData :--> Contexts.PScriptContext :--> POpaque

-- | a MintingPolicy Term
type PMintingPolicy = PData :--> Contexts.PScriptContext :--> POpaque

-- | a StakeValidator Term
type PStakeValidator = PData :--> Contexts.PScriptContext :--> POpaque
