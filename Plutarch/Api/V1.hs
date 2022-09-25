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

import qualified Plutarch.Api.V1.Address as Address
import qualified Plutarch.Api.V1.AssocMap as AssocMap
import qualified Plutarch.Api.V1.Contexts as Contexts
import qualified Plutarch.Api.V1.Crypto as Crypto
import qualified Plutarch.Api.V1.DCert as DCert
import qualified Plutarch.Api.V1.Interval as Interval
import qualified Plutarch.Api.V1.Maybe as Maybe
import qualified Plutarch.Api.V1.Scripts as Scripts
import qualified Plutarch.Api.V1.Time as Time
import qualified Plutarch.Api.V1.Tuple as Tuple
import qualified Plutarch.Api.V1.Tx as Tx
import qualified Plutarch.Api.V1.Value as Value

import Data.Coerce (coerce)

-- note about V2: This should there are no changes in Scripts or V1 itself that affect this module
import qualified PlutusLedgerApi.V1 as Plutus
import qualified Plutarch.Script as Plutus

import Plutarch.Api.Internal.Hashing (hashData, hashScriptWithPrefix)

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
