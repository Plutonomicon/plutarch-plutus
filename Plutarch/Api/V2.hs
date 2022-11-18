module Plutarch.Api.V2 (
  -- ** Contexts
  Contexts.PScriptContext (PScriptContext),
  Contexts.PTxInfo (PTxInfo),
  Contexts.PScriptPurpose (PMinting, PSpending, PRewarding, PCertifying),

  -- ** Tx
  Tx.PTxOutRef (PTxOutRef),
  Tx.PTxOut (PTxOut),
  Tx.PTxId (PTxId),
  Tx.PTxInInfo (PTxInInfo),
  Tx.POutputDatum (PNoOutputDatum, POutputDatumHash, POutputDatum),

  -- *** reexports for unchanged V1 ledger types
  V1.PMaybeData (PDNothing, PDJust),
  V1.PTuple,
  V1.PDatum (PDatum),
  V1.PDatumHash (PDatumHash),
  V1.PAddress (PAddress),
  V1.KeyGuarantees (Sorted, Unsorted),
  V1.AmountGuarantees (NoGuarantees, Positive, NonZero),
  V1.PScriptHash (PScriptHash),
  V1.PPubKeyHash (PPubKeyHash),
  V1.PStakingCredential (PStakingHash, PStakingPtr),
  V1.PValue (PValue),
  V1.PCurrencySymbol (PCurrencySymbol),
  V1.PTokenName (PTokenName),
  V1.PInterval (PInterval),
  V1.PExtended (PFinite, PPosInf, PNegInf),
  type V1.PClosure,
  V1.PLowerBound (PLowerBound),
  V1.PUpperBound (PUpperBound),
  V1.PPOSIXTime (PPOSIXTime),
  type V1.PPOSIXTimeRange,
  V1.PMap (PMap),

  -- ** Script Utils
  scriptHash,
  datumHash,
  redeemerHash,
  dataHash,
  PValidator,
  PStakeValidator,
  PMintingPolicy,
) where

import Plutarch.Api.V2.Contexts qualified as Contexts
import Plutarch.Api.V2.Tx qualified as Tx

import Plutarch.Api.Internal.Hashing (hashScriptWithPrefix)
import Plutarch.Api.V1 (
  dataHash,
  datumHash,
  redeemerHash,
 )
import Plutarch.Api.V1.Address qualified as V1
import Plutarch.Api.V1.AssocMap qualified as V1
import Plutarch.Api.V1.Crypto qualified as V1
import Plutarch.Api.V1.Interval qualified as V1
import Plutarch.Api.V1.Maybe qualified as V1
import Plutarch.Api.V1.Scripts qualified as V1
import Plutarch.Api.V1.Time qualified as V1
import Plutarch.Api.V1.Tuple qualified as V1
import Plutarch.Api.V1.Value qualified as V1

import Plutarch (POpaque, (:-->))
import Plutarch.Builtin (PData)

import Plutarch.Script (Script)
import PlutusLedgerApi.V1.Scripts (ScriptHash)

-- | Hash a Script, with the correct prefix for Plutus V2
scriptHash :: Script -> ScriptHash
scriptHash = hashScriptWithPrefix "\x02"

-- | a Validator Term
type PValidator = PData :--> PData :--> Contexts.PScriptContext :--> POpaque

-- | a MintingPolicy Term
type PMintingPolicy = PData :--> Contexts.PScriptContext :--> POpaque

-- | a StakeValidator Term
type PStakeValidator = PData :--> Contexts.PScriptContext :--> POpaque
