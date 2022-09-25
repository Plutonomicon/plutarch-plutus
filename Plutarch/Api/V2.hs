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
) where

import qualified Plutarch.Api.V2.Contexts as Contexts
import qualified Plutarch.Api.V2.Tx as Tx

import Plutarch.Api.Internal.Hashing (hashScriptWithPrefix)
import Plutarch.Api.V1 (dataHash, datumHash, redeemerHash)
import qualified Plutarch.Api.V1.Address as V1
import qualified Plutarch.Api.V1.AssocMap as V1
import qualified Plutarch.Api.V1.Crypto as V1
import qualified Plutarch.Api.V1.Interval as V1
import qualified Plutarch.Api.V1.Maybe as V1
import qualified Plutarch.Api.V1.Scripts as V1
import qualified Plutarch.Api.V1.Time as V1
import qualified Plutarch.Api.V1.Tuple as V1
import qualified Plutarch.Api.V1.Value as V1
import PlutusLedgerApi.V1.Scripts (ScriptHash)
import Plutarch.Script (Script)

-- | Hash a Script, with the correct prefix for Plutus V2
scriptHash :: Script -> ScriptHash
scriptHash = hashScriptWithPrefix "\x02"
