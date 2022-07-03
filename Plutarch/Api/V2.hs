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

  -- *** reexports for V2 Tx and Contexts
  V1.PMaybeData (PDNothing, PDJust),
  V1.PTuple,
  V1.PDatum (PDatum),
  V1.PDatumHash (PDatumHash),
  V1.PAddress (PAddress),
  V1.KeyGuarantees (Sorted, Unsorted),
  V1.AmountGuarantees (NoGuarantees, Positive),
  V1.PScriptHash (PScriptHash),
  V1.PPubKeyHash (PPubKeyHash),
  V1.PStakingCredential (PStakingHash, PStakingPtr),
  type V1.PPOSIXTimeRange,
) where

import qualified Plutarch.Api.V2.Contexts as Contexts
import qualified Plutarch.Api.V2.Tx as Tx

import qualified Plutarch.Api.V1.Address as V1
import qualified Plutarch.Api.V1.Crypto as V1
import qualified Plutarch.Api.V1.Maybe as V1
import qualified Plutarch.Api.V1.Scripts as V1
import qualified Plutarch.Api.V1.Time as V1
import qualified Plutarch.Api.V1.Tuple as V1
import qualified Plutarch.Api.V1.Value as V1

-- TODO:
-- - add util functions that are also part of PlutusLedgerApi.V2
