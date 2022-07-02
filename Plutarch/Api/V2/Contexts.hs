module Plutarch.Api.V2.Contexts (
  PScriptContext (PScriptContext),
) where

import qualified Plutarch.Api.V1.Value as V1
import Plutarch.Api.V2.Tx (PTxInInfo)
import qualified PlutusLedgerApi.V2 as Plutus

import Plutarch.DataRepr (PDataFields)
import Plutarch.Prelude

-- FIXME: add PDataFields to Prelude

-- | Script context consists of the script purpose and the pending transaction info.
newtype PScriptContext (s :: S)
  = PScriptContext
      ( Term
          s
          ( PDataRecord
              '[ "txInfo" ':= PTxInfo
               , "purpose" ':= PScriptPurpose
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

-- | A pending transaction. This is the view as seen by the validator script.
newtype PTxInfo (s :: S)
  = PTxInfo
      ( Term
          s
          ( PDataRecord
              '[ "inputs" ':= PBuiltinList (PAsData PTxInInfo) -- Transaction inputs
               , "refInputs" ':= PBuiltinList (PAsData PTxInInfo)
               , "outputs" ':= PBuiltinList (PAsData PTxOut) -- Transaction outputs
               , "fee" ':= V1.PValue V1.Sorted V1.Positive -- The fee paid by this transaction.
               , "mint" ':= V1.PValue V1.Sorted V1.NoGuarantees -- The value minted by the transaction.
               , "dcert" ':= PBuiltinList (PAsData PDCert) -- Digests of the certificates included in this transaction.
               , "wdrl" ':= PBuiltinList (PAsData (PTuple PStakingCredential PInteger)) -- Staking withdrawals
               , "validRange" ':= PPOSIXTimeRange -- The valid range for the transaction.
               , "signatories" ':= PBuiltinList (PAsData PPubKeyHash) -- Signatories attesting that they all signed the tx.
               , "datums" ':= PBuiltinList (PAsData (PTuple PDatumHash PDatum))
               , "id" ':= PTxId -- The hash of the pending transaction.
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PTxInfo where type DPTStrat _ = PlutusTypeData
