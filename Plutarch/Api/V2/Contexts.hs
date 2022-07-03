{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Api.V2.Contexts (
  PScriptContext (PScriptContext),
  PTxInfo (PTxInfo),
  V1.PScriptPurpose (PMinting, PSpending, PRewarding, PCertifying),
) where

import qualified Plutarch.Api.V1.Address as V1
import qualified Plutarch.Api.V1.Contexts as V1
import qualified Plutarch.Api.V1.Crypto as V1
import qualified Plutarch.Api.V1.DCert as V1
import qualified Plutarch.Api.V1.Scripts as V1
import qualified Plutarch.Api.V1.Time as V1
import qualified Plutarch.Api.V1.Value as V1
import Plutarch.Api.V2.Tx (PTxId, PTxInInfo, PTxOut)
import qualified PlutusLedgerApi.V2 as Plutus

import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
 )
import Plutarch.Lift (
  PConstantDecl,
  PLifted,
  PUnsafeLiftDecl,
 )
import Plutarch.Prelude

-- FIXME: add PDataFields to Prelude

-- | Script context consists of the script purpose and the pending transaction info.
newtype PScriptContext (s :: S)
  = PScriptContext
      ( Term
          s
          ( PDataRecord
              '[ "txInfo" ':= PTxInfo
               , "purpose" ':= V1.PScriptPurpose
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PScriptContext where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PScriptContext where type PLifted _ = Plutus.ScriptContext
deriving via (DerivePConstantViaData Plutus.ScriptContext PScriptContext) instance PConstantDecl Plutus.ScriptContext

-- | A pending transaction. This is the view as seen by the validator script.
newtype PTxInfo (s :: S)
  = PTxInfo
      ( Term
          s
          ( PDataRecord
              '[ "inputs" ':= PBuiltinList (PAsData PTxInInfo) -- Transaction inputs
               , "referenceInputs" ':= PBuiltinList (PAsData PTxInInfo)
               , "outputs" ':= PBuiltinList (PAsData PTxOut) -- Transaction outputs
               , "fee" ':= V1.PValue 'V1.Sorted 'V1.Positive -- The fee paid by this transaction.
               , "mint" ':= V1.PValue 'V1.Sorted 'V1.NoGuarantees -- The value minted by the transaction.
               , "dcert" ':= PBuiltinList (PAsData V1.PDCert) -- Digests of the certificates included in this transaction.
               , "wdrl" ':= PBuiltinList (PAsData (V1.PTuple V1.PStakingCredential PInteger)) -- Staking withdrawals
               , "validRange" ':= V1.PPOSIXTimeRange -- The valid range for the transaction.
               , "signatories" ':= PBuiltinList (PAsData V1.PPubKeyHash) -- Signatories attesting that they all signed the tx.
               , "datums" ':= PBuiltinList (PAsData (V1.PTuple V1.PDatumHash V1.PDatum))
               , "id" ':= PTxId -- The hash of the pending transaction.
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq)

instance DerivePlutusType PTxInfo where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PTxInfo where type PLifted _ = Plutus.TxInfo
deriving via (DerivePConstantViaData Plutus.TxInfo PTxInfo) instance PConstantDecl Plutus.TxInfo
