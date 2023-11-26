{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Api.V2.Contexts (
  PScriptContext (PScriptContext),
  PTxInfo (PTxInfo),
  V1.PScriptPurpose (PMinting, PSpending, PRewarding, PCertifying),
) where

import Plutarch.Api.V1 qualified as V1
import Plutarch.Api.V2.Tx (PTxId, PTxInInfo, PTxOut)
import PlutusLedgerApi.V2 qualified as Plutus

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
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq, PShow)

instance DerivePlutusType PScriptContext where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PScriptContext where type PLifted _ = Plutus.ScriptContext
deriving via (DerivePConstantViaData Plutus.ScriptContext PScriptContext) instance PConstantDecl Plutus.ScriptContext

-- | A pending transaction. This is the view as seen by the validator script.
newtype PTxInfo (s :: S)
  = PTxInfo
      ( Term
          s
          ( PDataRecord
              '[ "inputs" ':= PBuiltinList PTxInInfo -- Transaction inputs
               , "referenceInputs" ':= PBuiltinList PTxInInfo
               , "outputs" ':= PBuiltinList PTxOut -- Transaction outputs
               , "fee" ':= V1.PValue 'V1.Sorted 'V1.Positive -- The fee paid by this transaction.
               , "mint" ':= V1.PValue 'V1.Sorted 'V1.NoGuarantees -- The value minted by the transaction.
               , "dcert" ':= PBuiltinList V1.PDCert -- Digests of the certificates included in this transaction.
               , "wdrl" ':= V1.PMap 'V1.Unsorted V1.PStakingCredential PInteger -- Staking withdrawals
               , "validRange" ':= V1.PPOSIXTimeRange -- The valid range for the transaction.
               , "signatories" ':= PBuiltinList (PAsData V1.PPubKeyHash) -- Signatories attesting that they all signed the tx.
               , "redeemers" ':= V1.PMap 'V1.Unsorted V1.PScriptPurpose V1.PRedeemer
               , "datums" ':= V1.PMap 'V1.Unsorted V1.PDatumHash V1.PDatum
               , "id" ':= PTxId -- The hash of the pending transaction.
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq, PShow)

instance DerivePlutusType PTxInfo where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PTxInfo where type PLifted _ = Plutus.TxInfo
deriving via (DerivePConstantViaData Plutus.TxInfo PTxInfo) instance PConstantDecl Plutus.TxInfo
