{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Api.V1.Contexts (
  type PTuple,
  PScriptContext (PScriptContext),
  PTxInfo (PTxInfo),
  PScriptPurpose (PMinting, PSpending, PRewarding, PCertifying),
) where

import PlutusLedgerApi.V1 qualified as Plutus

import Plutarch.Api.V1.Address (
  PStakingCredential,
 )
import Plutarch.Api.V1.Crypto (PPubKeyHash)
import Plutarch.Api.V1.DCert (PDCert)
import Plutarch.Api.V1.Scripts (PDatum, PDatumHash)
import Plutarch.Api.V1.Time (PPOSIXTimeRange)
import Plutarch.Api.V1.Tuple (PTuple)
import Plutarch.Api.V1.Tx (PTxId, PTxInInfo, PTxOut, PTxOutRef)
import Plutarch.Api.V1.Value (
  AmountGuarantees (NoGuarantees, Positive),
  KeyGuarantees (Sorted),
  PCurrencySymbol,
  PValue,
 )
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

-- | A pending transaction. This is the view as seen by the validator script.
newtype PTxInfo (s :: S)
  = PTxInfo
      ( Term
          s
          ( PDataRecord
              '[ "inputs" ':= PBuiltinList PTxInInfo -- Transaction inputs
               , "outputs" ':= PBuiltinList PTxOut -- Transaction outputs
               , "fee" ':= PValue 'Sorted 'Positive -- The fee paid by this transaction.
               , "mint" ':= PValue 'Sorted 'NoGuarantees -- The value minted by the transaction.
               , "dcert" ':= PBuiltinList PDCert -- Digests of the certificates included in this transaction.
               , "wdrl" ':= PBuiltinList (PAsData (PTuple PStakingCredential PInteger)) -- Staking withdrawals
               , "validRange" ':= PPOSIXTimeRange -- The valid range for the transaction.
               , "signatories" ':= PBuiltinList (PAsData PPubKeyHash) -- Signatories attesting that they all signed the tx.
               , "datums" ':= PBuiltinList (PAsData (PTuple PDatumHash PDatum))
               , "id" ':= PTxId -- The hash of the pending transaction.
               ]
          )
      )
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq, PShow)

instance DerivePlutusType PTxInfo where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PTxInfo where type PLifted PTxInfo = Plutus.TxInfo
deriving via (DerivePConstantViaData Plutus.TxInfo PTxInfo) instance PConstantDecl Plutus.TxInfo

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
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq, PShow)

instance DerivePlutusType PScriptContext where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PScriptContext where type PLifted PScriptContext = Plutus.ScriptContext
deriving via (DerivePConstantViaData Plutus.ScriptContext PScriptContext) instance PConstantDecl Plutus.ScriptContext

-- General types, used by V1 and V2

-- | The purpose of the script that is currently running
data PScriptPurpose (s :: S)
  = PMinting (Term s (PDataRecord '["_0" ':= PCurrencySymbol]))
  | PSpending (Term s (PDataRecord '["_0" ':= PTxOutRef]))
  | PRewarding (Term s (PDataRecord '["_0" ':= PStakingCredential]))
  | PCertifying (Term s (PDataRecord '["_0" ':= PDCert]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType PScriptPurpose where type DPTStrat _ = PlutusTypeData

instance PUnsafeLiftDecl PScriptPurpose where type PLifted PScriptPurpose = Plutus.ScriptPurpose
deriving via (DerivePConstantViaData Plutus.ScriptPurpose PScriptPurpose) instance PConstantDecl Plutus.ScriptPurpose
