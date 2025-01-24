{-# OPTIONS_GHC -Wno-orphans #-}

-- Mirrors the equivalent V3 module in plutus-ledger-api
module Plutarch.LedgerApi.V3.Contexts (
  PColdCommitteeCredential (..),
  PHotCommitteeCredential (..),
  PDRepCredential (..),
  PDRep (..),
  PDelegatee (..),
  PTxCert (..),
  PVoter (..),
  PVote (..),
  PGovernanceActionId (..),
  PCommittee (..),
  PConstitution (..),
  PProtocolVersion (..),
  PChangedParameters (..),
  PGovernanceAction (..),
  PProposalProcedure (..),
  PScriptPurpose (..),
  PScriptInfo (..),
  PTxInInfo (..),
  PTxInfo (..),
  PScriptContext (..),
  -- TODO: Add these
  -- pfindOwnInput,
  pfindDatum,
  pfindDatumHash,
  -- pfindTxInByTxOutRef,
  --  pfindContinuingInputs,
  --  pgetContinuingInputs,
  --  ptxSignedBy,
  --  ppubKeyOutputsAt,
  --  pvaluePaidTo,
  --  pvalueSpent,
  --  pvalueProduced,
  --  pownCurrencySymbol,
  --  pspendsOutput
) where

import GHC.Generics (Generic)
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Interval qualified as Interval
import Plutarch.LedgerApi.Utils (PMaybeData, PRationalData)
import Plutarch.LedgerApi.V1.Credential (PCredential)
import Plutarch.LedgerApi.V1.Crypto (PPubKeyHash)
import Plutarch.LedgerApi.V1.Scripts (
  PDatum,
  PDatumHash,
  PRedeemer,
  PScriptHash,
 )
import Plutarch.LedgerApi.V1.Time (PPosixTime)
import Plutarch.LedgerApi.V2.Tx (PTxOut)
import Plutarch.LedgerApi.V3.Tx (PTxId, PTxOutRef)
import Plutarch.LedgerApi.Value qualified as Value
import Plutarch.Prelude
import PlutusLedgerApi.V3 qualified as Plutus

-- | @since 3.1.0
newtype PColdCommitteeCredential (s :: S) = PColdCommitteeCredential (Term s PCredential)
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.0
      PlutusType
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )

-- | @since 3.1.0
instance DerivePlutusType PColdCommitteeCredential where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
deriving via
  DeriveDataPLiftable PColdCommitteeCredential Plutus.ColdCommitteeCredential
  instance
    PLiftable PColdCommitteeCredential

-- | @since 3.1.0
newtype PHotCommitteeCredential (s :: S) = PHotCommitteeCredential (Term s PCredential)
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.0
      PlutusType
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )

-- | @since 3.1.0
instance DerivePlutusType PHotCommitteeCredential where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
deriving via
  DeriveDataPLiftable PHotCommitteeCredential Plutus.HotCommitteeCredential
  instance
    PLiftable PHotCommitteeCredential

-- | @since 3.1.0
newtype PDRepCredential (s :: S) = PDRepCredential (Term s PCredential)
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.0
      PlutusType
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )

-- | @since 3.1.0
instance DerivePlutusType PDRepCredential where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
deriving via
  DeriveDataPLiftable PDRepCredential Plutus.DRepCredential
  instance
    PLiftable PDRepCredential

-- | @since 3.1.0
data PDRep (s :: S)
  = PDRep (Term s (PDataRecord '["_0" ':= PDRepCredential]))
  | PDRepAlwaysAbstain (Term s (PDataRecord '[]))
  | PDRepAlwaysNoConfidence (Term s (PDataRecord '[]))
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.0
      PlutusType
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )

-- | @since 3.1.0
instance DerivePlutusType PDRep where
  type DPTStrat _ = PlutusTypeData

-- | @since WIP
deriving via
  DeriveDataPLiftable PDRep Plutus.DRep
  instance
    PLiftable PDRep

-- | @since 3.1.0
data PDelegatee (s :: S)
  = PDelegStake (Term s (PDataRecord '["_0" ':= PPubKeyHash]))
  | PDelegVote (Term s (PDataRecord '["_0" ':= PDRep]))
  | PDelegStakeVote (Term s (PDataRecord '["_0" ':= PPubKeyHash, "_1" ':= PDRep]))
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.0
      PlutusType
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )

-- | @since 3.1.0
instance DerivePlutusType PDelegatee where
  type DPTStrat _ = PlutusTypeData

-- | @since WIP
deriving via
  DeriveDataPLiftable PDelegatee Plutus.Delegatee
  instance
    PLiftable PDelegatee

-- | @since 3.1.0
data PTxCert (s :: S)
  = PTxCertRegStaking (Term s (PDataRecord '["_0" ':= PCredential, "_1" ':= PMaybeData Value.PLovelace]))
  | PTxCertUnRegStaking (Term s (PDataRecord '["_0" ':= PCredential, "_1" ':= PMaybeData Value.PLovelace]))
  | PTxCertDelegStaking (Term s (PDataRecord '["_0" ':= PCredential, "_1" ':= PDelegatee]))
  | PTxCertRegDeleg (Term s (PDataRecord '["_0" ':= PCredential, "_1" ':= PDelegatee, "_2" ':= Value.PLovelace]))
  | PTxCertRegDRep (Term s (PDataRecord '["_0" ':= PDRepCredential, "_1" ':= Value.PLovelace]))
  | PTxCertUpdateDRep (Term s (PDataRecord '["_0" ':= PDRepCredential]))
  | PTxCertUnRegDRep (Term s (PDataRecord '["_0" ':= PDRepCredential, "_1" ':= Value.PLovelace]))
  | PTxCertPoolRegister (Term s (PDataRecord '["_0" ':= PPubKeyHash, "_1" ':= PPubKeyHash]))
  | PTxCertPoolRetire (Term s (PDataRecord '["_0" ':= PPubKeyHash, "_1" ':= PInteger]))
  | PTxCertAuthHotCommittee (Term s (PDataRecord '["_0" ':= PColdCommitteeCredential, "_1" ':= PHotCommitteeCredential]))
  | PTxCertResignColdCommittee (Term s (PDataRecord '["_0" ':= PColdCommitteeCredential]))
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.0
      PlutusType
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )

-- | @since 3.1.0
instance DerivePlutusType PTxCert where
  type DPTStrat _ = PlutusTypeData

-- | @since WIP
deriving via
  DeriveDataPLiftable PTxCert Plutus.TxCert
  instance
    PLiftable PTxCert

-- | @since 3.1.0
data PVoter (s :: S)
  = PCommitteeVoter (Term s (PDataRecord '["_0" ':= PHotCommitteeCredential]))
  | PDRepVoter (Term s (PDataRecord '["_0" ':= PDRepCredential]))
  | PStakePoolVoter (Term s (PDataRecord '["_0" ':= PPubKeyHash]))
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.0
      PlutusType
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )

-- | @since 3.1.0
instance DerivePlutusType PVoter where
  type DPTStrat _ = PlutusTypeData

-- | @since WIP
deriving via
  DeriveDataPLiftable PVoter Plutus.Voter
  instance
    PLiftable PVoter

-- | @since 3.1.0
data PVote (s :: S)
  = PVoteYes (Term s (PDataRecord '[]))
  | PVoteNo (Term s (PDataRecord '[]))
  | PAbstain (Term s (PDataRecord '[]))
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.0
      PlutusType
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )

-- | @since 3.1.0
instance DerivePlutusType PVote where
  type DPTStrat _ = PlutusTypeData

-- | @since WIP
deriving via
  DeriveDataPLiftable PVote Plutus.Vote
  instance
    PLiftable PVote

-- | @since 3.1.0
newtype PGovernanceActionId (s :: S)
  = PGovernanceActionId (Term s (PDataRecord '["txId" ':= PTxId, "govActionIx" ':= PInteger]))
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.0
      PlutusType
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )

-- | @since 3.1.0
instance DerivePlutusType PGovernanceActionId where
  type DPTStrat _ = PlutusTypeData

-- | @since WIP
deriving via
  DeriveDataPLiftable PGovernanceActionId Plutus.GovernanceActionId
  instance
    PLiftable PGovernanceActionId

-- TODO: Investigate what guarantees this provides on the Map, if any

-- | @since 3.1.0
newtype PCommittee (s :: S)
  = PCommittee
      ( Term
          s
          ( PDataRecord
              '[ "members" ':= AssocMap.PMap 'AssocMap.Unsorted PColdCommitteeCredential PInteger
               , "quorum" ':= PRationalData
               ]
          )
      )
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.0
      PlutusType
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PDataFields
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )

-- | @since 3.1.0
instance DerivePlutusType PCommittee where
  type DPTStrat _ = PlutusTypeData

-- | @since WIP
deriving via
  DeriveDataPLiftable PCommittee Plutus.Committee
  instance
    PLiftable PCommittee

{- | A constitution, omitting the optional anchor.

@since 3.1.0
-}
newtype PConstitution (s :: S) = PConstitution (Term s (PDataRecord '["_0" := PMaybeData PScriptHash]))
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.0
      PlutusType
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )

-- | @since 3.1.0
instance DerivePlutusType PConstitution where
  type DPTStrat _ = PlutusTypeData

-- | @since WIP
deriving via
  DeriveDataPLiftable PConstitution Plutus.Constitution
  instance
    PLiftable PConstitution

-- | @since 3.1.0
newtype PProtocolVersion (s :: S)
  = PProtocolVersion (Term s (PDataRecord '["major" ':= PInteger, "minor" ':= PInteger]))
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.0
      PlutusType
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PDataFields
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )

-- | @since 3.1.0
instance DerivePlutusType PProtocolVersion where
  type DPTStrat _ = PlutusTypeData

-- | @since WIP
deriving via
  DeriveDataPLiftable PProtocolVersion Plutus.ProtocolVersion
  instance
    PLiftable PProtocolVersion

-- | @since 3.1.0
newtype PChangedParameters (s :: S)
  = PChangedParameters (Term s PData)
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.0
      PlutusType
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )

-- | @since 3.1.0
instance DerivePlutusType PChangedParameters where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
deriving via
  DeriveDataPLiftable PChangedParameters Plutus.ChangedParameters
  instance
    PLiftable PChangedParameters

-- | @since 3.1.0
data PGovernanceAction (s :: S)
  = PParameterChange (Term s (PDataRecord '["_0" ':= PMaybeData PGovernanceActionId, "_1" ':= PChangedParameters, "_2" ':= PMaybeData PScriptHash]))
  | PHardForkInitiation (Term s (PDataRecord '["_0" ':= PMaybeData PGovernanceActionId, "_1" ':= PProtocolVersion]))
  | PTreasuryWithdrawals (Term s (PDataRecord '["_0" ':= AssocMap.PMap 'AssocMap.Unsorted PCredential Value.PLovelace, "_1" ':= PMaybeData PScriptHash]))
  | PNoConfidence (Term s (PDataRecord '["_0" ':= PMaybeData PGovernanceActionId]))
  | PUpdateCommittee
      ( Term
          s
          ( PDataRecord
              '[ "_0" ':= PMaybeData PGovernanceActionId
               , "_1" ':= PBuiltinList (PAsData PColdCommitteeCredential)
               , "_2" ':= AssocMap.PMap 'AssocMap.Unsorted PColdCommitteeCredential PInteger
               , "_3" ':= PRationalData
               ]
          )
      )
  | PNewConstitution (Term s (PDataRecord '["_0" ':= PMaybeData PGovernanceActionId, "_1" ':= PConstitution]))
  | PInfoAction (Term s (PDataRecord '[]))
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.0
      PlutusType
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )

-- | @since 3.1.0
instance DerivePlutusType PGovernanceAction where
  type DPTStrat _ = PlutusTypeData

-- | @since WIP
deriving via
  DeriveDataPLiftable PGovernanceAction Plutus.GovernanceAction
  instance
    PLiftable PGovernanceAction

-- | @since 3.1.0
newtype PProposalProcedure (s :: S)
  = PProposalProcedure
      ( Term
          s
          ( PDataRecord
              '[ "deposit" ':= Value.PLovelace
               , "returnAddr" ':= PCredential
               , "governanceAction" ':= PGovernanceAction
               ]
          )
      )
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.0
      PlutusType
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PDataFields
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )

-- | @since 3.1.0
instance DerivePlutusType PProposalProcedure where
  type DPTStrat _ = PlutusTypeData

-- | @since WIP
deriving via
  DeriveDataPLiftable PProposalProcedure Plutus.ProposalProcedure
  instance
    PLiftable PProposalProcedure

-- | @since 2.0.0
data PScriptPurpose (s :: S)
  = PMinting (Term s (PDataRecord '["_0" ':= Value.PCurrencySymbol]))
  | PSpending (Term s (PDataRecord '["_0" ':= PTxOutRef]))
  | -- | @since 3.1.0
    PRewarding (Term s (PDataRecord '["_0" ':= PCredential]))
  | PCertifying (Term s (PDataRecord '["_0" ':= PInteger, "_1" ':= PTxCert]))
  | -- | @since 3.1.0
    PVoting (Term s (PDataRecord '["_0" ':= PVoter]))
  | -- | @since 3.1.0
    PProposing (Term s (PDataRecord '["_0" ':= PInteger, "_1" ':= PProposalProcedure]))
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType PScriptPurpose where
  type DPTStrat _ = PlutusTypeData

-- | @since WIP
deriving via
  DeriveDataPLiftable PScriptPurpose Plutus.ScriptPurpose
  instance
    PLiftable PScriptPurpose

-- | @since 3.1.0
data PScriptInfo (s :: S)
  = PMintingScript (Term s (PDataRecord '["_0" ':= Value.PCurrencySymbol]))
  | PSpendingScript (Term s (PDataRecord '["_0" ':= PTxOutRef, "_1" ':= PMaybeData PDatum]))
  | PRewardingScript (Term s (PDataRecord '["_0" ':= PCredential]))
  | PCertifyingScript (Term s (PDataRecord '["_0" ':= PInteger, "_1" ':= PTxCert]))
  | PVotingScript (Term s (PDataRecord '["_0" ':= PVoter]))
  | PProposingScript (Term s (PDataRecord '["_0" ':= PInteger, "_1" ':= PProposalProcedure]))
  deriving stock
    ( -- | @since 3.1.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.0
      PlutusType
    , -- | @since 3.1.0
      PIsData
    , -- | @since 3.1.0
      PEq
    , -- | @since 3.1.0
      PShow
    )

-- | @since 3.1.0
instance DerivePlutusType PScriptInfo where
  type DPTStrat _ = PlutusTypeData

-- | @since WIP
deriving via
  DeriveDataPLiftable PScriptInfo Plutus.ScriptInfo
  instance
    PLiftable PScriptInfo

{- | An input of the pending transaction.

@since 2.0.0
-}
newtype PTxInInfo (s :: S)
  = PTxInInfo
      ( Term
          s
          ( PDataRecord
              '[ "outRef" ':= PTxOutRef
               , "resolved" ':= PTxOut
               ]
          )
      )
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PDataFields
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType PTxInInfo where
  type DPTStrat _ = PlutusTypeData

-- | @since WIP
deriving via
  DeriveDataPLiftable PTxInInfo Plutus.TxInInfo
  instance
    PLiftable PTxInInfo

-- A pending transaction. This is the view as seen by a validator script.
--
-- @since WIP
newtype PTxInfo (s :: S)
  = PTxInfo
      ( Term
          s
          ( PDataRecord
              '[ "inputs" ':= PBuiltinList (PAsData PTxInInfo)
               , "referenceInputs" ':= PBuiltinList (PAsData PTxInInfo)
               , "outputs" ':= PBuiltinList (PAsData PTxOut)
               , "fee" ':= Value.PLovelace
               , "mint" ':= Value.PValue 'AssocMap.Sorted 'Value.NonZero -- value minted by transaction
               , "txCerts" ':= PBuiltinList (PAsData PTxCert)
               , "wdrl" ':= AssocMap.PMap 'AssocMap.Unsorted PCredential Value.PLovelace -- Staking withdrawals
               , "validRange" ':= Interval.PInterval PPosixTime
               , "signatories" ':= PBuiltinList (PAsData PPubKeyHash)
               , "redeemers" ':= AssocMap.PMap 'AssocMap.Unsorted PScriptPurpose PRedeemer
               , "data" ':= AssocMap.PMap 'AssocMap.Unsorted PDatumHash PDatum
               , "id" ':= PTxId -- hash of the pending transaction
               , "votes" ':= AssocMap.PMap 'AssocMap.Unsorted PVoter (AssocMap.PMap 'AssocMap.Unsorted PGovernanceActionId PVote)
               , "proposalProcedures" ':= PBuiltinList (PAsData PProposalProcedure)
               , "currentTreasuryAmount" ':= PMaybeData Value.PLovelace
               , "treasuryDonation" ':= PMaybeData Value.PLovelace
               ]
          )
      )
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PDataFields
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType PTxInfo where
  type DPTStrat _ = PlutusTypeData

-- | @since WIP
deriving via
  DeriveDataPLiftable PTxInfo Plutus.TxInfo
  instance
    PLiftable PTxInfo

-- | @since 3.1.0
newtype PScriptContext (s :: S)
  = PScriptContext
      ( Term
          s
          ( PDataRecord
              '[ "txInfo" ':= PTxInfo
               , "redeemer" ':= PRedeemer
               , "scriptInfo" ':= PScriptInfo
               ]
          )
      )
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PDataFields
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType PScriptContext where
  type DPTStrat _ = PlutusTypeData

-- | @since WIP
deriving via
  DeriveDataPLiftable PScriptContext Plutus.ScriptContext
  instance
    PLiftable PScriptContext

{- | Find the datum corresponding to a datum hash, if there is one.

@since 3.1.0
-}
pfindDatum ::
  forall (s :: S).
  Term s (PDatumHash :--> PTxInfo :--> PMaybe PDatum)
pfindDatum = phoistAcyclic $ plam $ \dh txI ->
  let infoData = pfield @"data" # txI
   in AssocMap.plookup # dh # infoData

{- | Find the hash of a datum if it's part of the pending transaction's hashes.

@since 3.1.0
-}
pfindDatumHash ::
  forall (s :: S).
  Term s (PDatum :--> PTxInfo :--> PMaybe PDatumHash)
pfindDatumHash = phoistAcyclic $ plam $ \d txI ->
  let infoData = pfield @"data" # txI
   in pmatch infoData $ \(AssocMap.PMap ell) ->
        pmatch (pfind # (matches # d) # ell) $ \case
          PNothing -> pcon PNothing
          PJust p -> pcon . PJust . pfromData $ pfstBuiltin # p
  where
    matches ::
      forall (s' :: S).
      Term
        s'
        ( PDatum
            :--> PBuiltinPair (PAsData PDatumHash) (PAsData PDatum)
            :--> PBool
        )
    matches = phoistAcyclic $ plam $ \needle p ->
      needle #== pfromData (psndBuiltin # p)
