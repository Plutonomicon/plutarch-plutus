{-# OPTIONS_GHC -Wno-orphans #-}

-- Mirrors the equivalent V4 module in plutus-ledger-api
module Plutarch.LedgerApi.V4.Contexts (
  PGovernanceActionId (..),
  PProtocolVersion (..),
  PConstitution (..),
  PCommittee (..),
  PGovernanceAction (..),
  PProposalProcedure (..),
  PAccountBalanceInterval (..),
  PAccountBalanceIntervals (..),
  PTxCert (..),
  PScriptPurpose (..),
  PTxInInfo (..),
  PTxInfo (..),
  PTopTxInfoSimplified (..),
  PTopTxInfo (..),
  PScriptInfo (..),
  PScriptContext (..),
) where

import Data.Bifunctor (bimap)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Internal.Lift (
  LiftError (CouldNotDecodeData),
  punsafeCoercePLifted,
 )
import Plutarch.LedgerApi.AssocMap (PUnsortedMap)
import Plutarch.LedgerApi.Utils (PMaybeData)
import Plutarch.LedgerApi.V1.Credential (PCredential)
import Plutarch.LedgerApi.V1.Crypto (PPubKeyHash)
import Plutarch.LedgerApi.V1.Scripts (
  PDatum,
  PDatumHash,
  PRedeemer,
  PScriptHash,
 )
import Plutarch.LedgerApi.V3.Contexts (
  PChangedParameters,
  PColdCommitteeCredential,
  PDRepCredential,
  PDelegatee,
  PHotCommitteeCredential,
  PVote,
  PVoter,
 )
import Plutarch.LedgerApi.V3.MintValue (PMintValue)
import Plutarch.LedgerApi.V3.Tx (PTxId, PTxOutRef)
import Plutarch.LedgerApi.V4.Address (PAccountId)
import Plutarch.LedgerApi.V4.Ratio (PRationalData)
import Plutarch.LedgerApi.V4.Time (PPosixTimeRange)
import Plutarch.LedgerApi.V4.Tx (PTxOut)
import Plutarch.LedgerApi.Value (PCurrencySymbol, PLovelace)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeBuiltin, punsafeCoerce)
import PlutusCore qualified as PLC
import PlutusLedgerApi.V4 qualified as Plutus
import PlutusTx.AssocMap qualified as PlutusMap

-- | @since 3.8.0
data PGovernanceActionId (s :: S)
  = PGovernanceActionId (Term s (PAsData PTxId)) (Term s (PAsData PInteger))
  deriving stock
    ( -- | @since 3.8.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.8.0
      SOP.Generic
    , -- | @since 3.8.0
      PIsData
    , -- | @since 3.8.0
      PEq
    , -- | @since 3.8.0
      PShow
    )
  deriving
    ( -- | @since 3.8.0
      PlutusType
    , -- | @since 3.8.0
      PValidateData
    )
    via (DeriveAsDataRec PGovernanceActionId)

-- | @since 3.8.0
instance PLiftable PGovernanceActionId where
  type AsHaskell PGovernanceActionId = Plutus.GovernanceActionId
  type PlutusRepr PGovernanceActionId = Plutus.Data
  haskToRepr = Plutus.toData
  reprToHask = maybe (Left CouldNotDecodeData) Right . Plutus.fromData
  plutToRepr t = fmap Plutus.List $ plutToReprUni @(PBuiltinList PData) $ punsafeCoercePLifted t
  reprToPlut = \case
    Plutus.List ell -> punsafeCoercePLifted (reprToPlutUni @(PBuiltinList PData) ell)
    _ -> error "Impossible: PGovernanceActionId is list-represented."

-- | @since 3.8.0
instance PTryFrom PData (PAsData PGovernanceActionId)

-- | @since 3.8.0
data PProtocolVersion (s :: S) = PProtocolVersion
  { pprotocolVersion'major :: Term s (PAsData PInteger)
  , pprotocolVersion'minor :: Term s (PAsData PInteger)
  }
  deriving stock
    ( -- | @since 3.8.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.8.0
      SOP.Generic
    , -- | @since 3.8.0
      PIsData
    , -- | @since 3.8.0
      PEq
    , -- | @since 3.8.0
      PShow
    )
  deriving
    ( -- | @since 3.8.0
      PlutusType
    , -- | @since 3.8.0
      PValidateData
    )
    via (DeriveAsDataRec PProtocolVersion)

-- | @since 3.8.0
instance PLiftable PProtocolVersion where
  type AsHaskell PProtocolVersion = Plutus.ProtocolVersion
  type PlutusRepr PProtocolVersion = Plutus.Data
  haskToRepr = Plutus.toData
  reprToHask = maybe (Left CouldNotDecodeData) Right . Plutus.fromData
  plutToRepr t = fmap Plutus.List $ plutToReprUni @(PBuiltinList PData) $ punsafeCoercePLifted t
  reprToPlut = \case
    Plutus.List ell -> punsafeCoercePLifted (reprToPlutUni @(PBuiltinList PData) ell)
    _ -> error "Impossible: PProtocolVersion is list-represented."

-- | @since 3.8.0
instance PTryFrom PData (PAsData PProtocolVersion)

{- | A constitution, omitting the optional anchor.

@since 3.8.0
-}
newtype PConstitution (s :: S) = PConstitution (Term s (PMaybeData PScriptHash))
  deriving stock
    ( -- | @since 3.8.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.8.0
      SOP.Generic
    , -- | @since 3.8.0
      PIsData
    , -- | @since 3.8.0
      PEq
    , -- | @since 3.8.0
      PShow
    )
  deriving
    ( -- | @since 3.8.0
      PlutusType
    , -- | @since 3.8.0
      PValidateData
    )
    via (DeriveAsDataRec PConstitution)

-- | @since 3.8.0
instance PLiftable PConstitution where
  type AsHaskell PConstitution = Plutus.Constitution
  type PlutusRepr PConstitution = Plutus.Data
  haskToRepr = Plutus.toData
  reprToHask = maybe (Left CouldNotDecodeData) Right . Plutus.fromData
  plutToRepr t = fmap Plutus.List $ plutToReprUni @(PBuiltinList PData) $ punsafeCoercePLifted t
  reprToPlut = \case
    Plutus.List ell -> punsafeCoercePLifted (reprToPlutUni @(PBuiltinList PData) ell)
    _ -> error "Impossible: PConstitution is list-represented."

-- | @since 3.8.0
instance PTryFrom PData (PAsData PConstitution)

-- | @since 3.8.0
data PCommittee (s :: S) = PCommittee
  { pcommittee'members :: Term s (PAsData (PUnsortedMap PColdCommitteeCredential PInteger))
  , pcommittee'quorum :: Term s (PAsData PRationalData)
  }
  deriving stock
    ( -- | @since 3.8.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.8.0
      SOP.Generic
    , -- | @since 3.8.0
      PIsData
    , -- | @since 3.8.0
      PEq
    , -- | @since 3.8.0
      PShow
    )
  deriving
    ( -- | @since 3.8.0
      PlutusType
    , -- | @since 3.8.0
      PValidateData
    )
    via (DeriveAsDataRec PCommittee)

-- | @since 3.8.0
instance PLiftable PCommittee where
  type AsHaskell PCommittee = Plutus.Committee
  type PlutusRepr PCommittee = Plutus.Data
  haskToRepr = Plutus.toData
  reprToHask = maybe (Left CouldNotDecodeData) Right . Plutus.fromData
  plutToRepr t = fmap Plutus.List $ plutToReprUni @(PBuiltinList PData) $ punsafeCoercePLifted t
  reprToPlut = \case
    Plutus.List ell -> punsafeCoercePLifted (reprToPlutUni @(PBuiltinList PData) ell)
    _ -> error "Impossible: PCommittee is list-represented."

-- | @since 3.8.0
instance PTryFrom PData (PAsData PCommittee)

-- | @since 3.8.0
data PGovernanceAction (s :: S)
  = PParameterChange (Term s (PMaybeData PGovernanceActionId)) (Term s PChangedParameters) (Term s (PMaybeData PScriptHash))
  | PHardForkInitiation (Term s (PMaybeData PGovernanceActionId)) (Term s (PAsData PProtocolVersion))
  | PTreasuryWithdrawals
      (Term s (PAsData (PUnsortedMap PCredential PLovelace)))
      (Term s (PMaybeData PScriptHash))
  | PNoConfidence (Term s (PMaybeData PGovernanceActionId))
  | PUpdateCommittee
      (Term s (PMaybeData PGovernanceActionId))
      (Term s (PAsData (PBuiltinList (PAsData PColdCommitteeCredential))))
      (Term s (PAsData (PUnsortedMap PColdCommitteeCredential PInteger)))
      (Term s (PAsData PRationalData))
  | PNewConstitution (Term s (PMaybeData PGovernanceActionId)) (Term s (PAsData PConstitution))
  | PInfoAction
  deriving stock
    ( -- | @since 3.8.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.3.0
      SOP.Generic
    , -- | @since 3.8.0
      PIsData
    , -- | @since 3.8.0
      PEq
    , -- | @since 3.8.0
      PShow
    )
  deriving
    ( -- | @since 3.8.0
      PlutusType
    , -- | @since 3.8.0
      PValidateData
    )
    via (DeriveAsDataStruct PGovernanceAction)

-- | @since 3.8.0
deriving via
  DeriveDataPLiftable PGovernanceAction Plutus.GovernanceAction
  instance
    PLiftable PGovernanceAction

-- | @since 3.8.0
instance PTryFrom PData (PAsData PGovernanceAction)

-- | @since 3.8.0
data PProposalProcedure (s :: S) = PProposalProcedure
  { pproposalProcedure'deposit :: Term s (PAsData PLovelace)
  , pproposalProcedure'returnAddr :: Term s PCredential
  , pproposalProcedure'governanceAction :: Term s PGovernanceAction
  }
  deriving stock
    ( -- | @since 3.8.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.8.0
      SOP.Generic
    , -- | @since 3.8.0
      PIsData
    , -- | @since 3.8.0
      PEq
    , -- | @since 3.8.0
      PShow
    )
  deriving
    ( -- | @since 3.8.0
      PlutusType
    , -- | @since 3.8.0
      PValidateData
    )
    via (DeriveAsDataRec PProposalProcedure)

-- | @since 3.8.0
instance PLiftable PProposalProcedure where
  type AsHaskell PProposalProcedure = Plutus.ProposalProcedure
  type PlutusRepr PProposalProcedure = Plutus.Data
  haskToRepr = Plutus.toData
  reprToHask = maybe (Left CouldNotDecodeData) Right . Plutus.fromData
  plutToRepr t = fmap Plutus.List $ plutToReprUni @(PBuiltinList PData) $ punsafeCoercePLifted t
  reprToPlut = \case
    Plutus.List ell -> punsafeCoercePLifted (reprToPlutUni @(PBuiltinList PData) ell)
    _ -> error "Impossible: PProposalProcedure is list-represented."

-- | @since 3.8.0
instance PTryFrom PData (PAsData PProposalProcedure)

-- | @since 3.8.0
data PAccountBalanceInterval (s :: S)
  = PAccountBalanceLowerBound (Term s (PAsData PLovelace))
  | PAccountBalanceUpperBound (Term s (PAsData PLovelace))
  | PAccountBalanceBothBounds (Term s (PAsData PLovelace)) (Term s (PAsData PLovelace))
  | PAccountBalanceExact (Term s (PAsData PLovelace))
  deriving stock
    ( -- | @since 3.8.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.8.0
      SOP.Generic
    , -- | @since 3.8.0
      PIsData
    , -- | @since 3.8.0
      PEq
    , -- | @since 3.8.0
      PShow
    )
  deriving
    ( -- | @since 3.8.0
      PlutusType
    , -- | @since 3.8.0
      PValidateData
    )
    via (DeriveAsDataStruct PAccountBalanceInterval)

-- | @since 3.8.0
deriving via
  DeriveDataPLiftable PAccountBalanceInterval Plutus.AccountBalanceInterval
  instance
    PLiftable PAccountBalanceInterval

-- | @since 3.8.0
instance PTryFrom PData (PAsData PAccountBalanceInterval)

-- | @since 3.8.0
newtype PAccountBalanceIntervals (s :: S)
  = PAccountBalanceIntervals (Term s (PUnsortedMap PAccountId PAccountBalanceInterval))
  deriving stock
    ( -- | @since 3.8.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.8.0
      SOP.Generic
    , -- | @since 3.8.0
      PShow
    )
  deriving
    ( -- | @since 3.8.0
      PlutusType
    )
    via (DeriveNewtypePlutusType PAccountBalanceIntervals)
  deriving
    ( -- | @since 3.8.0
      PValidateData
    )
    via (DeriveNewtypePValidateData PAccountBalanceIntervals (PUnsortedMap PAccountId PAccountBalanceInterval))

-- | @since 3.8.0
instance PIsData PAccountBalanceIntervals where
  pfromDataImpl x = punsafeCoerce $ pasMap # pforgetData x
  pdataImpl x = punsafeBuiltin PLC.MapData # x

-- | @since 3.8.0
instance PLiftable PAccountBalanceIntervals where
  type AsHaskell PAccountBalanceIntervals = Plutus.AccountBalanceIntervals
  type PlutusRepr PAccountBalanceIntervals = [(Plutus.Data, Plutus.Data)]
  haskToRepr (Plutus.AccountBalanceIntervals m) =
    fmap (bimap Plutus.toData Plutus.toData) . PlutusMap.toList $ m
  reprToHask x =
    Plutus.AccountBalanceIntervals . PlutusMap.unsafeFromList
      <$> traverse
        ( \(k, v) ->
            (,)
              <$> (maybe (Left CouldNotDecodeData) Right . Plutus.fromData) k
              <*> (maybe (Left CouldNotDecodeData) Right . Plutus.fromData) v
        )
        x
  reprToPlut = reprToPlutUni
  plutToRepr = plutToReprUni

-- | @since 3.8.0
instance PTryFrom PData (PAsData PAccountBalanceIntervals)

-- | @since 3.8.0
data PTxCert (s :: S)
  = PTxCertRegAccount (Term s (PAsData PAccountId)) (Term s (PAsData PLovelace))
  | PTxCertUnRegAccount (Term s (PAsData PAccountId)) (Term s (PAsData PLovelace))
  | PTxCertDelegAccount (Term s (PAsData PAccountId)) (Term s (PAsData PDelegatee))
  | PTxCertRegAccountDeleg (Term s (PAsData PAccountId)) (Term s (PAsData PDelegatee)) (Term s (PAsData PLovelace))
  | PTxCertRegDRep (Term s (PAsData PDRepCredential)) (Term s (PAsData PLovelace))
  | PTxCertUpdateDRep (Term s (PAsData PDRepCredential))
  | PTxCertUnRegDRep (Term s (PAsData PDRepCredential)) (Term s (PAsData PLovelace))
  | PTxCertPoolRegister (Term s (PAsData PPubKeyHash)) (Term s (PAsData PPubKeyHash))
  | PTxCertPoolRetire (Term s (PAsData PPubKeyHash)) (Term s (PAsData PInteger))
  | PTxCertAuthHotCommittee (Term s (PAsData PColdCommitteeCredential)) (Term s (PAsData PHotCommitteeCredential))
  | PTxCertResignColdCommittee (Term s (PAsData PColdCommitteeCredential))
  deriving stock
    ( -- | @since 3.8.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.8.0
      SOP.Generic
    , -- | @since 3.8.0
      PIsData
    , -- | @since 3.8.0
      PEq
    , -- | @since 3.8.0
      PShow
    )
  deriving
    ( -- | @since 3.8.0
      PlutusType
    , -- | @since 3.8.0
      PValidateData
    )
    via (DeriveAsDataStruct PTxCert)

-- | @since 3.8.0
deriving via
  DeriveDataPLiftable PTxCert Plutus.TxCert
  instance
    PLiftable PTxCert

-- | @since 3.8.0
instance PTryFrom PData (PAsData PTxCert)

-- | @since 3.8.0
data PScriptPurpose (s :: S)
  = PMinting (Term s (PAsData PScriptHash)) (Term s (PAsData PCurrencySymbol))
  | PSpending (Term s (PAsData PScriptHash)) (Term s (PAsData PTxOutRef))
  | PWithdrawing (Term s (PAsData PScriptHash)) (Term s (PAsData PCredential))
  | PCertifying (Term s (PAsData PScriptHash)) (Term s (PAsData PInteger)) (Term s (PAsData PTxCert))
  | PVoting (Term s (PAsData PScriptHash)) (Term s (PAsData PVoter))
  | PProposing (Term s (PAsData PScriptHash)) (Term s (PAsData PInteger)) (Term s (PAsData PProposalProcedure))
  | PGuarding (Term s (PAsData PScriptHash)) (Term s (PAsData PInteger))
  deriving stock
    ( -- | @since 3.8.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.8.0
      SOP.Generic
    , -- | @since 3.8.0
      PIsData
    , -- | @since 3.8.0
      PEq
    , -- | @since 3.8.0
      PShow
    )
  deriving
    ( -- | @since 3.8.0
      PlutusType
    , -- | @since 3.8.0
      PValidateData
    )
    via (DeriveAsDataStruct PScriptPurpose)

-- | @since 3.8.0
deriving via
  DeriveDataPLiftable PScriptPurpose Plutus.ScriptPurpose
  instance
    PLiftable PScriptPurpose

-- | @since 3.8.0
instance PTryFrom PData (PAsData PScriptPurpose)

-- | @since 3.8.0
data PTxInInfo (s :: S) = PTxInInfo
  { ptxInInfo'outRef :: Term s (PAsData PTxOutRef)
  , ptxInInfo'resolved :: Term s (PAsData PTxOut)
  }
  deriving stock
    ( -- | @since 3.8.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.8.0
      SOP.Generic
    , -- | @since 3.8.0
      PIsData
    , -- | @since 3.8.0
      PEq
    , -- | @since 3.8.0
      PShow
    )
  deriving
    ( -- | @since 3.8.0
      PlutusType
    , -- | @since 3.8.0
      PValidateData
    )
    via (DeriveAsDataRec PTxInInfo)

-- | @since 3.8.0
instance PLiftable PTxInInfo where
  type AsHaskell PTxInInfo = Plutus.TxInInfo
  type PlutusRepr PTxInInfo = Plutus.Data
  haskToRepr = Plutus.toData
  reprToHask = maybe (Left CouldNotDecodeData) Right . Plutus.fromData
  plutToRepr t = fmap Plutus.List $ plutToReprUni @(PBuiltinList PData) $ punsafeCoercePLifted t
  reprToPlut = \case
    Plutus.List ell -> punsafeCoercePLifted (reprToPlutUni @(PBuiltinList PData) ell)
    _ -> error "Impossible: PTxInInfo is list-represented."

-- | @since 3.8.0
instance PTryFrom PData (PAsData PTxInInfo)

-- | @since 3.8.0
data PTxInfo (s :: S) = PTxInfo
  { ptxInfo'id :: Term s (PAsData PTxId)
  , ptxInfo'subTxIx :: Term s (PMaybeData PInteger)
  , ptxInfo'inputs :: Term s (PAsData (PBuiltinList (PAsData PTxInInfo)))
  , ptxInfo'referenceInputs :: Term s (PAsData (PBuiltinList (PAsData PTxInInfo)))
  , ptxInfo'outputs :: Term s (PAsData (PBuiltinList (PAsData PTxOut)))
  , ptxInfo'mint :: Term s (PAsData PMintValue)
  , ptxInfo'txCerts :: Term s (PAsData (PBuiltinList (PAsData PTxCert)))
  , ptxInfo'withdrawals :: Term s (PAsData (PUnsortedMap PCredential PLovelace))
  , ptxInfo'directDeposits :: Term s (PAsData (PUnsortedMap PCredential PLovelace))
  , ptxInfo'accountBalanceIntervals :: Term s (PAsData PAccountBalanceIntervals)
  , ptxInfo'validRange :: Term s (PAsData PPosixTimeRange)
  , ptxInfo'guards :: Term s (PAsData (PBuiltinList (PAsData PCredential)))
  , ptxInfo'requiredTopLevelGuards :: Term s (PAsData (PUnsortedMap PCredential (PMaybeData PDatum)))
  , ptxInfo'redeemers :: Term s (PAsData (PUnsortedMap PScriptPurpose PRedeemer))
  , ptxInfo'data :: Term s (PAsData (PUnsortedMap PDatumHash PDatum))
  , ptxInfo'votes :: Term s (PAsData (PUnsortedMap PVoter (PUnsortedMap PGovernanceActionId PVote)))
  , ptxInfo'proposalProcedures :: Term s (PAsData (PBuiltinList (PAsData PProposalProcedure)))
  , ptxInfo'currentTreasuryAmount :: Term s (PMaybeData PLovelace)
  , ptxInfo'treasuryDonation :: Term s (PAsData PLovelace)
  }
  deriving stock
    ( -- | @since 3.8.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.8.0
      SOP.Generic
    , -- | @since 3.8.0
      PIsData
    , -- | @since 3.8.0
      PEq
    , -- | @since 3.8.0
      PShow
    )
  deriving
    ( -- | @since 3.8.0
      PlutusType
    , -- | @since 3.8.0
      PValidateData
    )
    via (DeriveAsDataRec PTxInfo)

-- | @since 3.8.0
instance PLiftable PTxInfo where
  type AsHaskell PTxInfo = Plutus.TxInfo
  type PlutusRepr PTxInfo = Plutus.Data
  haskToRepr = Plutus.toData
  reprToHask = maybe (Left CouldNotDecodeData) Right . Plutus.fromData
  plutToRepr t = fmap Plutus.List $ plutToReprUni @(PBuiltinList PData) $ punsafeCoercePLifted t
  reprToPlut = \case
    Plutus.List ell -> punsafeCoercePLifted (reprToPlutUni @(PBuiltinList PData) ell)
    _ -> error "Impossible: PTxInfo is list-represented."

-- | @since 3.8.0
instance PTryFrom PData (PAsData PTxInfo)

-- | @since 3.8.0
data PTopTxInfoSimplified (s :: S) = PTopTxInfoSimplified
  { pttis'ids :: Term s (PAsData (PBuiltinList (PAsData PTxId)))
  , pttis'inputs :: Term s (PAsData (PBuiltinList (PAsData PTxInInfo)))
  , pttis'referenceInputs :: Term s (PAsData (PBuiltinList (PAsData PTxInInfo)))
  , pttis'outputs :: Term s (PAsData (PBuiltinList (PAsData PTxOut)))
  , pttis'mints :: Term s (PAsData PMintValue)
  , pttis'burns :: Term s (PAsData PMintValue)
  , pttis'txCerts :: Term s (PAsData (PBuiltinList (PAsData PTxCert)))
  , pttis'withdrawals :: Term s (PAsData (PUnsortedMap PCredential PLovelace))
  , pttis'directDeposits :: Term s (PAsData (PUnsortedMap PCredential PLovelace))
  , pttis'validRange :: Term s (PAsData PPosixTimeRange)
  , pttis'guards :: Term s (PAsData (PBuiltinList (PAsData PCredential)))
  , pttis'requiredTopLevelGuards :: Term s (PAsData (PBuiltinList (PAsData PCredential)))
  , pttis'redeemerHashes :: Term s (PAsData (PBuiltinList (PAsData PScriptHash)))
  , pttis'data :: Term s (PAsData (PUnsortedMap PDatumHash PDatum))
  , pttis'votes :: Term s (PAsData (PUnsortedMap PVoter (PUnsortedMap PGovernanceActionId PVote)))
  , pttis'proposalProcedures :: Term s (PAsData (PBuiltinList (PAsData PProposalProcedure)))
  , pttis'currentTreasuryAmount :: Term s (PMaybeData PLovelace)
  , pttis'treasuryDonations :: Term s (PAsData PLovelace)
  }
  deriving stock
    ( -- | @since 3.8.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.8.0
      SOP.Generic
    , -- | @since 3.8.0
      PIsData
    , -- | @since 3.8.0
      PEq
    , -- | @since 3.8.0
      PShow
    )
  deriving
    ( -- | @since 3.8.0
      PlutusType
    , -- | @since 3.8.0
      PValidateData
    )
    via (DeriveAsDataRec PTopTxInfoSimplified)

-- | @since 3.8.0
instance PLiftable PTopTxInfoSimplified where
  type AsHaskell PTopTxInfoSimplified = Plutus.TopTxInfoSimplified
  type PlutusRepr PTopTxInfoSimplified = Plutus.Data
  haskToRepr = Plutus.toData
  reprToHask = maybe (Left CouldNotDecodeData) Right . Plutus.fromData
  plutToRepr t = fmap Plutus.List $ plutToReprUni @(PBuiltinList PData) $ punsafeCoercePLifted t
  reprToPlut = \case
    Plutus.List ell -> punsafeCoercePLifted (reprToPlutUni @(PBuiltinList PData) ell)
    _ -> error "Impossible: PTopTxInfoSimplified is list-represented."

-- | @since 3.8.0
instance PTryFrom PData (PAsData PTopTxInfoSimplified)

-- | @since 3.8.0
data PTopTxInfo (s :: S) = PTopTxInfo
  { topTxInfo'subTransactions :: Term s (PAsData (PBuiltinList (PAsData PTxInfo)))
  , topTxInfo'datums :: Term s (PAsData (PUnsortedMap PTxId PDatum))
  , topTxInfo'startingBalanceIntervals :: Term s (PAsData PAccountBalanceIntervals)
  , topTxInfo'simplified :: Term s (PAsData PTopTxInfoSimplified)
  }
  deriving stock
    ( -- | @since 3.8.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.8.0
      SOP.Generic
    , -- | @since 3.8.0
      PIsData
    , -- | @since 3.8.0
      PEq
    , -- | @since 3.8.0
      PShow
    )
  deriving
    ( -- | @since 3.8.0
      PlutusType
    , -- | @since 3.8.0
      PValidateData
    )
    via (DeriveAsDataRec PTopTxInfo)

-- | @since 3.8.0
instance PLiftable PTopTxInfo where
  type AsHaskell PTopTxInfo = Plutus.TopTxInfo
  type PlutusRepr PTopTxInfo = Plutus.Data
  haskToRepr = Plutus.toData
  reprToHask = maybe (Left CouldNotDecodeData) Right . Plutus.fromData
  plutToRepr t = fmap Plutus.List $ plutToReprUni @(PBuiltinList PData) $ punsafeCoercePLifted t
  reprToPlut = \case
    Plutus.List ell -> punsafeCoercePLifted (reprToPlutUni @(PBuiltinList PData) ell)
    _ -> error "Impossible: PTopTxInfo is list-represented."

-- | @since 3.8.0
instance PTryFrom PData (PAsData PTopTxInfo)

-- | @since 3.8.0
data PScriptInfo (s :: S)
  = PMintingScript (Term s (PAsData PCurrencySymbol))
  | PSpendingScript (Term s (PAsData PTxOutRef)) (Term s (PMaybeData PDatum))
  | PWithdrawingScript (Term s (PAsData PAccountId))
  | PCertifyingScript (Term s (PAsData PInteger)) (Term s (PAsData PTxCert))
  | PVotingScript (Term s (PAsData PVoter))
  | PProposingScript (Term s (PAsData PNatural)) (Term s (PAsData PProposalProcedure))
  | PGuardingScript (Term s (PAsData PInteger)) (Term s (PMaybeData PTopTxInfo))
  deriving stock
    ( -- | @since 3.8.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.8.0
      SOP.Generic
    , -- | @since 3.8.0
      PIsData
    , -- | @since 3.8.0
      PEq
    , -- | @since 3.8.0
      PShow
    )
  deriving
    ( -- | @since 3.8.0
      PlutusType
    , -- | @since 3.8.0
      PValidateData
    )
    via (DeriveAsDataStruct PScriptInfo)

-- | @since 3.8.0
deriving via
  DeriveDataPLiftable PScriptInfo Plutus.ScriptInfo
  instance
    PLiftable PScriptInfo

-- | @since 3.8.0
instance PTryFrom PData (PAsData PScriptInfo)

-- | @since 3.8.0
data PScriptContext (s :: S) = PScriptContext
  { pscriptContext'txInfo :: Term s (PAsData PTxInfo)
  , pscriptContext'redeemer :: Term s (PAsData PRedeemer)
  , pscriptContext'scriptInfo :: Term s (PAsData PScriptInfo)
  , pscriptContext'scriptHash :: Term s (PAsData PScriptHash)
  }
  deriving stock
    ( -- | @since 3.8.0
      Generic
    )
  deriving anyclass
    ( -- | @since 3.8.0
      SOP.Generic
    , -- | @since 3.8.0
      PIsData
    , -- | @since 3.8.0
      PEq
    , -- | @since 3.8.0
      PShow
    )
  deriving
    ( -- | @since 3.8.0
      PlutusType
    , -- | @since 3.8.0
      PValidateData
    )
    via (DeriveAsDataRec PScriptContext)

-- | @since 3.8.0
instance PLiftable PScriptContext where
  type AsHaskell PScriptContext = Plutus.ScriptContext
  type PlutusRepr PScriptContext = Plutus.Data
  haskToRepr = Plutus.toData
  reprToHask = maybe (Left CouldNotDecodeData) Right . Plutus.fromData
  plutToRepr t = fmap Plutus.List $ plutToReprUni @(PBuiltinList PData) $ punsafeCoercePLifted t
  reprToPlut = \case
    Plutus.List ell -> punsafeCoercePLifted (reprToPlutUni @(PBuiltinList PData) ell)
    _ -> error "Impossible: PScriptContext is list-represented."

-- | @since 3.8.0
instance PTryFrom PData (PAsData PScriptContext)
