{- | = Note

The 'Value.PValue', 'AssocMap.PMap' and 'Interval.PInterval'-related
functionality can be found in other modules, as these clash with the Plutarch
prelude. These should be imported qualified.
-}
module Plutarch.LedgerApi.V4 (
  -- * Accounts
  PAccountId (..),
  PAccountBalanceInterval (..),
  PAccountBalanceIntervals (..),

  -- * Other
  PAddress (..),
  PTxOut (..),
  PTxCert (..),
  PScriptPurpose (..),
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Internal.Lift (LiftError (CouldNotDecodeData))
import Plutarch.LedgerApi.AssocMap (PAssocMap)
import Plutarch.LedgerApi.Utils (PMaybeData)
import Plutarch.LedgerApi.V1.Credential (PCredential)
import Plutarch.LedgerApi.V1.Crypto (PPubKeyHash)
import Plutarch.LedgerApi.V1.Scripts (PScriptHash)
import Plutarch.LedgerApi.V2.Tx (POutputDatum)
import Plutarch.LedgerApi.V3.Contexts (
  PColdCommitteeCredential,
  PDRepCredential,
  PDelegatee,
  PHotCommitteeCredential,
  PProposalProcedure,
  PVoter,
 )
import Plutarch.LedgerApi.V3.Tx (PTxOutRef)
import Plutarch.LedgerApi.Value (PCurrencySymbol, PLedgerValue, PLovelace)
import Plutarch.Prelude
import PlutusLedgerApi.V4 qualified as Plutus

-- | @since 3.8.0
newtype PAccountId (s :: S) = PAccountId (Term s PCredential)
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
    )
    via (DeriveNewtypePlutusType PAccountId)
  deriving
    ( -- | @since 3.8.0
      PValidateData
    )
    via (DeriveNewtypePValidateData PAccountId PCredential)

-- | @since 3.8.0
deriving via
  DeriveDataPLiftable PAccountId Plutus.AccountId
  instance
    PLiftable PAccountId

-- | @since 3.8.0
instance PTryFrom PData (PAsData PAccountId)

-- | @since 3.8.0
data PAddress (s :: S) = PAddress
  { paddress'credential :: Term s (PAsData PCredential)
  , paddress'stakingAccountId :: Term s (PMaybeData PAccountId)
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
    via (DeriveAsDataStruct PAddress)

-- | @since 3.8.0
deriving via
  DeriveDataPLiftable PAddress Plutus.Address
  instance
    PLiftable PAddress

-- | @since 3.8.0
instance PTryFrom PData (PAsData PAddress)

-- | @since 3.8.0
data PTxOut (s :: S) = PTxOut
  { ptxOut'address :: Term s (PAsData PAddress)
  , ptxOut'value :: Term s (PAsData PLedgerValue)
  , ptxOut'datum :: Term s (PAsData POutputDatum)
  , ptxOut'referenceScript :: Term s (PMaybeData PScriptHash)
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
    via (DeriveAsDataStruct PTxOut)

-- | @since 3.8.0
deriving via
  DeriveDataPLiftable PTxOut Plutus.TxOut
  instance
    PLiftable PTxOut

-- | @since 3.8.0
instance PTryFrom PData (PAsData PTxOut)

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
  = PAccountBalanceIntervals (Term s (PAssocMap PAccountId PAccountBalanceInterval))
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
    via (DeriveNewtypePValidateData PAccountBalanceIntervals (PAssocMap PAccountId PAccountBalanceInterval))

-- | @since 3.8.0
instance PLiftable PAccountBalanceIntervals where
  type AsHaskell PAccountBalanceIntervals = Plutus.AccountBalanceIntervals
  type PlutusRepr PAccountBalanceIntervals = Plutus.Data
  haskToRepr = Plutus.toData
  reprToHask = maybe (Left CouldNotDecodeData) Right . Plutus.fromData
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
