{-# OPTIONS_GHC -Wno-orphans #-}

module PlutusLedgerApi.V3.Orphans () where

import Control.Monad (guard)
import Data.Coerce (coerce)
import PlutusLedgerApi.Orphans.Common (
  Blake2b256Hash (Blake2b256Hash),
 )
import PlutusLedgerApi.V1.Orphans.Credential ()
import PlutusLedgerApi.V1.Orphans.Interval ()
import PlutusLedgerApi.V1.Orphans.Time ()
import PlutusLedgerApi.V1.Orphans.Value ()
import PlutusLedgerApi.V2.Orphans.Tx ()
import PlutusLedgerApi.V3 qualified as PLA
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Prelude qualified as PlutusTx
import PlutusTx.Ratio qualified as Ratio
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  Arbitrary1 (liftArbitrary, liftShrink),
  CoArbitrary (coarbitrary),
  Function (function),
  Gen,
  NonNegative (NonNegative),
  Positive (Positive),
  chooseInt,
  elements,
  functionMap,
  getNonNegative,
  getPositive,
  oneof,
  variant,
 )
import Test.QuickCheck.Instances.Containers ()

-- | @since 1.0.1
deriving via PLA.Credential instance Arbitrary PLA.ColdCommitteeCredential

-- | @since 1.0.1
deriving via PLA.Credential instance CoArbitrary PLA.ColdCommitteeCredential

-- | @since 1.0.1
instance Function PLA.ColdCommitteeCredential where
  {-# INLINEABLE function #-}
  function = functionMap coerce PLA.ColdCommitteeCredential

-- | @since 1.0.1
deriving via PLA.Credential instance Arbitrary PLA.HotCommitteeCredential

-- | @since 1.0.1
deriving via PLA.Credential instance CoArbitrary PLA.HotCommitteeCredential

-- | @since 1.0.1
instance Function PLA.HotCommitteeCredential where
  {-# INLINEABLE function #-}
  function = functionMap coerce PLA.HotCommitteeCredential

-- | @since 1.0.1
deriving via PLA.Credential instance Arbitrary PLA.DRepCredential

-- | @since 1.0.1
deriving via PLA.Credential instance CoArbitrary PLA.DRepCredential

-- | @since 1.0.1
instance Function PLA.DRepCredential where
  {-# INLINEABLE function #-}
  function = functionMap coerce PLA.DRepCredential

{- | This instance has equal chance of generating always-abstain,
always-no-confidence and credential \'arms\'. Use this instance with this in
mind.

@since 1.0.1
-}
instance Arbitrary PLA.DRep where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    oneof
      [ PLA.DRep <$> arbitrary
      , pure PLA.DRepAlwaysAbstain
      , pure PLA.DRepAlwaysNoConfidence
      ]
  {-# INLINEABLE shrink #-}
  shrink = \case
    PLA.DRep cred -> PLA.DRep <$> shrink cred
    _ -> []

-- | since 1.0.0
instance CoArbitrary PLA.DRep where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary = \case
    PLA.DRep cred -> variant (0 :: Int) . coarbitrary cred
    PLA.DRepAlwaysAbstain -> variant (1 :: Int)
    PLA.DRepAlwaysNoConfidence -> variant (2 :: Int)

-- | @since 1.0.1
instance Function PLA.DRep where
  {-# INLINEABLE function #-}
  function = functionMap into outOf
    where
      into :: PLA.DRep -> Maybe (Maybe PLA.DRepCredential)
      into = \case
        PLA.DRep cred -> Just (Just cred)
        PLA.DRepAlwaysAbstain -> Nothing
        PLA.DRepAlwaysNoConfidence -> Just Nothing
      outOf :: Maybe (Maybe PLA.DRepCredential) -> PLA.DRep
      outOf = \case
        Nothing -> PLA.DRepAlwaysAbstain
        Just Nothing -> PLA.DRepAlwaysNoConfidence
        Just (Just cred) -> PLA.DRep cred

-- | @since 1.0.1
instance Arbitrary PLA.Delegatee where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    oneof
      [ PLA.DelegStake <$> arbitrary
      , PLA.DelegVote <$> arbitrary
      , PLA.DelegStakeVote <$> arbitrary <*> arbitrary
      ]
  {-# INLINEABLE shrink #-}
  shrink = \case
    PLA.DelegStake _ -> [] -- PubKeyHashes don't shrink anyway
    PLA.DelegVote drep -> PLA.DelegVote <$> shrink drep
    PLA.DelegStakeVote pkh drep -> PLA.DelegStakeVote pkh <$> shrink drep

-- | @since 1.0.1
instance CoArbitrary PLA.Delegatee where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary = \case
    PLA.DelegStake pkh -> variant (0 :: Int) . coarbitrary pkh
    PLA.DelegVote drep -> variant (1 :: Int) . coarbitrary drep
    PLA.DelegStakeVote pkh drep -> variant (2 :: Int) . coarbitrary pkh . coarbitrary drep

-- | @since 1.0.1
instance Function PLA.Delegatee where
  {-# INLINEABLE function #-}
  function = functionMap into outOf
    where
      into ::
        PLA.Delegatee ->
        Either PLA.PubKeyHash (Either PLA.DRep (PLA.PubKeyHash, PLA.DRep))
      into = \case
        PLA.DelegStake pkh -> Left pkh
        PLA.DelegVote drep -> Right (Left drep)
        PLA.DelegStakeVote pkh drep -> Right (Right (pkh, drep))
      outOf ::
        Either PLA.PubKeyHash (Either PLA.DRep (PLA.PubKeyHash, PLA.DRep)) ->
        PLA.Delegatee
      outOf = \case
        Left pkh -> PLA.DelegStake pkh
        Right (Left drep) -> PLA.DelegVote drep
        Right (Right (pkh, drep)) -> PLA.DelegStakeVote pkh drep

-- | @since 1.0.1
instance Arbitrary PLA.TxCert where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    oneof
      [ PLA.TxCertRegStaking <$> arbitrary <*> arbitrary
      , PLA.TxCertUnRegStaking <$> arbitrary <*> arbitrary
      , PLA.TxCertDelegStaking <$> arbitrary <*> arbitrary
      , PLA.TxCertRegDeleg <$> arbitrary <*> arbitrary <*> arbitrary
      , PLA.TxCertRegDRep <$> arbitrary <*> arbitrary
      , PLA.TxCertUpdateDRep <$> arbitrary
      , PLA.TxCertUnRegDRep <$> arbitrary <*> arbitrary
      , PLA.TxCertPoolRegister <$> arbitrary <*> arbitrary
      , -- epoch must be positive for this to make any sense
        PLA.TxCertPoolRetire <$> arbitrary <*> (getPositive <$> arbitrary)
      , PLA.TxCertAuthHotCommittee <$> arbitrary <*> arbitrary
      , PLA.TxCertResignColdCommittee <$> arbitrary
      ]
  {-# INLINEABLE shrink #-}
  shrink = \case
    PLA.TxCertRegStaking cred mLovelace ->
      PLA.TxCertRegStaking <$> shrink cred <*> shrink mLovelace
    PLA.TxCertUnRegStaking cred mLovelace ->
      PLA.TxCertUnRegStaking <$> shrink cred <*> shrink mLovelace
    PLA.TxCertDelegStaking cred deleg ->
      PLA.TxCertDelegStaking <$> shrink cred <*> shrink deleg
    PLA.TxCertRegDeleg cred deleg lovelace ->
      PLA.TxCertRegDeleg <$> shrink cred <*> shrink deleg <*> shrink lovelace
    PLA.TxCertRegDRep drepCred lovelace ->
      PLA.TxCertRegDRep <$> shrink drepCred <*> shrink lovelace
    PLA.TxCertUpdateDRep drepCred -> PLA.TxCertUpdateDRep <$> shrink drepCred
    PLA.TxCertUnRegDRep drepCred lovelace ->
      PLA.TxCertUnRegDRep <$> shrink drepCred <*> shrink lovelace
    -- PubKeyHash doesn't shrink, so we don't bother either
    PLA.TxCertPoolRegister _ _ -> []
    PLA.TxCertPoolRetire pkh epoch ->
      PLA.TxCertPoolRetire pkh . getPositive <$> shrink (Positive epoch)
    PLA.TxCertAuthHotCommittee cold hot ->
      PLA.TxCertAuthHotCommittee <$> shrink cold <*> shrink hot
    PLA.TxCertResignColdCommittee cold -> PLA.TxCertResignColdCommittee <$> shrink cold

-- | @since 1.0.1
instance CoArbitrary PLA.TxCert where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary = \case
    PLA.TxCertRegStaking cred mLovelace ->
      variant (0 :: Int) . coarbitrary cred . coarbitrary mLovelace
    PLA.TxCertUnRegStaking cred mLovelace ->
      variant (1 :: Int) . coarbitrary cred . coarbitrary mLovelace
    PLA.TxCertDelegStaking cred deleg ->
      variant (2 :: Int) . coarbitrary cred . coarbitrary deleg
    PLA.TxCertRegDeleg cred deleg lovelace ->
      variant (3 :: Int) . coarbitrary cred . coarbitrary deleg . coarbitrary lovelace
    PLA.TxCertRegDRep drepCred lovelace ->
      variant (4 :: Int) . coarbitrary drepCred . coarbitrary lovelace
    PLA.TxCertUpdateDRep drepCred ->
      variant (5 :: Int) . coarbitrary drepCred
    PLA.TxCertUnRegDRep drepCred lovelace ->
      variant (6 :: Int) . coarbitrary drepCred . coarbitrary lovelace
    PLA.TxCertPoolRegister pkh pkh' ->
      variant (7 :: Int) . coarbitrary pkh . coarbitrary pkh'
    PLA.TxCertPoolRetire pkh epoch ->
      variant (8 :: Int) . coarbitrary pkh . coarbitrary epoch
    PLA.TxCertAuthHotCommittee cold hot ->
      variant (9 :: Int) . coarbitrary cold . coarbitrary hot
    PLA.TxCertResignColdCommittee cold ->
      variant (10 :: Int) . coarbitrary cold

-- | @since 1.0.1
instance Function PLA.TxCert where
  {-# INLINEABLE function #-}
  function = functionMap into outOf
    where
      into ::
        PLA.TxCert ->
        Either
          (PLA.Credential, Maybe PLA.Lovelace)
          ( Either
              (PLA.Credential, Maybe PLA.Lovelace)
              ( Either
                  (PLA.Credential, PLA.Delegatee)
                  ( Either
                      (PLA.Credential, PLA.Delegatee, PLA.Lovelace)
                      ( Either
                          (PLA.DRepCredential, PLA.Lovelace)
                          ( Either
                              PLA.DRepCredential
                              ( Either
                                  (PLA.DRepCredential, PLA.Lovelace)
                                  ( Either
                                      (PLA.PubKeyHash, PLA.PubKeyHash)
                                      ( Either
                                          (PLA.PubKeyHash, Integer)
                                          ( Either (PLA.ColdCommitteeCredential, PLA.HotCommitteeCredential) PLA.ColdCommitteeCredential
                                          )
                                      )
                                  )
                              )
                          )
                      )
                  )
              )
          )
      into = \case
        PLA.TxCertRegStaking cred mLovelace ->
          Left (cred, mLovelace)
        PLA.TxCertUnRegStaking cred mLovelace ->
          Right (Left (cred, mLovelace))
        PLA.TxCertDelegStaking cred deleg ->
          Right (Right (Left (cred, deleg)))
        PLA.TxCertRegDeleg cred deleg lovelace ->
          Right (Right (Right (Left (cred, deleg, lovelace))))
        PLA.TxCertRegDRep drepCred lovelace ->
          Right (Right (Right (Right (Left (drepCred, lovelace)))))
        PLA.TxCertUpdateDRep drepCred ->
          Right (Right (Right (Right (Right (Left drepCred)))))
        PLA.TxCertUnRegDRep drepCred lovelace ->
          Right (Right (Right (Right (Right (Right (Left (drepCred, lovelace)))))))
        PLA.TxCertPoolRegister pkh pkh' ->
          Right (Right (Right (Right (Right (Right (Right (Left (pkh, pkh'))))))))
        PLA.TxCertPoolRetire pkh epoch ->
          Right (Right (Right (Right (Right (Right (Right (Right (Left (pkh, epoch)))))))))
        PLA.TxCertAuthHotCommittee hot cold ->
          Right (Right (Right (Right (Right (Right (Right (Right (Right (Left (hot, cold))))))))))
        PLA.TxCertResignColdCommittee cold ->
          Right (Right (Right (Right (Right (Right (Right (Right (Right (Right cold)))))))))
      outOf ::
        Either
          (PLA.Credential, Maybe PLA.Lovelace)
          ( Either
              (PLA.Credential, Maybe PLA.Lovelace)
              ( Either
                  (PLA.Credential, PLA.Delegatee)
                  ( Either
                      (PLA.Credential, PLA.Delegatee, PLA.Lovelace)
                      ( Either
                          (PLA.DRepCredential, PLA.Lovelace)
                          ( Either
                              PLA.DRepCredential
                              ( Either
                                  (PLA.DRepCredential, PLA.Lovelace)
                                  ( Either
                                      (PLA.PubKeyHash, PLA.PubKeyHash)
                                      ( Either
                                          (PLA.PubKeyHash, Integer)
                                          ( Either (PLA.ColdCommitteeCredential, PLA.HotCommitteeCredential) PLA.ColdCommitteeCredential
                                          )
                                      )
                                  )
                              )
                          )
                      )
                  )
              )
          ) ->
        PLA.TxCert
      outOf = \case
        Left (cred, mLovelace) ->
          PLA.TxCertRegStaking cred mLovelace
        Right (Left (cred, mLovelace)) ->
          PLA.TxCertUnRegStaking cred mLovelace
        Right (Right (Left (cred, deleg))) ->
          PLA.TxCertDelegStaking cred deleg
        Right (Right (Right (Left (cred, deleg, lovelace)))) ->
          PLA.TxCertRegDeleg cred deleg lovelace
        Right (Right (Right (Right (Left (drepCred, lovelace))))) ->
          PLA.TxCertRegDRep drepCred lovelace
        Right (Right (Right (Right (Right (Left drepCred))))) ->
          PLA.TxCertUpdateDRep drepCred
        Right (Right (Right (Right (Right (Right (Left (drepCred, lovelace))))))) ->
          PLA.TxCertUnRegDRep drepCred lovelace
        Right (Right (Right (Right (Right (Right (Right (Left (pkh, pkh')))))))) ->
          PLA.TxCertPoolRegister pkh pkh'
        Right (Right (Right (Right (Right (Right (Right (Right (Left (pkh, epoch))))))))) ->
          PLA.TxCertPoolRetire pkh epoch
        Right (Right (Right (Right (Right (Right (Right (Right (Right (Left (hot, cold)))))))))) ->
          PLA.TxCertAuthHotCommittee hot cold
        Right (Right (Right (Right (Right (Right (Right (Right (Right (Right cold))))))))) ->
          PLA.TxCertResignColdCommittee cold

-- | @since 1.0.1
instance Arbitrary PLA.Voter where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    oneof
      [ PLA.CommitteeVoter <$> arbitrary
      , PLA.DRepVoter <$> arbitrary
      , PLA.StakePoolVoter <$> arbitrary
      ]
  {-# INLINEABLE shrink #-}
  shrink = \case
    PLA.CommitteeVoter hcc -> PLA.CommitteeVoter <$> shrink hcc
    PLA.DRepVoter drepCred -> PLA.DRepVoter <$> shrink drepCred
    -- PubKeyHashes don't shrink so we don't bother either
    _ -> []

-- | @since 1.0.1
instance CoArbitrary PLA.Voter where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary = \case
    PLA.CommitteeVoter hcc -> variant (0 :: Int) . coarbitrary hcc
    PLA.DRepVoter drepCred -> variant (1 :: Int) . coarbitrary drepCred
    PLA.StakePoolVoter pkh -> variant (2 :: Int) . coarbitrary pkh

-- | @since 1.0.1
instance Function PLA.Voter where
  {-# INLINEABLE function #-}
  function = functionMap into outOf
    where
      into ::
        PLA.Voter ->
        Either PLA.HotCommitteeCredential (Either PLA.DRepCredential PLA.PubKeyHash)
      into = \case
        PLA.CommitteeVoter hcc -> Left hcc
        PLA.DRepVoter drepCred -> Right (Left drepCred)
        PLA.StakePoolVoter pkh -> Right (Right pkh)
      outOf ::
        Either PLA.HotCommitteeCredential (Either PLA.DRepCredential PLA.PubKeyHash) ->
        PLA.Voter
      outOf = \case
        Left hcc -> PLA.CommitteeVoter hcc
        Right (Left drepCred) -> PLA.DRepVoter drepCred
        Right (Right pkh) -> PLA.StakePoolVoter pkh

{- | Does not shrink (as there's not much point).

@since 1.0.1
-}
instance Arbitrary PLA.Vote where
  {-# INLINEABLE arbitrary #-}
  arbitrary = elements [PLA.VoteNo, PLA.VoteYes, PLA.Abstain]

-- | @since 1.0.1
instance CoArbitrary PLA.Vote where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary = \case
    PLA.VoteNo -> variant (0 :: Int)
    PLA.VoteYes -> variant (1 :: Int)
    PLA.Abstain -> variant (2 :: Int)

-- | @since 1.0.1
instance Function PLA.Vote where
  {-# INLINEABLE function #-}
  function = functionMap into outOf
    where
      into :: PLA.Vote -> Int
      into = \case
        PLA.VoteNo -> 0
        PLA.VoteYes -> 1
        _ -> 2
      outOf :: Int -> PLA.Vote
      outOf = \case
        0 -> PLA.VoteNo
        1 -> PLA.VoteYes
        _ -> PLA.Abstain

-- | @since 1.0.1
deriving via Blake2b256Hash instance Arbitrary PLA.TxId

-- | @since 1.0.1
deriving via Blake2b256Hash instance CoArbitrary PLA.TxId

-- | @since 1.0.1
instance Function PLA.TxId where
  {-# INLINEABLE function #-}
  function = functionMap coerce PLA.TxId

-- | @since 1.0.1
instance Arbitrary PLA.GovernanceActionId where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    PLA.GovernanceActionId
      <$> arbitrary
      <*> (getNonNegative <$> arbitrary)
  {-# INLINEABLE shrink #-}
  shrink (PLA.GovernanceActionId tid ix) =
    -- Hashes don't shrink, so we don't bother either
    PLA.GovernanceActionId tid . getNonNegative <$> shrink (NonNegative ix)

-- | @since 1.0.1
instance CoArbitrary PLA.GovernanceActionId where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary (PLA.GovernanceActionId tid ix) =
    coarbitrary tid . coarbitrary ix

-- | @since 1.0.1
instance Function PLA.GovernanceActionId where
  {-# INLINEABLE function #-}
  function = functionMap (\(PLA.GovernanceActionId tid ix) -> (tid, ix)) (uncurry PLA.GovernanceActionId)

{- | Does not shrink the quorum, as this is surprisingly hard to do sensibly. We
assume the quorum is in the interval @(0, 1]@ (meaning anywhere from a single
voice to unanimity).

@since 1.0.1
-}
instance Arbitrary PLA.Committee where
  {-# INLINEABLE arbitrary #-}
  arbitrary = do
    committee <- liftArbitrary (getPositive <$> arbitrary)
    -- We can't have a quorum of 0.0
    num <- chooseInt (1, 100)
    let quorum = Ratio.unsafeRatio (fromIntegral num) 100
    pure . PLA.Committee committee $ quorum
  {-# INLINEABLE shrink #-}
  shrink (PLA.Committee committee quorum) = do
    committee' <- liftShrink (fmap getPositive . shrink . Positive) committee
    guard (not . AssocMap.null $ committee')
    pure . PLA.Committee committee' $ quorum

-- | @since 1.0.1
instance CoArbitrary PLA.Committee where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary (PLA.Committee committee quorum) =
    coarbitrary committee
      . coarbitrary (Ratio.numerator quorum)
      . coarbitrary (Ratio.denominator quorum)

-- | @since 1.0.1
instance Function PLA.Committee where
  {-# INLINEABLE function #-}
  function = functionMap into outOf
    where
      into ::
        PLA.Committee ->
        (PLA.Map PLA.ColdCommitteeCredential Integer, Integer, Integer)
      into (PLA.Committee committee quorum) =
        (committee, Ratio.numerator quorum, Ratio.denominator quorum)
      outOf ::
        (PLA.Map PLA.ColdCommitteeCredential Integer, Integer, Integer) ->
        PLA.Committee
      outOf (committee, num, den) =
        PLA.Committee committee . Ratio.unsafeRatio num $ den

-- | @since 1.0.1
deriving via (Maybe PLA.ScriptHash) instance Arbitrary PLA.Constitution

-- | @since 1.0.1
deriving via (Maybe PLA.ScriptHash) instance CoArbitrary PLA.Constitution

-- | @since 1.0.1
instance Function PLA.Constitution where
  {-# INLINEABLE function #-}
  function = functionMap coerce PLA.Constitution

-- | @since 1.0.1
instance Arbitrary PLA.ProtocolVersion where
  {-# INLINEABLE arbitrary #-}
  arbitrary = do
    NonNegative major <- arbitrary
    NonNegative minor <- arbitrary
    pure . PLA.ProtocolVersion major $ minor
  {-# INLINEABLE shrink #-}
  shrink (PLA.ProtocolVersion major minor) = do
    NonNegative major' <- shrink (NonNegative major)
    NonNegative minor' <- shrink (NonNegative minor)
    pure . PLA.ProtocolVersion major' $ minor'

-- | @since 1.0.1
instance CoArbitrary PLA.ProtocolVersion where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary (PLA.ProtocolVersion major minor) =
    coarbitrary major . coarbitrary minor

-- | @since 1.0.1
instance Function PLA.ProtocolVersion where
  {-# INLINEABLE function #-}
  function =
    functionMap
      (\(PLA.ProtocolVersion maj' min') -> (maj', min'))
      (uncurry PLA.ProtocolVersion)

-- TODO: Mirror this: https://github.com/IntersectMBO/cardano-ledger/blob/master/eras/conway/impl/cddl-files/conway.cddl#L381

{- | Currently only generates a map with integer keys in the range 0-33, with
random values. Does not shrink.

@since 1.0.1
-}
instance Arbitrary PLA.ChangedParameters where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    PLA.ChangedParameters . Builtins.mkMap <$> liftArbitrary go
    where
      go :: Gen (PLA.BuiltinData, PLA.BuiltinData)
      go =
        (,) . Builtins.mkI . fromIntegral
          <$> chooseInt (0, 33)
          <*> arbitrary

-- | @since 1.0.1
deriving via PlutusTx.BuiltinData instance CoArbitrary PLA.ChangedParameters

-- | @since 1.0.1
instance Function PLA.ChangedParameters where
  {-# INLINEABLE function #-}
  function = functionMap coerce PLA.ChangedParameters

-- TODO: Technically this can generate nonsensical instances (such as committee
-- members without keys), and we need to fix this.

-- | @since 1.0.1
instance Arbitrary PLA.GovernanceAction where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    oneof
      [ PLA.ParameterChange <$> arbitrary <*> arbitrary <*> arbitrary
      , PLA.HardForkInitiation <$> arbitrary <*> arbitrary
      , PLA.TreasuryWithdrawals <$> arbitrary <*> arbitrary
      , PLA.NoConfidence <$> arbitrary
      , PLA.UpdateCommittee
          <$> arbitrary
          <*> arbitrary
          <*> arbitrary
          <*> (Ratio.unsafeRatio . fromIntegral <$> chooseInt (1, 100) <*> pure 100)
      , PLA.NewConstitution <$> arbitrary <*> arbitrary
      , pure PLA.InfoAction
      ]
  {-# INLINEABLE shrink #-}
  shrink = \case
    PLA.ParameterChange mgid cp msh ->
      PLA.ParameterChange
        <$> shrink mgid
        <*> shrink cp
        <*> shrink msh
    PLA.HardForkInitiation mgid v ->
      PLA.HardForkInitiation
        <$> shrink mgid
        <*> shrink v
    PLA.TreasuryWithdrawals wdrls msh ->
      PLA.TreasuryWithdrawals
        <$> shrink wdrls
        <*> shrink msh
    PLA.NoConfidence msh -> PLA.NoConfidence <$> shrink msh
    -- No quorum shrinking
    PLA.UpdateCommittee mgid creds mems quorum -> do
      mgid' <- shrink mgid
      creds' <- shrink creds
      mems' <- shrink mems
      pure . PLA.UpdateCommittee mgid' creds' mems' $ quorum
    PLA.NewConstitution mgid c -> PLA.NewConstitution <$> shrink mgid <*> shrink c
    _ -> []

-- | @since 1.0.1
instance CoArbitrary PLA.GovernanceAction where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary = \case
    PLA.ParameterChange mgid cp msh ->
      variant (0 :: Int) . coarbitrary mgid . coarbitrary cp . coarbitrary msh
    PLA.HardForkInitiation mgid v ->
      variant (1 :: Int) . coarbitrary mgid . coarbitrary v
    PLA.TreasuryWithdrawals wdrls msh ->
      variant (2 :: Int) . coarbitrary wdrls . coarbitrary msh
    PLA.NoConfidence msh ->
      variant (3 :: Int) . coarbitrary msh
    PLA.UpdateCommittee mgid creds mems quorum ->
      variant (4 :: Int)
        . coarbitrary mgid
        . coarbitrary creds
        . coarbitrary mems
        . coarbitrary (Ratio.numerator quorum)
        . coarbitrary (Ratio.denominator quorum)
    PLA.NewConstitution mgid c ->
      variant (5 :: Int) . coarbitrary mgid . coarbitrary c
    PLA.InfoAction -> variant (6 :: Int)

-- | @since 1.0.1
instance Function PLA.GovernanceAction where
  {-# INLINEABLE function #-}
  function = functionMap into outOf
    where
      into ::
        PLA.GovernanceAction ->
        Maybe
          ( Either
              (Maybe PLA.GovernanceActionId, PLA.ChangedParameters, Maybe PLA.ScriptHash)
              ( Either
                  (Maybe PLA.GovernanceActionId, PLA.ProtocolVersion)
                  ( Either
                      (PLA.Map PLA.Credential PLA.Lovelace, Maybe PLA.ScriptHash)
                      ( Either
                          (Maybe PLA.GovernanceActionId)
                          ( Either (Maybe PLA.GovernanceActionId, [PLA.ColdCommitteeCredential], PLA.Map PLA.ColdCommitteeCredential Integer, Integer, Integer) (Maybe PLA.GovernanceActionId, PLA.Constitution)
                          )
                      )
                  )
              )
          )
      into = \case
        PLA.InfoAction -> Nothing
        PLA.ParameterChange mgid cp msh -> Just (Left (mgid, cp, msh))
        PLA.HardForkInitiation mgid v -> Just (Right (Left (mgid, v)))
        PLA.TreasuryWithdrawals wdrls msh -> Just (Right (Right (Left (wdrls, msh))))
        PLA.NoConfidence msh -> Just (Right (Right (Right (Left msh))))
        PLA.UpdateCommittee mgid creds mems quorum ->
          Just (Right (Right (Right (Right (Left (mgid, creds, mems, Ratio.numerator quorum, Ratio.denominator quorum))))))
        PLA.NewConstitution mgid c ->
          Just (Right (Right (Right (Right (Right (mgid, c))))))
      outOf ::
        Maybe
          ( Either
              (Maybe PLA.GovernanceActionId, PLA.ChangedParameters, Maybe PLA.ScriptHash)
              ( Either
                  (Maybe PLA.GovernanceActionId, PLA.ProtocolVersion)
                  ( Either
                      (PLA.Map PLA.Credential PLA.Lovelace, Maybe PLA.ScriptHash)
                      ( Either
                          (Maybe PLA.GovernanceActionId)
                          ( Either (Maybe PLA.GovernanceActionId, [PLA.ColdCommitteeCredential], PLA.Map PLA.ColdCommitteeCredential Integer, Integer, Integer) (Maybe PLA.GovernanceActionId, PLA.Constitution)
                          )
                      )
                  )
              )
          ) ->
        PLA.GovernanceAction
      outOf = \case
        Nothing -> PLA.InfoAction
        Just (Left (mgid, cp, msh)) -> PLA.ParameterChange mgid cp msh
        Just (Right (Left (mgid, v))) -> PLA.HardForkInitiation mgid v
        Just (Right (Right (Left (wdrls, msh)))) -> PLA.TreasuryWithdrawals wdrls msh
        Just (Right (Right (Right (Left msh)))) -> PLA.NoConfidence msh
        Just (Right (Right (Right (Right (Left (mgid, creds, mems, n, d)))))) ->
          PLA.UpdateCommittee mgid creds mems (Ratio.unsafeRatio n d)
        Just (Right (Right (Right (Right (Right (mgid, c)))))) ->
          PLA.NewConstitution mgid c

-- | @since 1.0.1
instance Arbitrary PLA.ProposalProcedure where
  {-# INLINEABLE arbitrary #-}
  arbitrary = PLA.ProposalProcedure <$> arbitrary <*> arbitrary <*> arbitrary
  {-# INLINEABLE shrink #-}
  shrink (PLA.ProposalProcedure dep raddr ga) =
    PLA.ProposalProcedure <$> shrink dep <*> shrink raddr <*> shrink ga

-- | @since 1.0.1
instance CoArbitrary PLA.ProposalProcedure where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary (PLA.ProposalProcedure dep raddr ga) =
    coarbitrary dep . coarbitrary raddr . coarbitrary ga

-- | @since 1.0.1
instance Function PLA.ProposalProcedure where
  {-# INLINEABLE function #-}
  function = functionMap into outOf
    where
      into ::
        PLA.ProposalProcedure ->
        (PLA.Lovelace, PLA.Credential, PLA.GovernanceAction)
      into (PLA.ProposalProcedure dep raddr ga) = (dep, raddr, ga)
      outOf ::
        (PLA.Lovelace, PLA.Credential, PLA.GovernanceAction) ->
        PLA.ProposalProcedure
      outOf (dep, raddr, ga) = PLA.ProposalProcedure dep raddr ga

-- | @since 1.0.1
instance Arbitrary PLA.TxOutRef where
  {-# INLINEABLE arbitrary #-}
  arbitrary = PLA.TxOutRef <$> arbitrary <*> (getNonNegative <$> arbitrary)
  {-# INLINEABLE shrink #-}
  shrink (PLA.TxOutRef tid ix) =
    PLA.TxOutRef <$> shrink tid <*> (fmap getNonNegative . shrink . NonNegative $ ix)

-- | @since 1.0.1
instance CoArbitrary PLA.TxOutRef where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary (PLA.TxOutRef tid ix) =
    coarbitrary tid . coarbitrary ix

-- | @since 1.0.1
instance Function PLA.TxOutRef where
  {-# INLINEABLE function #-}
  function = functionMap (\(PLA.TxOutRef tid ix) -> (tid, ix)) (uncurry PLA.TxOutRef)

-- | @since 1.0.1
instance Arbitrary PLA.ScriptPurpose where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    oneof
      [ PLA.Minting <$> arbitrary
      , PLA.Spending <$> arbitrary
      , PLA.Rewarding <$> arbitrary
      , PLA.Certifying . getNonNegative <$> arbitrary <*> arbitrary
      , PLA.Voting <$> arbitrary
      , PLA.Proposing . getNonNegative <$> arbitrary <*> arbitrary
      ]
  {-# INLINEABLE shrink #-}
  shrink = \case
    PLA.Minting cs -> PLA.Minting <$> shrink cs
    PLA.Spending txo -> PLA.Spending <$> shrink txo
    PLA.Rewarding cred -> PLA.Rewarding <$> shrink cred
    PLA.Certifying ix cert -> do
      cert' <- shrink cert
      NonNegative ix' <- shrink (NonNegative ix)
      pure . PLA.Certifying ix' $ cert'
    PLA.Voting voter -> PLA.Voting <$> shrink voter
    PLA.Proposing ix pp -> do
      pp' <- shrink pp
      NonNegative ix' <- shrink (NonNegative ix)
      pure . PLA.Proposing ix' $ pp'

-- | @since 1.0.1
instance CoArbitrary PLA.ScriptPurpose where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary = \case
    PLA.Minting cs -> variant (0 :: Int) . coarbitrary cs
    PLA.Spending txo -> variant (1 :: Int) . coarbitrary txo
    PLA.Rewarding cred -> variant (2 :: Int) . coarbitrary cred
    PLA.Certifying ix cert -> variant (3 :: Int) . coarbitrary ix . coarbitrary cert
    PLA.Voting voter -> variant (4 :: Int) . coarbitrary voter
    PLA.Proposing ix pp -> variant (5 :: Int) . coarbitrary ix . coarbitrary pp

-- | @since 1.0.1
instance Function PLA.ScriptPurpose where
  {-# INLINEABLE function #-}
  function = functionMap into outOf
    where
      into ::
        PLA.ScriptPurpose ->
        Either
          PLA.CurrencySymbol
          ( Either
              PLA.TxOutRef
              ( Either
                  PLA.Credential
                  ( Either
                      (Integer, PLA.TxCert)
                      ( Either PLA.Voter (Integer, PLA.ProposalProcedure)
                      )
                  )
              )
          )
      into = \case
        PLA.Minting cs -> Left cs
        PLA.Spending txo -> Right (Left txo)
        PLA.Rewarding cred -> Right (Right (Left cred))
        PLA.Certifying ix cert -> Right (Right (Right (Left (ix, cert))))
        PLA.Voting voter -> Right (Right (Right (Right (Left voter))))
        PLA.Proposing ix pp -> Right (Right (Right (Right (Right (ix, pp)))))
      outOf ::
        Either
          PLA.CurrencySymbol
          ( Either
              PLA.TxOutRef
              ( Either
                  PLA.Credential
                  ( Either
                      (Integer, PLA.TxCert)
                      ( Either PLA.Voter (Integer, PLA.ProposalProcedure)
                      )
                  )
              )
          ) ->
        PLA.ScriptPurpose
      outOf = \case
        Left cs -> PLA.Minting cs
        Right (Left txo) -> PLA.Spending txo
        Right (Right (Left cred)) -> PLA.Rewarding cred
        Right (Right (Right (Left (ix, cert)))) -> PLA.Certifying ix cert
        Right (Right (Right (Right (Left voter)))) -> PLA.Voting voter
        Right (Right (Right (Right (Right (ix, pp))))) -> PLA.Proposing ix pp

-- TODO: CoArbitrary, Function

-- | @since 1.0.1
instance Arbitrary PLA.ScriptInfo where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    oneof
      [ PLA.MintingScript <$> arbitrary
      , PLA.SpendingScript <$> arbitrary <*> arbitrary
      , PLA.RewardingScript <$> arbitrary
      , PLA.CertifyingScript . getNonNegative <$> arbitrary <*> arbitrary
      , PLA.VotingScript <$> arbitrary
      , PLA.ProposingScript . getNonNegative <$> arbitrary <*> arbitrary
      ]
  {-# INLINEABLE shrink #-}
  shrink = \case
    PLA.MintingScript cs -> PLA.MintingScript <$> shrink cs
    PLA.SpendingScript outRef mdat -> PLA.SpendingScript <$> shrink outRef <*> shrink mdat
    PLA.RewardingScript cred -> PLA.RewardingScript <$> shrink cred
    PLA.CertifyingScript ix cert -> do
      NonNegative ix' <- shrink (NonNegative ix)
      PLA.CertifyingScript ix' <$> shrink cert
    PLA.VotingScript voter -> PLA.VotingScript <$> shrink voter
    PLA.ProposingScript ix pp -> do
      NonNegative ix' <- shrink (NonNegative ix)
      PLA.ProposingScript ix' <$> shrink pp

-- TODO: CoArbitrary, Function

-- | @since 1.0.1
instance Arbitrary PLA.TxInInfo where
  {-# INLINEABLE arbitrary #-}
  arbitrary = PLA.TxInInfo <$> arbitrary <*> arbitrary
  {-# INLINEABLE shrink #-}
  shrink (PLA.TxInInfo toutref tout) =
    PLA.TxInInfo <$> shrink toutref <*> shrink tout

-- TODO: CoArbitrary, Function
-- TODO: invariants

-- | @since 1.0.1
instance Arbitrary PLA.TxInfo where
  {-# INLINEABLE arbitrary #-}
  arbitrary = do
    ins <- arbitrary
    routs <- arbitrary
    outs <- arbitrary
    fee <- arbitrary
    mint <- arbitrary
    cert <- arbitrary
    wdrl <- arbitrary
    valid <- arbitrary
    sigs <- arbitrary
    reds <- arbitrary
    dats <- arbitrary
    tid <- arbitrary
    votes <- arbitrary
    pps <- arbitrary
    currT <- arbitrary
    tDonation <- arbitrary
    pure . PLA.TxInfo ins routs outs fee mint cert wdrl valid sigs reds dats tid votes pps currT $ tDonation
  {-# INLINEABLE shrink #-}
  shrink = _
