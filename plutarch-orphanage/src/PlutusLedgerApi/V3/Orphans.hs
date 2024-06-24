module PlutusLedgerApi.V3.Orphans () where

import Data.Coerce (coerce)
import PlutusLedgerApi.V1.Orphans.Credential ()
import PlutusLedgerApi.V1.Orphans.Value ()
import PlutusLedgerApi.V3 qualified as PLA
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  CoArbitrary (coarbitrary),
  Function (function),
  Positive (Positive),
  elements,
  functionMap,
  getPositive,
  oneof,
  variant,
 )

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
