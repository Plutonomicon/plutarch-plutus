{-# OPTIONS_GHC -Wno-orphans #-}

module PlutusLedgerApi.V4.Orphans where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Coerce (coerce)
import Data.Foldable (foldl')
import Data.Maybe (fromJust)
import Data.These (These (That, These, This), these)
import PlutusLedgerApi.V1.Orphans.Value qualified as Value
import PlutusLedgerApi.V3.Orphans ()
import PlutusLedgerApi.V4 qualified as PLA
import PlutusLedgerApi.V4.Ratio qualified as Ratio
import PlutusTx.AssocMap qualified as AssocMap
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  CoArbitrary (coarbitrary),
  Function (function),
  NonEmptyList (NonEmpty),
  NonNegative (NonNegative),
  Positive (Positive),
  chooseInt,
  functionMap,
  getNonEmpty,
  getNonNegative,
  getPositive,
  liftArbitrary,
  liftShrink,
  oneof,
  variant,
 )

-- | @since 1.4.0
instance Arbitrary PLA.Rational where
  {-# INLINEABLE arbitrary #-}
  arbitrary = do
    den <- chooseInt (1, 100)
    num <- chooseInt (-100, 100)
    pure . fromJust . PLA.ratio (fromIntegral num) . fromIntegral $ den
  {-# INLINEABLE shrink #-}
  shrink r = do
    let n = PLA.numerator r
    let d = PLA.denominator r
    n' <- shrink n
    pure . fromJust . PLA.ratio n' $ d

-- | @since 1.4.0
instance CoArbitrary PLA.Rational where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary r =
    coarbitrary (PLA.numerator r)
      . coarbitrary (PLA.denominator r)

-- | @since 1.4.0
instance Function PLA.Rational where
  {-# INLINE function #-}
  function = functionMap into outOf
    where
      into :: PLA.Rational -> (Integer, Integer)
      into r = (PLA.numerator r, PLA.denominator r)
      outOf :: (Integer, Integer) -> PLA.Rational
      outOf (n, d) = PLA.unsafeRatio n d

{- | Does not shrink the quorum, as this is surprisingly hard to do sensibly. We
assume the quorum is in the interval @(0, 1]@ (meaning anywhere from a single
voice to unanimity).

@since 1.4.0
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

-- | @since 1.4.0
instance CoArbitrary PLA.Committee where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary (PLA.Committee committee quorum) =
    coarbitrary committee
      . coarbitrary (Ratio.numerator quorum)
      . coarbitrary (Ratio.denominator quorum)

-- | @since 1.4.0
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

-- | @since 1.4.0
deriving via PLA.Credential instance Arbitrary PLA.AccountId

-- | @since 1.4.0
deriving via PLA.Credential instance CoArbitrary PLA.AccountId

-- | @since 1.4.0
instance Function PLA.AccountId where
  {-# INLINEABLE function #-}
  function = functionMap coerce PLA.AccountId

-- | @since 1.4.0
instance Arbitrary PLA.Address where
  {-# INLINEABLE arbitrary #-}
  arbitrary = PLA.Address <$> arbitrary <*> arbitrary
  {-# INLINEABLE shrink #-}
  shrink (PLA.Address cred mAccId) = do
    cred' <- shrink cred
    mAccId' <- shrink mAccId
    (pure . PLA.Address cred $ mAccId') <|> (pure . PLA.Address cred' $ mAccId)

-- | @since 1.4.0
instance CoArbitrary PLA.Address where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary (PLA.Address cred mAccId) = coarbitrary cred . coarbitrary mAccId

-- | @since 1.4.0
instance Function PLA.Address where
  {-# INLINEABLE function #-}
  function = functionMap into outOf
    where
      into :: PLA.Address -> (PLA.Credential, Maybe PLA.AccountId)
      into (PLA.Address cred mAccId) = (cred, mAccId)
      outOf :: (PLA.Credential, Maybe PLA.AccountId) -> PLA.Address
      outOf = uncurry PLA.Address

-- | @since 1.4.0
instance Arbitrary PLA.TxOut where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    PLA.TxOut
      <$> arbitrary
      <*> (Value.getUtxoValue <$> arbitrary)
      <*> arbitrary
      <*> arbitrary
  {-# INLINEABLE shrink #-}
  shrink (PLA.TxOut addr val outD refScript) = do
    addr' <- shrink addr
    Value.UTxOValue val' <- shrink (Value.UTxOValue val)
    outD' <- shrink outD
    refScript' <- shrink refScript
    let shrink1 = PLA.TxOut addr val outD refScript'
    let shrink2 = PLA.TxOut addr val outD' refScript
    let shrink3 = PLA.TxOut addr val' outD refScript
    let shrink4 = PLA.TxOut addr' val outD refScript
    foldl' (<|>) [shrink1] [[shrink2], [shrink3], [shrink4]]

-- | @since 1.4.0
instance CoArbitrary PLA.TxOut where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary (PLA.TxOut addr val outD refScript) =
    coarbitrary addr
      . coarbitrary (Value.UTxOValue val)
      . coarbitrary outD
      . coarbitrary refScript

-- | @since 1.4.0
instance Function PLA.TxOut where
  {-# INLINEABLE function #-}
  function = functionMap into outOf
    where
      into ::
        PLA.TxOut ->
        (PLA.Address, Value.UTxOValue, PLA.OutputDatum, Maybe PLA.ScriptHash)
      into (PLA.TxOut addr val outD refScript) = (addr, Value.UTxOValue val, outD, refScript)
      outOf ::
        (PLA.Address, Value.UTxOValue, PLA.OutputDatum, Maybe PLA.ScriptHash) ->
        PLA.TxOut
      outOf (addr, Value.UTxOValue val, outD, refScript) = PLA.TxOut addr val outD refScript

-- | @since 1.4.0
instance Arbitrary PLA.AccountBalanceInterval where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    oneof
      [ PLA.AccountBalanceLowerBound <$> arbitrary
      , PLA.AccountBalanceUpperBound <$> arbitrary
      , do
          lb <- arbitrary
          delta <- arbitrary
          pure . PLA.AccountBalanceBothBounds lb $ lb + delta
      , PLA.AccountBalanceExact <$> arbitrary
      ]
  {-# INLINEABLE shrink #-}
  shrink = \case
    PLA.AccountBalanceLowerBound lb -> PLA.AccountBalanceLowerBound <$> shrink lb
    PLA.AccountBalanceUpperBound ub -> PLA.AccountBalanceUpperBound <$> shrink ub
    PLA.AccountBalanceBothBounds lb ub -> do
      lb' <- shrink lb
      ub' <- shrink ub
      guard (lb < ub')
      (pure . PLA.AccountBalanceBothBounds lb' $ ub)
        <|> (pure . PLA.AccountBalanceBothBounds lb $ ub')
    PLA.AccountBalanceExact b -> PLA.AccountBalanceExact <$> shrink b

-- | @since 1.4.0
instance CoArbitrary PLA.AccountBalanceInterval where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary = \case
    PLA.AccountBalanceLowerBound lb -> variant (0 :: Int) . coarbitrary lb
    PLA.AccountBalanceUpperBound ub -> variant (1 :: Int) . coarbitrary ub
    PLA.AccountBalanceBothBounds lb ub -> variant (2 :: Int) . coarbitrary lb . coarbitrary ub
    PLA.AccountBalanceExact b -> variant (3 :: Int) . coarbitrary b

-- | @since 1.4.0
instance Function PLA.AccountBalanceInterval where
  {-# INLINEABLE function #-}
  function = functionMap into outOf
    where
      into :: PLA.AccountBalanceInterval -> Either PLA.Lovelace (These PLA.Lovelace PLA.Lovelace)
      into = \case
        PLA.AccountBalanceLowerBound lb -> Right (This lb)
        PLA.AccountBalanceUpperBound ub -> Right (That ub)
        PLA.AccountBalanceBothBounds lb ub -> Right (These lb ub)
        PLA.AccountBalanceExact b -> Left b
      outOf ::
        Either PLA.Lovelace (These PLA.Lovelace PLA.Lovelace) ->
        PLA.AccountBalanceInterval
      outOf = \case
        Left b -> PLA.AccountBalanceExact b
        Right x -> these PLA.AccountBalanceLowerBound PLA.AccountBalanceUpperBound PLA.AccountBalanceBothBounds x

-- | @since 1.4.0
deriving via
  (PLA.Map PLA.AccountId PLA.AccountBalanceInterval)
  instance
    Arbitrary PLA.AccountBalanceIntervals

-- | @since 1.4.0
deriving via
  (PLA.Map PLA.AccountId PLA.AccountBalanceInterval)
  instance
    CoArbitrary PLA.AccountBalanceIntervals

-- | @since 1.4.0
instance Function PLA.AccountBalanceIntervals where
  {-# INLINEABLE function #-}
  function = functionMap coerce PLA.AccountBalanceIntervals

-- | @since 1.4.0
instance Arbitrary PLA.TxCert where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    oneof
      [ PLA.TxCertRegAccount <$> arbitrary <*> arbitrary
      , PLA.TxCertUnRegAccount <$> arbitrary <*> arbitrary
      , PLA.TxCertDelegAccount <$> arbitrary <*> arbitrary
      , PLA.TxCertRegAccountDeleg <$> arbitrary <*> arbitrary <*> arbitrary
      , PLA.TxCertRegDRep <$> arbitrary <*> arbitrary
      , PLA.TxCertUpdateDRep <$> arbitrary
      , PLA.TxCertUnRegDRep <$> arbitrary <*> arbitrary
      , PLA.TxCertPoolRegister <$> arbitrary <*> arbitrary
      , PLA.TxCertPoolRetire <$> arbitrary <*> arbitrary
      , PLA.TxCertAuthHotCommittee <$> arbitrary <*> arbitrary
      , PLA.TxCertResignColdCommittee <$> arbitrary
      ]
  {-# INLINEABLE shrink #-}
  shrink = \case
    PLA.TxCertRegAccount aid ll -> do
      aid' <- shrink aid
      ll' <- shrink ll
      (pure . PLA.TxCertRegAccount aid $ ll')
        <|> (pure . PLA.TxCertRegAccount aid' $ ll)
    PLA.TxCertUnRegAccount aid ll -> do
      aid' <- shrink aid
      ll' <- shrink ll
      (pure . PLA.TxCertUnRegAccount aid $ ll')
        <|> (pure . PLA.TxCertUnRegAccount aid' $ ll)
    PLA.TxCertDelegAccount aid d -> do
      aid' <- shrink aid
      d' <- shrink d
      (pure . PLA.TxCertDelegAccount aid $ d')
        <|> (pure . PLA.TxCertDelegAccount aid' $ d)
    PLA.TxCertRegAccountDeleg aid d ll -> do
      aid' <- shrink aid
      d' <- shrink d
      ll' <- shrink ll
      (pure . PLA.TxCertRegAccountDeleg aid d $ ll')
        <|> (pure . PLA.TxCertRegAccountDeleg aid d' $ ll)
        <|> (pure . PLA.TxCertRegAccountDeleg aid' d $ ll)
    PLA.TxCertRegDRep cred ll -> do
      cred' <- shrink cred
      ll' <- shrink ll
      (pure . PLA.TxCertRegDRep cred $ ll')
        <|> (pure . PLA.TxCertRegDRep cred' $ ll)
    PLA.TxCertUpdateDRep cred -> PLA.TxCertUpdateDRep <$> shrink cred
    PLA.TxCertUnRegDRep cred ll -> do
      cred' <- shrink cred
      ll' <- shrink ll
      (pure . PLA.TxCertUnRegDRep cred $ ll')
        <|> (pure . PLA.TxCertUnRegDRep cred' $ ll)
    PLA.TxCertPoolRegister pkh1 pkh2 -> do
      pkh1' <- shrink pkh1
      pkh2' <- shrink pkh2
      (pure . PLA.TxCertPoolRegister pkh1 $ pkh2')
        <|> (pure . PLA.TxCertPoolRegister pkh1' $ pkh2)
    PLA.TxCertPoolRetire pkh i -> do
      pkh' <- shrink pkh
      i' <- shrink i
      (pure . PLA.TxCertPoolRetire pkh $ i')
        <|> (pure . PLA.TxCertPoolRetire pkh' $ i)
    PLA.TxCertAuthHotCommittee ccc hcc -> do
      ccc' <- shrink ccc
      hcc' <- shrink hcc
      (pure . PLA.TxCertAuthHotCommittee ccc' $ hcc)
        <|> (pure . PLA.TxCertAuthHotCommittee ccc $ hcc')
    PLA.TxCertResignColdCommittee ccc -> PLA.TxCertResignColdCommittee <$> shrink ccc

-- TODO: CoArbitrary, Function

-- | @since 1.4.0
instance Arbitrary PLA.TxOutRef where
  {-# INLINEABLE arbitrary #-}
  arbitrary = do
    refId <- arbitrary
    refIdX <- getNonNegative <$> arbitrary
    pure . PLA.TxOutRef refId $ refIdX
  shrink (PLA.TxOutRef tid ix) =
    PLA.TxOutRef <$> shrink tid <*> (fmap getNonNegative . shrink . NonNegative $ ix)

-- TODO: CoArbitrary, Function

-- | @since 1.4.0
instance Arbitrary PLA.TxInInfo where
  {-# INLINEABLE arbitrary #-}
  arbitrary = PLA.TxInInfo <$> arbitrary <*> arbitrary
  {-# INLINEABLE shrink #-}
  shrink (PLA.TxInInfo outRef resolved) = do
    outRef' <- shrink outRef
    resolved' <- shrink resolved
    (pure . PLA.TxInInfo outRef $ resolved')
      <|> (pure . PLA.TxInInfo outRef' $ resolved)

-- | @since 1.4.0
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

-- | @since 1.4.0
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

-- | @since 1.4.0
deriving via (Maybe PLA.ScriptHash) instance Arbitrary PLA.Constitution

-- | @since 1.4.0
deriving via (Maybe PLA.ScriptHash) instance CoArbitrary PLA.Constitution

-- | @since 1.4.0
instance Function PLA.Constitution where
  {-# INLINEABLE function #-}
  function = functionMap coerce PLA.Constitution

-- | @since 1.4.0
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
          <*> (PLA.unsafeRatio . fromIntegral <$> chooseInt (1, 100) <*> pure 100)
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

-- | @since 1.4.0
instance Arbitrary PLA.ProposalProcedure where
  {-# INLINEABLE arbitrary #-}
  arbitrary = PLA.ProposalProcedure <$> arbitrary <*> arbitrary <*> arbitrary
  {-# INLINEABLE shrink #-}
  shrink (PLA.ProposalProcedure dep raddr ga) =
    PLA.ProposalProcedure <$> shrink dep <*> shrink raddr <*> shrink ga

-- | @since 1.4.0
instance Arbitrary PLA.ScriptPurpose where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    oneof
      [ PLA.Minting <$> arbitrary <*> arbitrary
      , PLA.Spending <$> arbitrary <*> arbitrary
      , PLA.Withdrawing <$> arbitrary <*> arbitrary
      , PLA.Certifying <$> arbitrary <*> arbitrary <*> arbitrary
      , PLA.Voting <$> arbitrary <*> arbitrary
      , PLA.Proposing <$> arbitrary <*> arbitrary <*> arbitrary
      , PLA.Guarding <$> arbitrary <*> arbitrary
      ]
  {-# INLINEABLE shrink #-}
  shrink = \case
    PLA.Minting sh cs -> do
      sh' <- shrink sh
      cs' <- shrink cs
      (pure . PLA.Minting sh $ cs')
        <|> (pure . PLA.Minting sh' $ cs)
    PLA.Spending sh tor -> do
      sh' <- shrink sh
      tor' <- shrink tor
      (pure . PLA.Spending sh $ tor')
        <|> (pure . PLA.Spending sh' $ tor)
    PLA.Withdrawing sh cred -> do
      sh' <- shrink sh
      cred' <- shrink cred
      (pure . PLA.Withdrawing sh $ cred')
        <|> (pure . PLA.Withdrawing sh' $ cred)
    PLA.Certifying sh i cert -> do
      sh' <- shrink sh
      i' <- shrink i
      cert' <- shrink cert
      (pure . PLA.Certifying sh i $ cert')
        <|> (pure . PLA.Certifying sh i' $ cert)
        <|> (pure . PLA.Certifying sh' i $ cert)
    PLA.Voting sh v -> do
      sh' <- shrink sh
      v' <- shrink v
      (pure . PLA.Voting sh $ v')
        <|> (pure . PLA.Voting sh' $ v)
    PLA.Proposing sh i pp -> do
      sh' <- shrink sh
      i' <- shrink i
      pp' <- shrink pp
      (pure . PLA.Proposing sh i $ pp')
        <|> (pure . PLA.Proposing sh i' $ pp)
        <|> (pure . PLA.Proposing sh' i $ pp)
    PLA.Guarding sh i -> do
      sh' <- shrink sh
      i' <- shrink i
      (pure . PLA.Guarding sh $ i')
        <|> (pure . PLA.Guarding sh' $ i)

-- TODO: CoArbitrary, Function

{- | Does not shrink.

@since 1.4.0
-}
instance Arbitrary PLA.POSIXTimeRange where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    oneof
      [ -- always
        pure . PLA.POSIXTimeRange Nothing $ Nothing
      , -- lower bound only
        PLA.POSIXTimeRange . Just <$> arbitrary <*> pure Nothing
      , -- upper bound only
        PLA.POSIXTimeRange Nothing . Just <$> arbitrary
      , -- lower and upper
        do
          range <- arbitrary
          start <- arbitrary
          pure . PLA.POSIXTimeRange (Just start) . Just $ start + range
      ]

-- | @since 1.4.0
instance Arbitrary PLA.TxInfo where
  {-# INLINEABLE arbitrary #-}
  arbitrary = do
    iid <- arbitrary
    subTxIx <- arbitrary
    ins <- getNonEmpty <$> arbitrary
    refIns <- arbitrary
    outs <- getNonEmpty <$> arbitrary
    mint <- arbitrary
    certs <- arbitrary
    wdrls <- arbitrary
    directs <- arbitrary
    abints <- arbitrary
    valid <- arbitrary
    guards <- arbitrary
    reqGuards <- arbitrary
    reds <- arbitrary
    dats <- arbitrary
    votes <- arbitrary
    pps <- arbitrary
    currT <- arbitrary
    tDonation <- arbitrary
    pure
      . PLA.TxInfo
        iid
        subTxIx
        ins
        refIns
        outs
        mint
        certs
        wdrls
        directs
        abints
        valid
        guards
        reqGuards
        reds
        dats
        votes
        pps
        currT
      $ tDonation
  {-# INLINEABLE shrink #-}
  shrink (PLA.TxInfo iid subTxIx ins refIns outs mint certs wdrls directs abints valid guards reqGuards reds dats votes pps currT tDonation) = do
    ins' <- getNonEmpty <$> shrink (NonEmpty ins)
    outs' <- getNonEmpty <$> shrink (NonEmpty outs)
    PLA.TxInfo
      <$> shrink iid
      <*> shrink subTxIx
      <*> pure ins'
      <*> shrink refIns
      <*> pure outs'
      <*> shrink mint
      <*> shrink certs
      <*> shrink wdrls
      <*> shrink directs
      <*> shrink abints
      <*> shrink valid
      <*> shrink guards
      <*> shrink reqGuards
      <*> shrink reds
      <*> shrink dats
      <*> shrink votes
      <*> shrink pps
      <*> shrink currT
      <*> shrink tDonation

-- TODO: CoArbitrary, Function

-- | @since 1.4.0
instance Arbitrary PLA.TopTxInfoSimplified where
  {-# INLINEABLE arbitrary #-}
  arbitrary = do
    ids <- arbitrary
    ins <- getNonEmpty <$> arbitrary
    refIns <- arbitrary
    outs <- getNonEmpty <$> arbitrary
    mints <- arbitrary
    burns <- arbitrary
    certs <- arbitrary
    wdrls <- arbitrary
    dds <- arbitrary
    valid <- arbitrary
    guards <- arbitrary
    reqGuards <- arbitrary
    redHashes <- arbitrary
    datas <- arbitrary
    votes <- arbitrary
    pps <- arbitrary
    currT <- arbitrary
    tDonations <- arbitrary
    pure
      . PLA.TopTxInfoSimplified
        ids
        ins
        refIns
        outs
        mints
        burns
        certs
        wdrls
        dds
        valid
        guards
        reqGuards
        redHashes
        datas
        votes
        pps
        currT
      $ tDonations
  {-# INLINEABLE shrink #-}
  shrink (PLA.TopTxInfoSimplified ids ins refIns outs mints burns certs wdrls dds valid guards reqGuards reds datas votes pps currT tDonations) = do
    ins' <- getNonEmpty <$> shrink (NonEmpty ins)
    outs' <- getNonEmpty <$> shrink (NonEmpty outs)
    PLA.TopTxInfoSimplified
      <$> shrink ids
      <*> pure ins'
      <*> shrink refIns
      <*> pure outs'
      <*> shrink mints
      <*> shrink burns
      <*> shrink certs
      <*> shrink wdrls
      <*> shrink dds
      <*> shrink valid
      <*> shrink guards
      <*> shrink reqGuards
      <*> shrink reds
      <*> shrink datas
      <*> shrink votes
      <*> shrink pps
      <*> shrink currT
      <*> shrink tDonations

-- TODO: CoArbitrary, Function

-- | @since 1.4.0
instance Arbitrary PLA.TopTxInfo where
  {-# INLINEABLE arbitrary #-}
  arbitrary = do
    trans <- arbitrary
    dats <- arbitrary
    starts <- arbitrary
    simple <- arbitrary
    pure . PLA.TopTxInfo trans dats starts $ simple
  {-# INLINEABLE shrink #-}
  shrink (PLA.TopTxInfo trans dats starts simple) =
    PLA.TopTxInfo <$> shrink trans <*> shrink dats <*> shrink starts <*> shrink simple

-- TODO: CoArbitrary, Function

-- | @since 1.4.0
instance Arbitrary PLA.ScriptInfo where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    oneof
      [ PLA.MintingScript <$> arbitrary
      , PLA.SpendingScript <$> arbitrary <*> arbitrary
      , PLA.WithdrawingScript <$> arbitrary
      , PLA.CertifyingScript <$> arbitrary <*> arbitrary
      , PLA.VotingScript <$> arbitrary
      , PLA.ProposingScript <$> arbitrary <*> arbitrary
      , PLA.GuardingScript <$> arbitrary <*> arbitrary
      ]
  {-# INLINEABLE shrink #-}
  shrink = \case
    PLA.MintingScript cs -> PLA.MintingScript <$> shrink cs
    PLA.SpendingScript outRef mDat -> do
      outRef' <- shrink outRef
      mDat' <- shrink mDat
      (pure . PLA.SpendingScript outRef $ mDat')
        <|> (pure . PLA.SpendingScript outRef' $ mDat)
    PLA.WithdrawingScript aid -> PLA.WithdrawingScript <$> shrink aid
    PLA.CertifyingScript i txCert -> do
      i' <- shrink i
      txCert' <- shrink txCert
      (pure . PLA.CertifyingScript i $ txCert')
        <|> (pure . PLA.CertifyingScript i' $ txCert)
    PLA.VotingScript v -> PLA.VotingScript <$> shrink v
    PLA.ProposingScript i pp -> do
      i' <- shrink i
      pp' <- shrink pp
      (pure . PLA.ProposingScript i $ pp')
        <|> (pure . PLA.ProposingScript i' $ pp)
    PLA.GuardingScript i mTxInfo -> do
      i' <- shrink i
      mTxInfo' <- shrink mTxInfo
      (pure . PLA.GuardingScript i $ mTxInfo')
        <|> (pure . PLA.GuardingScript i' $ mTxInfo)

-- TODO: CoArbitrary, Function

-- | @since 1.4.0
instance Arbitrary PLA.ScriptContext where
  {-# INLINEABLE arbitrary #-}
  arbitrary = PLA.ScriptContext <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  {-# INLINEABLE shrink #-}
  shrink (PLA.ScriptContext info red si sh) =
    PLA.ScriptContext <$> shrink info <*> shrink red <*> shrink si <*> shrink sh

-- TODO: CoArbitrary, Function
