{-# OPTIONS_GHC -Wno-orphans #-}

module PlutusLedgerApi.V1.Orphans.DCert () where

import PlutusLedgerApi.QuickCheck.Utils (fromAsWord64)
import PlutusLedgerApi.V1 qualified as PLA
import PlutusLedgerApi.V1.Orphans.Credential ()
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  CoArbitrary (coarbitrary),
  Function (function),
  NonNegative (NonNegative),
  functionMap,
  getNonNegative,
  oneof,
  variant,
 )

-- | @since 1.0.0
instance Arbitrary PLA.DCert where
  {-# INLINEABLE arbitrary #-}
  arbitrary =
    oneof
      [ PLA.DCertDelegRegKey <$> arbitrary
      , PLA.DCertDelegDeRegKey <$> arbitrary
      , PLA.DCertDelegDelegate <$> arbitrary <*> arbitrary
      , PLA.DCertPoolRegister <$> arbitrary <*> arbitrary
      , PLA.DCertPoolRetire <$> arbitrary <*> (fromAsWord64 <$> arbitrary)
      , pure PLA.DCertGenesis
      , pure PLA.DCertMir
      ]
  {-# INLINEABLE shrink #-}
  shrink = \case
    PLA.DCertDelegRegKey sc -> PLA.DCertDelegRegKey <$> shrink sc
    PLA.DCertDelegDeRegKey sc -> PLA.DCertDelegDeRegKey <$> shrink sc
    -- PubKeyHash can't shrink, so we just pass it through, as otherwise, the
    -- semantics of shrinking would mean the whole think can't shrink.
    PLA.DCertDelegDelegate sc pkh -> PLA.DCertDelegDelegate <$> shrink sc <*> pure pkh
    -- PubKeyHash can't shrink, so neither can this.
    PLA.DCertPoolRegister _ _ -> []
    -- PubKeyHash can't shrink, so we just pass it through, as otherwise, the
    -- semantics of shrinking would mean the whole think can't shrink.
    PLA.DCertPoolRetire pkh e ->
      PLA.DCertPoolRetire pkh . getNonNegative <$> shrink (NonNegative e)
    -- None of the other constructors have any data, so we don't shrink them.
    _ -> []

-- | @since 1.0.0
instance CoArbitrary PLA.DCert where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary = \case
    PLA.DCertDelegRegKey sc -> variant (0 :: Int) . coarbitrary sc
    PLA.DCertDelegDeRegKey sc -> variant (1 :: Int) . coarbitrary sc
    PLA.DCertDelegDelegate sc pkh -> variant (2 :: Int) . coarbitrary sc . coarbitrary pkh
    PLA.DCertPoolRegister pkh pkh' -> variant (3 :: Int) . coarbitrary pkh . coarbitrary pkh'
    PLA.DCertPoolRetire pkh e -> variant (4 :: Int) . coarbitrary pkh . coarbitrary e
    PLA.DCertGenesis -> variant (5 :: Int)
    PLA.DCertMir -> variant (6 :: Int)

-- | @since 1.0.0
instance Function PLA.DCert where
  {-# INLINEABLE function #-}
  function = functionMap into outOf
    where
      into ::
        PLA.DCert ->
        Maybe
          ( Maybe
              ( Either
                  PLA.StakingCredential
                  ( Either
                      PLA.StakingCredential
                      ( Either
                          (PLA.StakingCredential, PLA.PubKeyHash)
                          ( Either (PLA.PubKeyHash, PLA.PubKeyHash) (PLA.PubKeyHash, Integer)
                          )
                      )
                  )
              )
          )
      into = \case
        PLA.DCertGenesis -> Nothing
        PLA.DCertMir -> Just Nothing
        PLA.DCertDelegRegKey sc -> Just (Just (Left sc))
        PLA.DCertDelegDeRegKey sc -> Just (Just (Right (Left sc)))
        PLA.DCertDelegDelegate sc pkh -> Just (Just (Right (Right (Left (sc, pkh)))))
        PLA.DCertPoolRegister pkh pkh' -> Just (Just (Right (Right (Right (Left (pkh, pkh'))))))
        PLA.DCertPoolRetire pkh e -> Just (Just (Right (Right (Right (Right (pkh, e))))))
      outOf ::
        Maybe
          ( Maybe
              ( Either
                  PLA.StakingCredential
                  ( Either
                      PLA.StakingCredential
                      ( Either
                          (PLA.StakingCredential, PLA.PubKeyHash)
                          ( Either (PLA.PubKeyHash, PLA.PubKeyHash) (PLA.PubKeyHash, Integer)
                          )
                      )
                  )
              )
          ) ->
        PLA.DCert
      outOf = \case
        Nothing -> PLA.DCertGenesis
        Just Nothing -> PLA.DCertMir
        Just (Just (Left sc)) -> PLA.DCertDelegRegKey sc
        Just (Just (Right (Left sc))) -> PLA.DCertDelegDeRegKey sc
        Just (Just (Right (Right (Left (sc, pkh))))) -> PLA.DCertDelegDelegate sc pkh
        Just (Just (Right (Right (Right (Left (pkh, pkh')))))) -> PLA.DCertPoolRegister pkh pkh'
        Just (Just (Right (Right (Right (Right (pkh, e)))))) -> PLA.DCertPoolRetire pkh e
