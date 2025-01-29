{-# OPTIONS_GHC -Wno-orphans #-}

module PlutusLedgerApi.V1.Orphans.Interval () where

import Data.Word (Word32)
import PlutusLedgerApi.V1 qualified as PLA
import PlutusLedgerApi.V1.Interval qualified as Interval
import PlutusLedgerApi.V1.Orphans.Time ()
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  Arbitrary1 (liftArbitrary, liftShrink),
  CoArbitrary (coarbitrary),
  Function (function),
  frequency,
  functionMap,
  getNonNegative,
  oneof,
  variant,
 )

{- | This instance does not bias the constructor choice: it is equally likely to
produce 'PLA.Finite', 'PLA.NegInf' and 'PLA.PosInf'. Bear this in mind when
using: in particular, the instance for 'PLA.Interval' /does not/ make use of
this instance.

@since 1.0.0
-}
instance Arbitrary1 PLA.Extended where
  {-# INLINEABLE liftArbitrary #-}
  liftArbitrary genInner =
    oneof
      [ pure PLA.NegInf
      , PLA.Finite <$> genInner
      , pure PLA.PosInf
      ]
  {-# INLINEABLE liftShrink #-}
  liftShrink shrinkInner = \case
    PLA.NegInf -> []
    PLA.Finite x -> PLA.Finite <$> shrinkInner x
    PLA.PosInf -> []

{- | This makes use of the 'Arbitrary1' instance of 'PLA.Extended' internally,
and thus is subject to the same caveats.

@since 1.0.0
-}
instance Arbitrary a => Arbitrary (PLA.Extended a) where
  {-# INLINEABLE arbitrary #-}
  arbitrary = liftArbitrary arbitrary
  {-# INLINEABLE shrink #-}
  shrink = liftShrink shrink

-- | @since 1.0.0
instance CoArbitrary a => CoArbitrary (PLA.Extended a) where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary = \case
    PLA.NegInf -> variant (0 :: Int)
    PLA.Finite x -> variant (1 :: Int) . coarbitrary x
    PLA.PosInf -> variant (2 :: Int)

-- | @since 1.0.0
instance Function a => Function (PLA.Extended a) where
  {-# INLINEABLE function #-}
  function = functionMap into outOf
    where
      into :: PLA.Extended a -> Maybe (Maybe a)
      into = \case
        PLA.NegInf -> Nothing
        PLA.PosInf -> Just Nothing
        PLA.Finite x -> Just (Just x)
      outOf :: Maybe (Maybe a) -> PLA.Extended a
      outOf = \case
        Nothing -> PLA.NegInf
        Just Nothing -> PLA.PosInf
        Just (Just x) -> PLA.Finite x

{- | This makes use of the 'Arbitrary1' instance of 'PLA.Extended' internally,
and thus is subject to the same caveats. Furthermore, in cases where it makes
sense to talk about open and closed bounds, this instance produces open and
closed bounds with equal probability. Keep these in mind when using this
instance; in particular, the instance for 'PLA.Interval' /does not/ make use
of this instance.

@since 1.0.0
-}
instance Arbitrary (PLA.LowerBound PLA.POSIXTime) where
  {-# INLINEABLE arbitrary #-}
  arbitrary = do
    e <- arbitrary
    case e of
      -- For a finite bound, it makes sense to talk about it being open or
      -- closed.
      PLA.Finite _ -> PLA.LowerBound e <$> arbitrary
      -- If the bound is infinite, it _must_ be open.
      _ -> pure . PLA.LowerBound e $ False
  {-# INLINEABLE shrink #-}
  shrink (PLA.LowerBound e c) = case e of
    PLA.Finite _ -> PLA.LowerBound <$> shrink e <*> shrink c
    -- Negative or positive infinity bounds can't really shrink sensibly
    _ -> []

-- | @since 1.0.0
instance CoArbitrary a => CoArbitrary (PLA.LowerBound a) where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary (PLA.LowerBound e c) = coarbitrary e . coarbitrary c

-- | @since 1.0.0
instance Function a => Function (PLA.LowerBound a) where
  {-# INLINEABLE function #-}
  function = functionMap (\(PLA.LowerBound e c) -> (e, c)) (uncurry PLA.LowerBound)

{- | This makes use of the 'Arbitrary1' instance of 'PLA.Extended' internally,
and thus is subject to the same caveats. Furthermore, in cases where it makes
sense to talk about open and closed bounds, this instance produces open and
closed bounds with equal probability. Keep these in mind when using this
instance; in particular, the instance for 'PLA.Interval' /does not/ make use
of this instance.

@since 1.0.0
-}
instance Arbitrary (PLA.UpperBound PLA.POSIXTime) where
  {-# INLINEABLE arbitrary #-}
  arbitrary = do
    e <- arbitrary
    case e of
      -- For a finite bound, it makes sense to talk about it being open or
      -- closed.
      PLA.Finite _ -> PLA.UpperBound e <$> arbitrary
      -- If the bound is infinite, it _must_ be open.
      _ -> pure . PLA.UpperBound e $ False
  {-# INLINEABLE shrink #-}
  shrink (PLA.UpperBound e c) = case e of
    PLA.Finite _ -> PLA.UpperBound <$> shrink e <*> shrink c
    -- Negative or positive infinity bounds can't really shrink sensibly
    _ -> []

-- | @since 1.0.0
instance CoArbitrary a => CoArbitrary (PLA.UpperBound a) where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary (PLA.UpperBound e c) = coarbitrary e . coarbitrary c

-- | @since 1.0.0
instance Function a => Function (PLA.UpperBound a) where
  {-# INLINEABLE function #-}
  function = functionMap (\(PLA.UpperBound e c) -> (e, c)) (uncurry PLA.UpperBound)

{- | We provide an instance specialized to 'PLA.POSIXTime', rather than a more
general one, as it doesn't make much sense to talk about 'PLA.Interval's of
arbitrary types in general. Furthermore, this is the only instance we
actually use, so there's no real loss there.

This instance tries to make time intervals as fairly as possible, while also
ensuring that they're sensibly formed. We work under the assumption of a
32-bit epoch: while this is _technically_ not going to last much longer,
we're safe until about 2030 on that basis, which should be enough for now.

We choose not to shrink intervals, as this is surprisingly complex: in at
least one common case, it's not even possible to write a shrinker that will
ever 'bottom out', due to us having infinite bounds!

@since 1.0.0
-}
instance Arbitrary (PLA.Interval PLA.POSIXTime) where
  {-# INLINEABLE arbitrary #-}
  arbitrary = do
    let epochSize = fromIntegral (maxBound :: Word32)
    lowerBound <-
      frequency
        [ (1, pure PLA.NegInf)
        , (1, pure PLA.PosInf)
        , (epochSize, PLA.Finite . getNonNegative <$> arbitrary)
        ]
    case lowerBound of
      -- With a finite lower bound, it makes sense to talk about an upper one
      PLA.Finite x -> do
        lowerClosure <- arbitrary
        let lower = PLA.LowerBound lowerBound lowerClosure
        -- To ensure we generate something sensible for the upper bound, we
        -- either generate a 'diff', or positive infinity.
        whatUpper <-
          frequency
            [ (1, pure . Left $ PLA.PosInf)
            , (epochSize, Right . getNonNegative <$> arbitrary)
            ]
        case whatUpper of
          -- If we have an infinite upper bound, we know it will be open.
          Left _ -> do
            let upper = PLA.UpperBound PLA.PosInf False
            pure . PLA.Interval lower $ upper
          Right diff -> case (diff, lowerClosure) of
            -- A diff of 0 means we can only have a singleton closure sensibly.
            (0, _) -> pure . Interval.singleton $ x
            -- A diff of 1 with an open lower bound means we either have a
            -- singleton closure or an empty one.
            (1, False) -> do
              upperClosure <- arbitrary
              pure $
                if upperClosure
                  then Interval.singleton x
                  else Interval.never
            -- A diff of 1 with a closed lower bound is either a singleton
            -- closure or one with two values.
            (1, True) -> do
              upperClosure <- arbitrary
              pure $
                if upperClosure
                  then PLA.Interval lower . PLA.UpperBound (PLA.Finite (x + diff)) $ upperClosure
                  else Interval.singleton x
            -- A diff bigger than 1 can be treated uniformly.
            (_, _) -> PLA.Interval lower . PLA.UpperBound (PLA.Finite (x + diff)) <$> arbitrary
      -- With an negative infinite lower bound, we know it will be open.
      PLA.NegInf -> do
        let lower = PLA.LowerBound lowerBound False
        -- To ensure we generate something sensible for the upper bound, we
        -- do not attempt to generate NegInf
        upperBound <-
          frequency
            [ (1, pure PLA.PosInf)
            , (epochSize, PLA.Finite . getNonNegative <$> arbitrary)
            ]
        case upperBound of
          -- With a finite upper bound, we just choose a closure and move on.
          PLA.Finite _ -> do
            upper <- PLA.UpperBound upperBound <$> arbitrary
            pure . PLA.Interval lower $ upper
          -- With an infinite upper bound, we have the range that includes
          -- everything. We use the canonical choice provided by
          -- Interval.always.
          _ -> pure Interval.always
      -- With an positive infinite lower bound, we have the empty interval, and
      -- can choose any representation of such that we like. We use the
      -- canonical choice provided by Interval.never.
      PLA.PosInf -> pure Interval.never

-- | @since 1.0.0
instance CoArbitrary a => CoArbitrary (PLA.Interval a) where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary (PLA.Interval lower upper) = coarbitrary lower . coarbitrary upper

-- | @since 1.0.0
instance Function a => Function (PLA.Interval a) where
  {-# INLINEABLE function #-}
  function = functionMap (\(PLA.Interval lower upper) -> (lower, upper)) (uncurry PLA.Interval)
