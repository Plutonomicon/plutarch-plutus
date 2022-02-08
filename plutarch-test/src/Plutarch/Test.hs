{-# LANGUAGE ImpredicativeTypes #-}

-- | Common functions for testing Plutarch code
module Plutarch.Test (
  -- | Plutarch specific `Assertion` operators
  (#@?=),
  passert,
  -- | Golden testing
  --
  -- Typically you want to use `golden`. For grouping multiple goldens, use
  -- `goldens`. `golden'` allows you to specify an extra key (useful when
  -- creating multiple *separate* goldens in the same describe block)
  golden,
  golden',
  goldens,
) where

import qualified Data.Aeson.Text as Aeson
import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as TL
import System.FilePath
import Test.Syd (Spec, describe, getTestDescriptionPath, it, pureGoldenByteStringFile)
import Test.Tasty.HUnit

import Plutarch
import Plutarch.Benchmark
import Plutarch.Bool
import Plutarch.Evaluate
import qualified Plutus.V1.Ledger.Scripts as Scripts

eval :: Scripts.Script -> IO Scripts.Script
eval s = case evaluateScript s of
  Left e -> assertFailure $ "Script evaluation failed: " <> show e
  Right (_, _, x') -> pure x'

equal :: ClosedTerm a -> ClosedTerm b -> Assertion
equal x y = do
  p1 <- uplcE x
  p2 <- uplcE y
  p1 @?= p2
  where
    -- TODO: Do both variants somehow: `compile` and `shrink . compile`.
    uplcE = fmap printScript . eval . compile

-- | Like `@?=` but for Plutarch terms
(#@?=) :: forall (a :: PType) (b :: PType). ClosedTerm a -> ClosedTerm b -> Assertion
(#@?=) = equal

passert :: forall (a :: PType). ClosedTerm a -> Assertion
passert p = p #@?= pcon PTrue

-- TODO: remove the empty string hack
golden :: ClosedTerm a -> Spec
golden = golden' ""

-- | Make golden tests for the given Plutarch program.
golden' :: forall a. String -> ClosedTerm a -> Spec
golden' k p =
  goldens' k [("0", popaque p)]

{- | Like `golden` but for multiple programs

  Multiple programs use a single golden file. Each output separated from the
  keyword with a space.
-}
goldens :: [(String, ClosedTerm a)] -> Spec
goldens = goldens' ""

goldens' :: String -> [(String, ClosedTerm a)] -> Spec
goldens' k ps = do
  testAncestors <- fmap (drop 1 . reverse) $ getTestDescriptionPath
  let name = T.unpack $ T.intercalate "." testAncestors
      goldenKey = if null k then "golden" else k <> ".golden"
  describe goldenKey $ do
    let k' = if null k then "" else "." <> k
        nUplc = name <> k' <> ".uplc.golden"
        nBench = name <> k' <> ".bench.golden"
    -- Golden test for UPLC
    it "uplc" $
      pureGoldenByteStringFile ("goldens" </> nUplc) $
        multiGolden ps $ \p ->
          T.pack $ printTerm p
    -- Golden test for Plutus benchmarks
    it "bench" $
      pureGoldenByteStringFile ("goldens" </> nBench) $
        -- TODO: Do both variants somehow: `compile` and `shrink . compile`.
        multiGolden ps $ \p ->
          TL.toStrict $ Aeson.encodeToLazyText $ benchmarkScript' $ compile p

multiGolden :: forall a. [(String, a)] -> (a -> T.Text) -> ByteString
multiGolden xs f =
  encodeUtf8 $
    T.intercalate "\n" $
      (\(s, x) -> T.pack s <> " " <> f x) <$> xs

_shrink :: ClosedTerm a -> ClosedTerm a
_shrink = id -- TODO
