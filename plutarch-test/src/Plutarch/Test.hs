{-# LANGUAGE ImpredicativeTypes #-}

module Plutarch.Test (
  -- | Plutarch specific test assertion operators
  (#@?=),
  passert,
  -- | Golden testing
  goldens,
) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden
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
    uplcE = fmap printScript . eval . compile

-- | Like `@?=` but for Plutarch terms
(#@?=) :: forall (a :: PType) (b :: PType). ClosedTerm a -> ClosedTerm b -> Assertion
(#@?=) = equal

passert :: forall (a :: PType). ClosedTerm a -> Assertion
passert p = p #@?= pcon PTrue

-- | Golden tests for a Plutarch program.
goldens :: String -> ClosedTerm a -> [TestTree]
goldens name p =
  [ -- Golden test for UPLC
    goldenVsString nUplc ("goldens" </> nUplc) $ do
      pure $ BS.fromStrict . encodeUtf8 . T.pack $ printTerm p
  , -- Golden test for Plutus benchmarks
    goldenVsString nBench ("goldens" </> nBench) $ do
      -- TODO: `compile` and `shrink . compile`.
      let b = benchmarkScript' $ compile p
      pure $ Aeson.encode b
  ]
  where
    nUplc = name <> ".uplc.golden"
    nBench = name <> ".bench.golden"

_shrink :: ClosedTerm a -> ClosedTerm a
_shrink = id -- TODO
