{-# LANGUAGE ImpredicativeTypes #-}

-- | Common functions for testing Plutarch code
module Plutarch.Test (
  -- | Plutarch specific test assertion operators
  (#@?=),
  passert,
  -- | Golden testing
  golden,
  goldens,
) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit

import qualified Data.Aeson.Text as Aeson
import qualified Data.Text.Lazy as TL
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

-- | Make golden tests for the given Plutarch program.
golden :: String -> ClosedTerm a -> [TestTree]
golden name p =
  goldens name [("0", p)]

{- | Like `golden` but for multiple programs

  Multiple programs use a single golden file. Each output separated from the
  keyword with a space.
-}
goldens :: String -> [(String, ClosedTerm a)] -> [TestTree]
goldens name ps =
  [ -- Golden test for UPLC
    goldenVsString nUplc ("goldens" </> nUplc) $ do
      pure $
        multiGolden ps $ \p ->
          T.pack $ printTerm p
  , -- Golden test for Plutus benchmarks
    goldenVsString nBench ("goldens" </> nBench) $ do
      -- TODO: Do both variants somehow: `compile` and `shrink . compile`.
      pure $
        multiGolden ps $ \p ->
          TL.toStrict $ Aeson.encodeToLazyText $ benchmarkScript' $ compile p
  ]
  where
    nUplc = name <> ".uplc.golden"
    nBench = name <> ".bench.golden"
    multiGolden xs f =
      BS.fromStrict . encodeUtf8 $
        T.intercalate "\n" $
          (\(s, x) -> T.pack s <> " " <> f x) <$> xs

_shrink :: ClosedTerm a -> ClosedTerm a
_shrink = id -- TODO
