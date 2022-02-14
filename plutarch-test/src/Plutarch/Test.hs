{-# LANGUAGE ImpredicativeTypes #-}

-- | Common functions for testing Plutarch code
module Plutarch.Test (
  -- | Plutarch specific `Assertion` operators
  passert,
  pfails,
  psucceeds,
  pshouldBe,
  (#@?=),
  -- | Golden testing
  --
  -- Typically you want to use `golden`. For grouping multiple goldens, use
  -- `goldens`. `golden'` allows you to specify an extra key (useful when
  -- creating multiple *separate* goldens in the same describe block)
  golden,
  golden',
  goldens,
  PlutarchGolden (All, Bench, PrintTerm),
) where

import Control.Monad (when)
import qualified Data.Aeson.Text as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import System.FilePath
import Test.Syd (Expectation, Spec, describe, getTestDescriptionPath, it, pureGoldenTextFile, shouldBe)
import Test.Tasty.HUnit

import Plutarch
import Plutarch.Benchmark
import Plutarch.Bool
import Plutarch.Evaluate
import qualified Plutus.V1.Ledger.Scripts as Scripts

{- |
    like `shouldBe` but but for `PTypes`s
-}
pshouldBe :: forall (a :: PType) (b :: PType). ClosedTerm a -> ClosedTerm b -> Expectation
pshouldBe x y = do
  p1 <- printTermEvaluated x
  p2 <- printTermEvaluated y
  p1 `shouldBe` p2

{- Like `@?=` but for Plutarch terms -}
(#@?=) :: forall (a :: PType) (b :: PType). ClosedTerm a -> ClosedTerm b -> Assertion
(#@?=) = pshouldBe

eval :: Scripts.Script -> IO Scripts.Script
eval s = case evaluateScript s of
  Left e -> assertFailure $ "Script evaluation failed: " <> show e
  Right (_, _, x') -> pure x'

{- Like `printTerm` but the prints evaluated output of it -}
printTermEvaluated :: ClosedTerm a -> IO String
printTermEvaluated = fmap printScript . eval . compile

{- Asserts the term to be true -}
passert :: forall (a :: PType). ClosedTerm a -> Assertion
passert p = p #@?= pcon PTrue

psucceeds :: forall (a :: PType). ClosedTerm a -> Assertion
psucceeds p =
  case evaluateScript (compile p) of
    Left _ -> assertFailure $ "Term failed to evaluate"
    Right _ -> pure ()

{- Asserts the term evaluates without success -}
pfails :: forall (a :: PType). ClosedTerm a -> Assertion
pfails p = do
  case evaluateScript (compile p) of
    Left _ -> pure ()
    Right _ -> assertFailure $ "Term succeeded"

{- Whether to run all or a particular golden test -}
data PlutarchGolden
  = All
  | Bench
  | PrintTerm
  deriving stock (Eq, Show)

hasBenchGolden :: PlutarchGolden -> Bool
hasBenchGolden = \case
  PrintTerm -> False
  _ -> True

hasPrintTermGolden :: PlutarchGolden -> Bool
hasPrintTermGolden = \case
  Bench -> False
  _ -> True

{- Run golden tests on the given Plutarch program -}
golden :: PlutarchGolden -> ClosedTerm a -> Spec
golden pg = golden' pg Nothing

-- | Make golden tests for the given Plutarch program.
golden' :: forall a. PlutarchGolden -> Maybe String -> ClosedTerm a -> Spec
golden' pg mk p =
  goldens' pg mk [("0", popaque p)]

{- | Like `golden` but for multiple programs

  Multiple programs use a single golden file. Each output separated from the
  keyword with a space.
-}
goldens :: PlutarchGolden -> [(String, ClosedTerm a)] -> Spec
goldens pg = goldens' pg Nothing

goldens' :: PlutarchGolden -> Maybe String -> [(String, ClosedTerm a)] -> Spec
goldens' pg mk ps = do
  testAncestors <- fmap (drop 1 . reverse) $ getTestDescriptionPath
  let name = T.unpack $ T.intercalate "." testAncestors
      goldenKey = maybe "golden" (<> ".golden") mk
  describe goldenKey $ do
    let k = maybe "" ("." <>) mk
        nUplc = name <> k <> ".uplc.golden"
        nBench = name <> k <> ".bench.golden"
    -- Golden test for UPLC
    when (hasPrintTermGolden pg) $
      it "uplc" $
        pureGoldenTextFile ("goldens" </> nUplc) $
          multiGolden ps $ \p ->
            T.pack $ printTerm p
    -- Golden test for Plutus benchmarks
    when (hasBenchGolden pg) $
      it "bench" $
        pureGoldenTextFile ("goldens" </> nBench) $
          multiGolden ps $ \p ->
            TL.toStrict $ Aeson.encodeToLazyText $ benchmarkScript' $ compile p

multiGolden :: forall a. [(String, a)] -> (a -> T.Text) -> Text
multiGolden xs f =
  T.intercalate "\n" $
    (\(s, x) -> T.pack s <> " " <> f x) <$> xs
