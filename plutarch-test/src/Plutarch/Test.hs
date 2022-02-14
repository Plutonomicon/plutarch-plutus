{-# LANGUAGE CPP #-}
{-# LANGUAGE ImpredicativeTypes #-}

-- | Common functions for testing Plutarch code
module Plutarch.Test (
  -- | Plutarch specific `Expectation` operators
  passert,
  pfails,
  psucceeds,
  ptraces,
  pshouldBe,
  (#@?=),
  plutarchDevFlagDescribe,
  -- | Golden testing
  --
  -- Typically you want to use `golden`. For grouping multiple goldens, use
  -- `goldens`.
  golden,
  goldens,
  PlutarchGolden (All, Bench, PrintTerm),
) where

import Control.Monad (when)
import qualified Data.Aeson.Text as Aeson
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import System.FilePath
import Test.Syd (
  Expectation,
  Spec,
  TestDefM,
  describe,
  expectationFailure,
  getTestDescriptionPath,
  it,
  pureGoldenTextFile,
  shouldBe,
 )

import Plutarch
import Plutarch.Benchmark (benchmarkScript')
import Plutarch.Bool
import Plutarch.Evaluate
import qualified Plutus.V1.Ledger.Scripts as Scripts

{- |
    Like `shouldBe` but but for Plutarch terms
-}
pshouldBe :: forall (a :: PType) (b :: PType). ClosedTerm a -> ClosedTerm b -> Expectation
pshouldBe x y = do
  p1 <- printTermEvaluated x
  p2 <- printTermEvaluated y
  p1 `shouldBe` p2
  where
    printTermEvaluated :: forall a. ClosedTerm a -> IO String
    printTermEvaluated = fmap printScript . eval . compile

{- Like `@?=` but for Plutarch terms -}
(#@?=) :: forall (a :: PType) (b :: PType). ClosedTerm a -> ClosedTerm b -> Expectation
(#@?=) = pshouldBe

eval :: Scripts.Script -> IO Scripts.Script
eval s = case evaluateScript s of
  Left e -> expectationFailure $ "Script evaluation failed: " <> show e
  Right (_, _, x') -> pure x'

{- Asserts the term to be true -}
passert :: forall (a :: PType). ClosedTerm a -> Expectation
passert p = p #@?= pcon PTrue

{- Asserts the term evaluates successfully without failing -}
psucceeds :: forall (a :: PType). ClosedTerm a -> Expectation
psucceeds p =
  case evaluateScript (compile p) of
    Left _ -> expectationFailure $ "Term failed to evaluate"
    Right _ -> pure ()

{- | Asserts that the term evaluates successfully with the given trace sequence

  See also: `plutarchDevFlagDescribe`
-}
ptraces :: forall (a :: PType). ClosedTerm a -> [Text] -> Expectation
ptraces p develTraces =
  case evaluateScript (compile p) of
    Left _ -> expectationFailure $ "Term failed to evaluate"
    Right (_, traceLog, _) -> do
#ifdef Development 
      traceLog `shouldBe` develTraces
#else
      -- Tracing is disabled in non-developed modes, so we should expect an
      -- empty trace log.
      let noTraces = const [] develTraces
      traceLog `shouldBe` noTraces
#endif

{- | Like `describe`, but determines description from `Development` CPP flag

  Useful to create two sets of othersise identical group of tests that differ
  only by `Development` flag. This has the effect of creating two sets of golden
  tests (with different filepaths) for corresponding flag values.

  Typically meant to be used in conjunction with `ptraces`.
-}
plutarchDevFlagDescribe :: forall (outers :: [Type]) inner. TestDefM outers inner () -> TestDefM outers inner ()

-- CPP support isn't great in fourmolu.
{- ORMOLU_DISABLE -}
plutarchDevFlagDescribe m =
#ifdef Development 
  describe "dev=true" m
#else
  describe "dev=false" m
#endif
{- ORMOLU_ENABLE -}

{- Asserts the term evaluates without success -}
pfails :: forall (a :: PType). ClosedTerm a -> Expectation
pfails p = do
  case evaluateScript (compile p) of
    Left _ -> pure ()
    Right _ -> expectationFailure $ "Term succeeded"

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
