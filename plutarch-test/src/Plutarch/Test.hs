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

import Control.Monad (void, when)
import qualified Data.Aeson.Text as Aeson
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Void (Void)
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
import Plutarch.Bool (PBool (PTrue))
import Plutarch.Evaluate (evaluateScript)
import qualified Plutus.V1.Ledger.Scripts as Scripts
import PlutusCore.Default
import Replace.Megaparsec
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import UntypedPlutusCore
import qualified UntypedPlutusCore as UPLC

{- |
    Like `shouldBe` but but for Plutarch terms
-}
pshouldBe :: ClosedTerm a -> ClosedTerm b -> Expectation
pshouldBe x y = do
  p1 <- fmap printScript $ eval $ compile x
  p2 <- fmap printScript $ eval $ compile y
  p1 `shouldBe` p2
  where
    eval :: Scripts.Script -> IO Scripts.Script
    eval s = case evaluateScript s of
      Left e -> expectationFailure $ "Script evaluation failed: " <> show e
      Right (_, _, x') -> pure x'

{- Like `@?=` but for Plutarch terms -}
(#@?=) :: ClosedTerm a -> ClosedTerm b -> Expectation
(#@?=) = pshouldBe

{- Asserts the term to be true -}
passert :: ClosedTerm a -> Expectation
passert p = p #@?= pcon PTrue

{- Asserts the term evaluates successfully without failing -}
psucceeds :: ClosedTerm a -> Expectation
psucceeds p =
  case evaluateScript (compile p) of
    Left _ -> expectationFailure $ "Term failed to evaluate"
    Right _ -> pure ()

{- Like `printTerm` but eliminates non-deterministic parts in UPLC.

-}
printTermDeterministic :: ClosedTerm a -> String
printTermDeterministic p =
  printScript $ rewrite $ compile p

rewriteFailMsgToBeDeterministic :: String -> String
rewriteFailMsgToBeDeterministic =
  streamEdit ghcPatternMatchMsg id
  where
    ghcPatternMatchMsg :: M.Parsec Void String String
    ghcPatternMatchMsg = do
      -- What's being replaced.
      s <- M.string "Pattern match failure"
      void $ M.takeRest
      -- The replacement
      pure $ s <> "..."

-- TODO: refactor
rewrite :: Scripts.Script -> Scripts.Script
rewrite (Scripts.Script (Program ann ver term)) =
  Scripts.Script (Program ann ver (go term))
  where
    go :: UPLC.Term DeBruijn DefaultUni DefaultFun () -> UPLC.Term DeBruijn DefaultUni DefaultFun ()
    go = \case
      (Apply ann (Force fann (Builtin ann' builtin)) (Constant ann1 (Some (ValueOf DefaultUniString s)))) ->
        (Apply ann (Force fann (Builtin ann' builtin)) (Constant ann1 (someValueOf DefaultUniString $ T.pack . rewriteFailMsgToBeDeterministic . T.unpack $ s)))
      LamAbs ann name t ->
        LamAbs ann name (go t)
      Apply ann t1 t2 ->
        Apply ann (go t1) (go t2)
      Force ann t ->
        Force ann (go t)
      Delay ann t ->
        Delay ann (go t)
      x -> x

{- Like `printTermDeterministic` but evaluates the term beforehand.

  All evaluation failures are treated as equivalent to a `perror`. Plutus does
  not provide an accurate way to tell if the program evalutes to `Error` or not;
  see https://github.com/input-output-hk/plutus/issues/4270

-}
printTermEvaluated :: ClosedTerm a -> String
printTermEvaluated p =
  case evaluateScript (rewrite $ compile p) of
    Left _ -> printTermDeterministic perror
    Right (_, _, x) -> printScript x

{- | Asserts that the term evaluates successfully with the given trace sequence

  See also: `plutarchDevFlagDescribe`
-}
ptraces :: ClosedTerm a -> [Text] -> Expectation
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
pfails :: ClosedTerm a -> Expectation
pfails p = do
  case evaluateScript (compile p) of
    Left _ -> pure ()
    Right _ -> expectationFailure $ "Term succeeded"

{- Whether to run all or a particular golden test

  Typically you want to use `All` -- this produces printTerm and benchmark
  goldens.

  Occasionally you want `PrintTerm` because you don't care to benchmark that
  program.

  Use `Bench` to only benchmark the program.
-}
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
golden pg p =
  goldens pg [("0", popaque p)]

{- | Like `golden` but for multiple programs

  Multiple programs use a single golden file. Each output separated from the
  keyword with a space.
-}
goldens :: PlutarchGolden -> [(String, ClosedTerm a)] -> Spec
goldens pg ps = do
  testAncestors <- fmap (drop 1 . reverse) $ getTestDescriptionPath
  let name = T.unpack $ T.intercalate "." testAncestors
  describe "golden" $ do
    -- Golden test for UPLC
    when (hasPrintTermGolden pg) $ do
      it "uplc" $
        pureGoldenTextFile ("goldens" </> name <> ".uplc.golden") $
          multiGolden ps $ \p ->
            T.pack $ printTermDeterministic p
      it "uplc.eval" $
        let evaluateds = flip fmap ps $ \(s, p) -> (s, printTermEvaluated p)
         in pureGoldenTextFile ("goldens" </> name <> ".uplc.eval.golden") $
              multiGolden evaluateds T.pack
    -- Golden test for Plutus benchmarks
    when (hasBenchGolden pg) $
      it "bench" $
        pureGoldenTextFile ("goldens" </> name <> ".bench.golden") $
          multiGolden ps $ \p ->
            TL.toStrict $ Aeson.encodeToLazyText $ benchmarkScript' $ rewrite $ compile p

multiGolden :: forall a. [(String, a)] -> (a -> T.Text) -> Text
multiGolden xs f =
  T.intercalate "\n" $
    (\(s, x) -> T.pack s <> " " <> f x) <$> xs
