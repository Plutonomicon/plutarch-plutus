{-# LANGUAGE ImpredicativeTypes #-}

-- | Common functions for testing Plutarch code
module Plutarch.Test (
  -- | Plutarch specific test assertion operators
  (#@?=),
  passert,
  -- | Golden testing
  golden,
  goldens,
  -- | Monadic test trees
  TestTreeM,
  ttPath,
  ttGroup,
  tt,
  runTestTreeM,
) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer
import qualified Data.Aeson.Text as Aeson
import Data.List.NonEmpty
import qualified Data.Text.Lazy as TL
import Plutarch
import Plutarch.Benchmark
import Plutarch.Bool
import Plutarch.Evaluate
import qualified Plutus.V1.Ledger.Scripts as Scripts
import qualified Data.List.NonEmpty as NEL
import Data.List (intercalate)

{- | Monadic building of `TestTree`

Gives access to the current test name hierarchy, which is in turn used for other
things (like naming the golden tests).

Use as:
>>> runTestTreeM "main" $ do 
>>>   ttGroup "foo" $ do
>>>     tt $ testCase "bar" ...
>>>     ttGroup "qux" $ do 
>>>       ...

The above is equivalent to:
>>> testGroup "main" 
>>>   [ testGroup "foo"
>>>     [ testCase "bar" ...
>>>     , testGroup "aux" 
>>>       [ testCase "aux" ... ]
>>>     ]
>>>   ]

In adddition to nicer syntax, you can use `ttPath` to get access to the test
hierarchy path.
-}
newtype TestTreeM a = TestTreeM {unTestTreeM :: StateT (NonEmpty TestName) (Writer [TestTree]) a}
  deriving newtype (Functor, Applicative, Monad)

runTestTreeM :: TestName -> TestTreeM a -> TestTree
runTestTreeM k =
  testGroup k . runTestTreeM'
  where
    runTestTreeM' =
      snd . runWriter . flip runStateT (k :| []) . unTestTreeM

{-| Return the path from root to current. -}
ttPath :: TestTreeM (NonEmpty TestName)
ttPath = fmap NEL.reverse $ TestTreeM get

ttPathString :: TestTreeM TestName
ttPathString = 
  intercalate "." . NEL.toList <$> ttPath

{-| Create a new test sub-tree -}
ttGroup :: TestName -> TestTreeM () -> TestTreeM ()
ttGroup k m = do
  TestTreeM $ modify (k <|)
  tt $ runTestTreeM k m

{-| Add under current sub-tree 
-}
tt :: TestTree -> TestTreeM ()
tt t =
  ttLeaves [t]
  where
    ttLeaves :: [TestTree] -> TestTreeM ()
    ttLeaves ts =
      TestTreeM $ lift $ tell ts

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
golden :: String -> ClosedTerm a -> TestTreeM ()
golden name p =
  goldens name [("0", p)]

{- | Like `golden` but for multiple programs

  Multiple programs use a single golden file. Each output separated from the
  keyword with a space.
-}
goldens :: String -> [(String, ClosedTerm a)] -> TestTreeM ()
goldens name ps = do
  -- Golden test for UPLC
  tt $
    goldenVsString nUplc ("goldens" </> nUplc) $ do
      pure $
        multiGolden ps $ \p ->
          T.pack $ printTerm p
  -- Golden test for Plutus benchmarks
  tt $
    goldenVsString nBench ("goldens" </> nBench) $ do
      -- TODO: Do both variants somehow: `compile` and `shrink . compile`.
      pure $
        multiGolden ps $ \p ->
          TL.toStrict $ Aeson.encodeToLazyText $ benchmarkScript' $ compile p
  where
    nUplc = name <> ".uplc.golden"
    nBench = name <> ".bench.golden"
    multiGolden xs f =
      BS.fromStrict . encodeUtf8 $
        T.intercalate "\n" $
          (\(s, x) -> T.pack s <> " " <> f x) <$> xs

_shrink :: ClosedTerm a -> ClosedTerm a
_shrink = id -- TODO
