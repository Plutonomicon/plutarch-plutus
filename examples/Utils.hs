module Utils (equal, equal', fails, eval, expect, throws, traces) where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import Plutarch (ClosedTerm, PCon (pcon), Term, compile, printScript)
import Plutarch.Bool (PBool (PTrue))
import Plutarch.Evaluate (evaluateScript)
import qualified Plutus.V1.Ledger.Scripts as Scripts
import Test.Tasty.HUnit

eval :: HasCallStack => ClosedTerm a -> IO Scripts.Script
eval x = case evaluateScript $ compile x of
  Left e -> assertFailure $ "Script evaluation failed: " <> show e
  Right (_, _, x') -> pure x'

equal :: HasCallStack => ClosedTerm a -> ClosedTerm b -> Assertion
equal x y = do
  x' <- eval x
  y' <- eval y
  printScript x' @?= printScript y'

equal' :: HasCallStack => ClosedTerm a -> String -> Assertion
equal' x y = do
  x' <- eval x
  printScript x' @?= y

fails :: HasCallStack => ClosedTerm a -> Assertion
fails x =
  case evaluateScript $ compile x of
    Left (Scripts.EvaluationError _ _) -> mempty
    Left (Scripts.EvaluationException _ _) -> mempty
    Left e -> assertFailure $ "Script is malformed: " <> show e
    Right (_, _, s) -> assertFailure $ "Script didn't err: " <> printScript s

expect :: HasCallStack => ClosedTerm PBool -> Assertion
expect = equal (pcon PTrue :: Term s PBool)

throws :: ClosedTerm a -> Assertion
throws x =
  try @SomeException (putStrLn $ printScript $ compile x) >>= \case
    Right _ -> assertFailure "Supposed to throw"
    Left _ -> pure ()

traces :: ClosedTerm a -> [Text] -> Assertion
traces x sl =
  case evaluateScript $ compile x of
    Left e -> assertFailure $ "Script evaluation failed: " <> show e
    Right (_, traceLog, _) -> traceLog @?= sl
