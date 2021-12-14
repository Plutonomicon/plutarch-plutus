{-# LANGUAGE TypeApplications #-}

module Plutarch.Evaluate (eval, evalWithArgs) where

import Control.Monad.Except (MonadError, throwError)
import Data.Text (Text)
import Plutus.V1.Ledger.Scripts (Script)
import qualified Plutus.V1.Ledger.Scripts as Scripts
import qualified PlutusCore.Data as PLC
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget)
import PlutusTx.Evaluation (evaluateCekTrace)
import UntypedPlutusCore (
  DefaultFun,
  DefaultUni,
  Name,
  Term,
 )
import qualified UntypedPlutusCore.Evaluation.Machine.Cek as UPLC

-- Stolen from pluto, thanks Morgan

-- | Evaluate a UPLC script
eval :: Script -> Either Scripts.ScriptError (ExBudget, [Text], Term Name DefaultUni DefaultFun ())
eval = evaluateScript @(Either Scripts.ScriptError)

-- | Like `eval`, but with a list of arguments to pass to the script.
evalWithArgs :: [PLC.Data] -> Script -> Either Scripts.ScriptError (ExBudget, [Text], Term Name DefaultUni DefaultFun ())
evalWithArgs args =
  evaluateScript @(Either Scripts.ScriptError)
    . flip Scripts.applyArguments args

{- | Evaluate a script, returning the trace log and term result.

 This is same as `Plutus.V1.Ledger.Scripts.evaluateScript`, but returns the
 script result as well.
-}
evaluateScript ::
  forall m uni fun.
  (MonadError Scripts.ScriptError m, uni ~ DefaultUni, fun ~ DefaultFun) =>
  Script ->
  m (ExBudget, [Text], Term Name uni fun ())
evaluateScript s = do
  p <- case Scripts.mkTermToEvaluate s of
    Right p -> pure p
    Left e -> throwError $ Scripts.MalformedScript $ show e
  let (logOut, UPLC.TallyingSt _ budget, result) = evaluateCekTrace p
  term <- case result of
    Right term -> pure term
    Left errWithCause@(UPLC.ErrorWithCause err cause) ->
      throwError $ case err of
        UPLC.InternalEvaluationError internalEvalError ->
          Scripts.EvaluationException (show errWithCause) (show internalEvalError)
        UPLC.UserEvaluationError evalError ->
          -- We use `show` here because plutus doesn't expose mkError
          Scripts.EvaluationError logOut (show (evalError, cause))
  pure (budget, logOut, term)
