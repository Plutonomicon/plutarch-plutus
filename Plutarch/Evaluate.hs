module Plutarch.Evaluate (evaluateScript) where

import Control.Monad.Except (MonadError, throwError)
import Data.Text (Text)
import Plutus.V1.Ledger.Scripts (Script)
import qualified Plutus.V1.Ledger.Scripts as Scripts
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
