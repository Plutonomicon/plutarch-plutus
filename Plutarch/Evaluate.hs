{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Plutarch.Evaluate (evaluateScript) where
  
import Control.Monad.Except (runExceptT)
import Data.Text (Text)
import Plutus.V1.Ledger.Scripts (Script(Script))
import qualified Plutus.V1.Ledger.Scripts as Scripts
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget)
import PlutusTx.Evaluation (evaluateCekTrace)
import UntypedPlutusCore (
  Program(Program),
  termMapNames,
  unNameDeBruijn
 )
import qualified UntypedPlutusCore.Evaluation.Machine.Cek as UPLC
import UntypedPlutusCore.DeBruijn (deBruijnTerm)
import PlutusCore (defaultVersion, FreeVariableError)

-- Stolen from pluto, thanks Morgan

{- | Evaluate a script, returning the trace log and term result.

 This is same as `Plutus.V1.Ledger.Scripts.evaluateScript`, but returns the
 result as well.
-}
evaluateScript :: Script -> Either Scripts.ScriptError (ExBudget, [Text], Script)
evaluateScript s = do
  p <- case Scripts.mkTermToEvaluate s of
    Right p -> pure p
    Left e -> Left . Scripts.MalformedScript $ show e
  let (logOut, UPLC.TallyingSt _ budget, result) = evaluateCekTrace p
  named <- case result of
    Right term -> pure term
    Left errWithCause@(UPLC.ErrorWithCause err cause) ->
      Left $ case err of
        UPLC.InternalEvaluationError internalEvalError ->
          Scripts.EvaluationException (show errWithCause) (show internalEvalError)
        UPLC.UserEvaluationError evalError ->
          -- We use `show` here because plutus doesn't expose mkError
          Scripts.EvaluationError logOut (show (evalError, cause))
  term' <- runExceptT @FreeVariableError (deBruijnTerm named) --  <$>
  let Right term = term'
  let s' = Script $ Program () (defaultVersion ()) $ termMapNames unNameDeBruijn term
  pure (budget, logOut, s')
