{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Plutarch.Evaluate (evaluateBudgetedScript, evaluateScript) where

import Control.Monad.Except (runExceptT)
import Data.Text (Text)
import Plutus.V1.Ledger.Scripts (Script (Script))
import qualified Plutus.V1.Ledger.Scripts as Scripts
import PlutusCore (FreeVariableError, defaultVersion)
import qualified PlutusCore as PLC
import PlutusCore.Evaluation.Machine.ExBudget (
  ExBudget (ExBudget),
  ExRestrictingBudget (ExRestrictingBudget),
  minusExBudget,
 )
import qualified PlutusCore.Evaluation.Machine.ExMemory as ExMemory
import UntypedPlutusCore (
  Program (Program),
  Term,
  termMapNames,
  unNameDeBruijn,
 )
import UntypedPlutusCore.DeBruijn (deBruijnTerm)
import qualified UntypedPlutusCore.Evaluation.Machine.Cek as UPLC

-- Stolen from pluto, thanks Morgan

{- | Evaluate a script, returning the trace log and term result.

 This is same as `Plutus.V1.Ledger.Scripts.evaluateScript`, but returns the
 result as well.
-}
evaluateScript :: Script -> Either Scripts.ScriptError (ExBudget, [Text], Script)
evaluateScript = evaluateBudgetedScript $ ExBudget (ExMemory.ExCPU maxInt) (ExMemory.ExMemory maxInt)
  where
    maxInt = fromIntegral (maxBound :: Int)

evaluateBudgetedScript :: ExBudget -> Script -> Either Scripts.ScriptError (ExBudget, [Text], Script)
evaluateBudgetedScript totalBudget s = do
  p <- case Scripts.mkTermToEvaluate s of
    Right p -> pure p
    Left e -> Left . Scripts.MalformedScript $ show e
  let (logOut, usedBudget, result) = evaluateCekBudgetedTrace totalBudget p
  named <- case result of
    Right term -> pure term
    Left errWithCause@(UPLC.ErrorWithCause err cause) ->
      Left $ case err of
        UPLC.InternalEvaluationError internalEvalError ->
          Scripts.EvaluationException (show errWithCause) (show internalEvalError)
        UPLC.UserEvaluationError evalError ->
          -- We use `show` here because plutus doesn't expose mkError
          Scripts.EvaluationError logOut (show (evalError, cause))
  term' <- runExceptT @FreeVariableError (deBruijnTerm named)
  let Right term = term'
  let s' = Script $ Program () (defaultVersion ()) $ termMapNames unNameDeBruijn term
  pure (usedBudget, logOut, s')

{- | Evaluate a program in the CEK machine against the given budget, with the
 usual text dynamic builtins and tracing, additionally returning the trace
 output.
-}
evaluateCekBudgetedTrace ::
  -- | The resource budget which must not be exceeded during evaluation
  ExBudget ->
  Program PLC.Name PLC.DefaultUni PLC.DefaultFun () ->
  ( [Text]
  , ExBudget
  , Either
      (UPLC.CekEvaluationException PLC.DefaultUni PLC.DefaultFun)
      (Term PLC.Name PLC.DefaultUni PLC.DefaultFun ())
  )
evaluateCekBudgetedTrace budget (Program _ _ t) =
  case UPLC.runCek PLC.defaultCekParameters (UPLC.restricting (ExRestrictingBudget budget)) UPLC.logEmitter t of
    (errOrRes, UPLC.RestrictingSt (ExRestrictingBudget final), logs) -> (logs, budget `minusExBudget` final, errOrRes)
