{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Plutarch.Evaluate (evaluateBudgetedScript, evaluateScript, evalScript, evalScript', EvalError) where

import Data.Text (Text)
import Plutus.V1.Ledger.Scripts (Script (Script))
import qualified Plutus.V1.Ledger.Scripts as Scripts
import qualified PlutusCore as PLC
import PlutusCore.Evaluation.Machine.ExBudget (
  ExBudget (ExBudget),
  ExRestrictingBudget (ExRestrictingBudget),
  minusExBudget,
 )
import PlutusCore.Evaluation.Machine.ExMemory (ExCPU (ExCPU), ExMemory (ExMemory))
import UntypedPlutusCore (
  Program (Program),
  Term,
 )
import qualified UntypedPlutusCore as UPLC
import qualified UntypedPlutusCore.Evaluation.Machine.Cek as Cek

{-# DEPRECATED evaluateScript "use evalScript" #-}
evaluateScript :: Script -> Either Scripts.ScriptError (ExBudget, [Text], Script)
evaluateScript = evaluateBudgetedScript $ ExBudget (ExCPU maxInt) (ExMemory maxInt)
  where
    maxInt = fromIntegral (maxBound :: Int)

{-# DEPRECATED evaluateBudgetedScript "use evalScript'" #-}
evaluateBudgetedScript :: ExBudget -> Script -> Either Scripts.ScriptError (ExBudget, [Text], Script)
evaluateBudgetedScript budget script = case evalScript' budget script of
  (Right res, remaining, logs) -> Right (remaining, logs, res)
  (Left _, _, logs) -> Left $ Scripts.EvaluationError logs "evaluation failed"

type EvalError = (Cek.CekEvaluationException PLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun)

-- | Evaluate a script with a big budget, returning the trace log and term result.
evalScript :: Script -> (Either EvalError Script, ExBudget, [Text])
evalScript script = evalScript' budget script
  where
    -- from https://github.com/input-output-hk/cardano-node/blob/master/configuration/cardano/mainnet-alonzo-genesis.json#L17
    budget = ExBudget (ExCPU 10000000000) (ExMemory 10000000)

-- | Evaluate a script with a specific budget, returning the trace log and term result.
evalScript' :: ExBudget -> Script -> (Either (Cek.CekEvaluationException PLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun) Script, ExBudget, [Text])
evalScript' budget (Script (Program _ _ t)) = case evalTerm budget (UPLC.termMapNames UPLC.fakeNameDeBruijn $ t) of
  (res, remaining, logs) -> (Script . Program () (PLC.defaultVersion ()) . UPLC.termMapNames UPLC.unNameDeBruijn <$> res, remaining, logs)

evalTerm ::
  ExBudget ->
  Term PLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun () ->
  ( Either
      EvalError
      (Term PLC.NamedDeBruijn PLC.DefaultUni PLC.DefaultFun ())
  , ExBudget
  , [Text]
  )
evalTerm budget t =
  case Cek.runCekDeBruijn PLC.defaultCekParameters (Cek.restricting (ExRestrictingBudget budget)) Cek.logEmitter t of
    (errOrRes, Cek.RestrictingSt (ExRestrictingBudget final), logs) -> (errOrRes, budget `minusExBudget` final, logs)
