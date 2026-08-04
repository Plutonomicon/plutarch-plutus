{-# LANGUAGE NoPartialTypeSignatures #-}

module Plutarch.Helpers.Evaluate (
  evalUPLC,
  maxBudget,
) where

import Data.Text (Text)
import Plutarch.Backend.UPLC (UPLCTerm (UPLCTerm))
import PlutusCore qualified as PLC
import PlutusCore.Evaluation.Machine.ExBudget (
  ExBudget (ExBudget),
  ExRestrictingBudget (ExRestrictingBudget),
 )
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults (
  defaultCekParametersForTesting,
 )
import PlutusCore.Evaluation.Machine.ExMemory (
  ExCPU (ExCPU),
  ExMemory (ExMemory),
 )
import UntypedPlutusCore.Evaluation.Machine.Cek qualified as Cek

evalUPLC ::
  ExBudget ->
  UPLCTerm ->
  ( Either
      (Cek.CekEvaluationException PLC.Name PLC.DefaultUni PLC.DefaultFun)
      (Either (PLC.Some (PLC.ValueOf PLC.DefaultUni)) UPLCTerm)
  , ExBudget
  , [Text]
  )
evalUPLC budget (UPLCTerm t) =
  let params = defaultCekParametersForTesting
      budgetMode = Cek.restricting (ExRestrictingBudget budget)
      emitter = Cek.logEmitter
   in case Cek.runCek params budgetMode emitter t of
        Cek.CekReport res (Cek.RestrictingSt (ExRestrictingBudget cost)) logs -> case res of
          Cek.CekFailure err -> (Left err, cost, logs)
          Cek.CekSuccessConstant c -> (Right . Left $ c, cost, logs)
          Cek.CekSuccessNonConstant t -> (Right . Right . UPLCTerm $ t, cost, logs)

maxBudget :: ExBudget
maxBudget = ExBudget (ExCPU maxBound) (ExMemory maxBound)
