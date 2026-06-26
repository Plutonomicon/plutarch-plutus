{-# LANGUAGE NoPartialTypeSignatures #-}

module Plutarch.Backend.Evaluate (
  EvalError (..),
  peval,
) where

import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.RWS.CPS (runRWS)
import Data.Kind (Type)
import Data.Text (Text)
import Plutarch.Backend.ANF (analyzeDemand, fromHashedAST)
import Plutarch.Backend.AST (fromRawTerm)
import Plutarch.Backend.Compile (toUPLCTerm)
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (
  Term (Term),
  TermEnv (TermEnv),
  TermError,
 )
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
import UntypedPlutusCore qualified as UPLC
import UntypedPlutusCore.Evaluation.Machine.Cek qualified as Cek

-- | @since wip
data EvalError
  = ATermError TermError
  | AnExecError (Cek.CekEvaluationException PLC.Name PLC.DefaultUni PLC.DefaultFun)
  deriving stock
    ( -- | @since wip
      Show
    )

peval ::
  forall (a :: S -> Type).
  (forall (s :: S). Term s a) ->
  Either EvalError (Either (PLC.Some (PLC.ValueOf PLC.DefaultUni)) UPLCTerm)
peval (Term t) = case runRWS (runExceptT t) TermEnv 0 of
  (res, _, _) -> case res of
    Left err -> throwError . ATermError $ err
    -- We know that we have a closed term, so we can ignore the varmap.
    Right (_, rt) -> do
      let ast = fromRawTerm rt
      let anf = fromHashedAST ast
      let analyzedANF = analyzeDemand anf
      let (UPLCTerm compiled) = toUPLCTerm analyzedANF
      let budget = ExBudget (ExCPU maxBound) (ExMemory maxBound)
      case evalTerm budget compiled of
        (res, _, _) -> case res of
          Left err -> throwError . AnExecError $ err
          Right res' -> pure res'

-- Helpers

evalTerm ::
  ExBudget ->
  UPLC.Term PLC.Name PLC.DefaultUni PLC.DefaultFun () ->
  ( Either
      (Cek.CekEvaluationException PLC.Name PLC.DefaultUni PLC.DefaultFun)
      (Either (PLC.Some (PLC.ValueOf PLC.DefaultUni)) UPLCTerm)
  , ExBudget
  , [Text]
  )
evalTerm budget t = case Cek.runCek defaultCekParametersForTesting (Cek.restricting (ExRestrictingBudget budget)) Cek.logEmitter t of
  Cek.CekReport res (Cek.RestrictingSt (ExRestrictingBudget cost)) logs -> case res of
    Cek.CekFailure err -> (Left err, cost, logs)
    Cek.CekSuccessConstant c -> (Right . Left $ c, cost, logs)
    Cek.CekSuccessNonConstant t -> (Right . Right . UPLCTerm $ t, cost, logs)
