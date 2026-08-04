{-# LANGUAGE NoPartialTypeSignatures #-}

module Plutarch.Backend.Evaluate (
  EvalError (..),
  peval,
) where

import Control.Monad.Except (throwError)
import Data.Kind (Type)
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (Term, TermError)
import Plutarch.Backend.UPLC (UPLCTerm)
import Plutarch.Helpers.Compile (termToUPLC)
import Plutarch.Helpers.Evaluate (evalUPLC, maxBudget)
import PlutusCore qualified as PLC
import UntypedPlutusCore.Evaluation.Machine.Cek qualified as Cek

-- | @since wip
data EvalError
  = ATermError TermError
  | AnExecError (Cek.CekEvaluationException PLC.Name PLC.DefaultUni PLC.DefaultFun)
  deriving stock
    ( -- | @since wip
      Show
    )

-- | @since wip
peval ::
  forall (a :: S -> Type).
  (forall (s :: S). Term s a) ->
  Either EvalError (Either (PLC.Some (PLC.ValueOf PLC.DefaultUni)) UPLCTerm)
peval t = case termToUPLC t of
  Left err -> throwError . ATermError $ err
  Right t -> case evalUPLC maxBudget t of
    (res, _, _) -> case res of
      Left err -> throwError . AnExecError $ err
      Right res' -> pure res'
