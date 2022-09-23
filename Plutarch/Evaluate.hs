{-# LANGUAGE ImpredicativeTypes #-}

module Plutarch.Evaluate (
  E.evalScript,
  E.evalScriptHuge,
  E.evalScript',
  E.EvalError,
  evalTerm,
) where

import qualified Plutarch.Internal.Evaluate as E

import Data.Text (Text)
import Plutarch.Internal (ClosedTerm, Config, RawTerm (RCompiled), Term (..), TermResult (TermResult), compile)
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget)
import PlutusLedgerApi.V1.Scripts (Script (Script))
import qualified UntypedPlutusCore as UPLC

-- | Compile and evaluate term.
evalTerm ::
  Config ->
  ClosedTerm a ->
  Either Text (Either E.EvalError (ClosedTerm a), ExBudget, [Text])
evalTerm config term =
  case compile config term of
    Right script ->
      let (s, b, t) = E.evalScriptHuge script
       in Right (fromScript <$> s, b, t)
    Left a -> Left a
  where
    fromScript :: Script -> ClosedTerm a
    fromScript (Script script) =
      Term $ const $ pure $ TermResult (RCompiled $ UPLC._progTerm $ script) []
