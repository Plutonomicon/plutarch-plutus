{-# LANGUAGE ImpredicativeTypes #-}

module Plutarch.Evaluate (
  E.evalScript,
  E.evalScriptHuge,
  E.evalScript',
  E.EvalError,
  evalTerm,
  applyArguments,
) where

import Plutarch.Internal.Evaluate qualified as E

import Control.Lens.Combinators (over)
import Data.Text (Text)
import Plutarch.Internal (
  ClosedTerm,
  Config,
  RawTerm (RCompiled),
  Term (..),
  TermResult (TermResult),
  compile,
 )
import Plutarch.Script (Script (Script))
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget)
import PlutusCore.MkPlc (mkConstant, mkIterAppNoAnn)
import PlutusLedgerApi.Common (Data)
import UntypedPlutusCore qualified as UPLC

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
      Term $ const $ pure $ TermResult (RCompiled $ UPLC._progTerm script) []

{- | Given a compiled 'Script' representing a function that takes arguments, and
a list of those 'Data'-encoded arguments, produce a new script with those
arguments applied.

@since 1.8.1
-}
applyArguments :: Script -> [Data] -> Script
applyArguments (Script p) args =
  let termArgs = mkConstant () <$> args
      applied t = mkIterAppNoAnn t termArgs
   in Script $ over UPLC.progTerm applied p
