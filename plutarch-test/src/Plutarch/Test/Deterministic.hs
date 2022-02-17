{-# LANGUAGE ImpredicativeTypes #-}

module Plutarch.Test.Deterministic (compileD, evaluateScriptAlways) where

import qualified Data.Text as T
import Plutarch (ClosedTerm, compile, perror)
import Plutarch.Evaluate (evaluateScript)
import qualified Plutus.V1.Ledger.Scripts as Scripts
import PlutusCore.Default (
  DefaultFun (Trace),
  DefaultUni (DefaultUniString),
  Some (Some),
  ValueOf (ValueOf),
  someValueOf,
 )
import UntypedPlutusCore (
  Program (Program),
  Term (Apply, Builtin, Constant, Delay, Force, LamAbs),
 )
import qualified UntypedPlutusCore as UPLC

{- Like `evaluateScript` but doesn't fail. Also returns `Script`.

  All evaluation failures are treated as equivalent to a `perror`. Plutus does
  not provide an accurate way to tell if the program evalutes to `Error` or not;
  see https://github.com/input-output-hk/plutus/issues/4270
-}
evaluateScriptAlways :: Scripts.Script -> Scripts.Script
evaluateScriptAlways script =
  case evaluateScript script of
    Left _ -> compile perror
    Right (_, _, x) -> x

{- Like `compile`, but the result is deterministic -}
compileD :: ClosedTerm a -> Scripts.Script
compileD p = rewriteTraces $ compile p

{- Rewrite the string used by `Trace` so that the script becomes deterministic. -}
rewriteTraces :: Scripts.Script -> Scripts.Script
rewriteTraces =
  walkScript $ \term -> do
    -- Replace the 's' in `trace s`.
    Apply () b@(Force _ (Builtin _ Trace)) (Constant () (Some (ValueOf DefaultUniString s))) <- pure term
    let s' = replaceGhcPatternMatch s
    pure $ Apply () b (Constant () (someValueOf DefaultUniString $ s'))
  where
    replaceGhcPatternMatch = \case
      (T.stripPrefix "Pattern match failure" -> Just _) ->
        "Pattern match failure..."
      x -> x

{- Walk the Plutus script, transforming matching terms -}
walkScript ::
  forall term.
  (term ~ UPLC.Term UPLC.DeBruijn DefaultUni DefaultFun ()) =>
  (term -> Maybe term) ->
  Scripts.Script ->
  Scripts.Script
walkScript f (Scripts.Script (Program ann ver term)) =
  Scripts.Script (Program ann ver (go term))
  where
    go :: term -> term
    go term =
      case f term of
        Just term' -> term'
        Nothing -> case term of
          LamAbs ann name t ->
            LamAbs ann name (go t)
          Apply ann t1 t2 ->
            Apply ann (go t1) (go t2)
          Force ann t ->
            Force ann (go t)
          Delay ann t ->
            Delay ann (go t)
          x -> x
