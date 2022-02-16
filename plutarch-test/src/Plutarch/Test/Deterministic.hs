{-# LANGUAGE ImpredicativeTypes #-}

module Plutarch.Test.Deterministic (compileD) where

import Control.Monad (void)
import qualified Data.Text as T
import Data.Void (Void)
import Plutarch (ClosedTerm, compile)
import qualified Plutus.V1.Ledger.Scripts as Scripts
import PlutusCore.Default
import Replace.Megaparsec (streamEdit)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import UntypedPlutusCore (Program (Program), Term (Apply, Builtin, Constant, Delay, Force, LamAbs))
import qualified UntypedPlutusCore as UPLC

{- Like `compile`, but the result is deterministic -}
compileD :: ClosedTerm a -> Scripts.Script
compileD = rewrite . compile

-- TODO: refactor
rewrite :: Scripts.Script -> Scripts.Script
rewrite (Scripts.Script (Program ann ver term)) =
  Scripts.Script (Program ann ver (go term))
  where
    go :: UPLC.Term name DefaultUni DefaultFun () -> UPLC.Term name DefaultUni DefaultFun ()
    go = \case
      (Apply ann b@(Force _ (Builtin _ Trace)) (Constant ann1 (Some (ValueOf DefaultUniString s)))) ->
        (Apply ann b (Constant ann1 (someValueOf DefaultUniString $ T.pack . rewriteFailMsgToBeDeterministic . T.unpack $ s)))
      LamAbs ann name t ->
        LamAbs ann name (go t)
      Apply ann t1 t2 ->
        Apply ann (go t1) (go t2)
      Force ann t ->
        Force ann (go t)
      Delay ann t ->
        Delay ann (go t)
      x -> x

rewriteFailMsgToBeDeterministic :: String -> String
rewriteFailMsgToBeDeterministic =
  streamEdit ghcPatternMatchMsg id
  where
    ghcPatternMatchMsg :: M.Parsec Void String String
    ghcPatternMatchMsg = do
      -- What's being replaced.
      s <- M.string "Pattern match failure"
      void $ M.takeRest
      -- The replacement
      pure $ s <> "..."
