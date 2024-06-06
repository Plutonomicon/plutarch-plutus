module Main (main) where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Plutarch (
  Config (Tracing),
  LogLevel (LogDebug, LogInfo),
  TracingMode (DoTracing),
 )
import Plutarch.Evaluate (evalTerm)
import Plutarch.Prelude (
  PUnit,
  S,
  Term,
  pconstant,
  ptraceDebug,
  ptraceDebugError,
  ptraceDebugIfFalse,
  ptraceDebugIfTrue,
  ptraceDebugShowId,
  ptraceInfo,
  ptraceInfoError,
  ptraceInfoIfFalse,
  ptraceInfoIfTrue,
  ptraceInfoShowId,
 )
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain . testGroup "Plutarch" $
    [ testGroup
        "tracing"
        [ testCase "ptraceInfo traces at info level"
            . traces LogInfo (ptraceInfo "foo" (pconstant ()))
            $ ["foo"]
        , testCase "ptraceInfo traces at debug level"
            . traces LogDebug (ptraceInfo "foo" (pconstant ()))
            $ ["foo"]
        , testCase "ptraceInfoShowId traces at info level"
            . traces LogInfo (ptraceInfoShowId (pconstant ()))
            $ ["()"]
        , testCase "ptraceInfoShowId traces at debug level"
            . traces LogDebug (ptraceInfoShowId (pconstant ()))
            $ ["()"]
        , testCase "ptraceInfoError traces at info level"
            . traces LogInfo (ptraceInfoError @PUnit "foo")
            $ ["foo"]
        , testCase "ptraceInfoError traces at debug level"
            . traces LogDebug (ptraceInfoError @PUnit "foo")
            $ ["foo"]
        , testCase "ptraceInfoIfTrue traces at info level if true"
            . traces LogInfo (ptraceInfoIfTrue "foo" (pconstant True))
            $ ["foo"]
        , testCase "ptraceInfoIfTrue does not trace at info level if false"
            . traces LogInfo (ptraceInfoIfTrue "foo" (pconstant False))
            $ []
        , testCase "ptraceInfoIfTrue traces at debug level if true"
            . traces LogDebug (ptraceInfoIfTrue "foo" (pconstant True))
            $ ["foo"]
        , testCase "ptraceInfoIfTrue does not trace at debug level if false"
            . traces LogDebug (ptraceInfoIfTrue "foo" (pconstant False))
            $ []
        , testCase "ptraceInfoIfFalse does not trace at info level if true"
            . traces LogInfo (ptraceInfoIfFalse "foo" (pconstant True))
            $ []
        , testCase "ptraceInfoIfFalse traces at info level if false"
            . traces LogInfo (ptraceInfoIfFalse "foo" (pconstant False))
            $ ["foo"]
        , testCase "ptraceInfoIfFalse does not trace at debug level if true"
            . traces LogDebug (ptraceInfoIfFalse "foo" (pconstant True))
            $ []
        , testCase "ptraceInfoIfFalse traces at debug level if false"
            . traces LogDebug (ptraceInfoIfFalse "foo" (pconstant False))
            $ ["foo"]
        , testCase "ptraceDebug does not trace at info level"
            . traces LogInfo (ptraceDebug "foo" (pconstant ()))
            $ []
        , testCase "ptraceDebug traces at debug level"
            . traces LogDebug (ptraceDebug "foo" (pconstant ()))
            $ ["foo"]
        , testCase "ptraceDebugShowId does not trace at info level"
            . traces LogInfo (ptraceDebugShowId (pconstant ()))
            $ []
        , testCase "ptraceDebugShowId traces at debug level"
            . traces LogDebug (ptraceDebugShowId (pconstant ()))
            $ ["()"]
        , testCase "ptraceDebugError does not trace at info level"
            . traces LogInfo (ptraceDebugError @PUnit "foo")
            $ []
        , testCase "ptraceDebugError traces at debug level"
            . traces LogDebug (ptraceDebugError @PUnit "foo")
            $ ["foo"]
        , testCase "ptraceDebugIfTrue does not trace at info level if true"
            . traces LogInfo (ptraceDebugIfTrue "foo" (pconstant True))
            $ []
        , testCase "ptraceDebugIfTrue does not trace at info level if false"
            . traces LogInfo (ptraceDebugIfTrue "foo" (pconstant False))
            $ []
        , testCase "ptraceDebugIfTrue traces at debug level if true"
            . traces LogDebug (ptraceDebugIfTrue "foo" (pconstant True))
            $ ["foo"]
        , testCase "ptraceDebugIfTrue does not trace at debug level if false"
            . traces LogDebug (ptraceDebugIfTrue "foo" (pconstant False))
            $ []
        , testCase "ptraceDebugIfFalse does not trace at info level if true"
            . traces LogInfo (ptraceDebugIfFalse "foo" (pconstant True))
            $ []
        , testCase "ptraceDebugIfFalse does not trace at info level if false"
            . traces LogInfo (ptraceDebugIfFalse "foo" (pconstant False))
            $ []
        , testCase "ptraceDebugIfFalse does not trace at debug level if true"
            . traces LogDebug (ptraceDebugIfFalse "foo" (pconstant True))
            $ []
        , testCase "ptraceDebugIfFalse traces at debug level if false"
            . traces LogDebug (ptraceDebugIfFalse "foo" (pconstant False))
            $ ["foo"]
        ]
    ]

-- Helpers

traces ::
  forall (a :: S -> Type).
  LogLevel ->
  (forall (s :: S). Term s a) ->
  [Text] ->
  IO ()
traces ll comp expected = case evalTerm (Tracing ll DoTracing) comp of
  Left err -> assertFailure $ "Did not compile: " <> Text.unpack err
  Right (_, _, logs) -> assertEqual "" expected logs
