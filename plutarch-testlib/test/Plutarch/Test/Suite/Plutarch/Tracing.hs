module Plutarch.Test.Suite.Plutarch.Tracing (tests) where

import Plutarch (LogLevel (LogDebug, LogInfo))
import Plutarch.Prelude
import Plutarch.Test.Unit (testEvalEqualTraces)
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Tracing"
    [ testEvalEqualTraces
        "ptraceInfo traces at info level"
        (ptraceInfo "foo" (pconstant ()))
        LogInfo
        ["foo"]
    , testEvalEqualTraces
        "ptraceInfo traces at debug level"
        (ptraceInfo "foo" (pconstant ()))
        LogDebug
        ["foo"]
    , testEvalEqualTraces
        "ptraceInfoShowId traces at info level"
        (ptraceInfoShowId (pconstant ()))
        LogInfo
        ["()"]
    , testEvalEqualTraces
        "ptraceInfoShowId traces at debug level"
        (ptraceInfoShowId (pconstant ()))
        LogDebug
        ["()"]
    , testEvalEqualTraces
        "ptraceInfoError traces at info level"
        (ptraceInfoError @PUnit "foo")
        LogInfo
        ["foo"]
    , testEvalEqualTraces
        "ptraceInfoError traces at debug level"
        (ptraceInfoError @PUnit "foo")
        LogDebug
        ["foo"]
    , testEvalEqualTraces
        "ptraceInfoIfTrue traces at info level if true"
        (ptraceInfoIfTrue "foo" (pconstant True))
        LogInfo
        ["foo"]
    , testEvalEqualTraces
        "ptraceInfoIfTrue does not trace at info level if false"
        (ptraceInfoIfTrue "foo" (pconstant False))
        LogInfo
        []
    , testEvalEqualTraces
        "ptraceInfoIfTrue traces at debug level if true"
        (ptraceInfoIfTrue "foo" (pconstant True))
        LogDebug
        ["foo"]
    , testEvalEqualTraces
        "ptraceInfoIfTrue does not trace at debug level if false"
        (ptraceInfoIfTrue "foo" (pconstant False))
        LogDebug
        []
    , testEvalEqualTraces
        "ptraceInfoIfFalse does not trace at info level if true"
        (ptraceInfoIfFalse "foo" (pconstant True))
        LogInfo
        []
    , testEvalEqualTraces
        "ptraceInfoIfFalse traces at info level if false"
        (ptraceInfoIfFalse "foo" (pconstant False))
        LogInfo
        ["foo"]
    , testEvalEqualTraces
        "ptraceInfoIfFalse does not trace at debug level if true"
        (ptraceInfoIfFalse "foo" (pconstant True))
        LogDebug
        []
    , testEvalEqualTraces
        "ptraceInfoIfFalse traces at debug level if false"
        (ptraceInfoIfFalse "foo" (pconstant False))
        LogDebug
        ["foo"]
    , testEvalEqualTraces
        "ptraceDebug does not trace at info level"
        (ptraceDebug "foo" (pconstant ()))
        LogInfo
        []
    , testEvalEqualTraces
        "ptraceDebug traces at debug level"
        (ptraceDebug "foo" (pconstant ()))
        LogDebug
        ["foo"]
    , testEvalEqualTraces
        "ptraceDebugShowId does not trace at info level"
        (ptraceDebugShowId (pconstant ()))
        LogInfo
        []
    , testEvalEqualTraces
        "ptraceDebugShowId traces at debug level"
        (ptraceDebugShowId (pconstant ()))
        LogDebug
        ["()"]
    , testEvalEqualTraces
        "ptraceDebugError does not trace at info level"
        (ptraceDebugError @PUnit "foo")
        LogInfo
        []
    , testEvalEqualTraces
        "ptraceDebugError traces at debug level"
        (ptraceDebugError @PUnit "foo")
        LogDebug
        ["foo"]
    , testEvalEqualTraces
        "ptraceDebugIfTrue does not trace at info level if true"
        (ptraceDebugIfTrue "foo" (pconstant True))
        LogInfo
        []
    , testEvalEqualTraces
        "ptraceDebugIfTrue does not trace at info level if false"
        (ptraceDebugIfTrue "foo" (pconstant False))
        LogInfo
        []
    , testEvalEqualTraces
        "ptraceDebugIfTrue traces at debug level if true"
        (ptraceDebugIfTrue "foo" (pconstant True))
        LogDebug
        ["foo"]
    , testEvalEqualTraces
        "ptraceDebugIfTrue does not trace at debug level if false"
        (ptraceDebugIfTrue "foo" (pconstant False))
        LogDebug
        []
    , testEvalEqualTraces
        "ptraceDebugIfFalse does not trace at info level if true"
        (ptraceDebugIfFalse "foo" (pconstant True))
        LogInfo
        []
    , testEvalEqualTraces
        "ptraceDebugIfFalse does not trace at info level if false"
        (ptraceDebugIfFalse "foo" (pconstant False))
        LogInfo
        []
    , testEvalEqualTraces
        "ptraceDebugIfFalse does not trace at debug level if true"
        (ptraceDebugIfFalse "foo" (pconstant True))
        LogDebug
        []
    , testEvalEqualTraces
        "ptraceDebugIfFalse traces at debug level if false"
        (ptraceDebugIfFalse "foo" (pconstant False))
        LogDebug
        ["foo"]
    ]
