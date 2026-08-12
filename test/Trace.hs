module Trace (goldens) where

import Data.Text (Text)
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (
  Term,
  TermEnv (TermEnv),
  TracingMode (DebugTracing, ErrorTracing, NoTracing),
  punsafeConstant,
 )
import Plutarch.Primitive.Numeric (PInteger)
import Plutarch.Primitive.Trace (ptraceDebug, ptraceError)
import Plutarch.Test.Golden (plutarchGoldenAllWith)
import PlutusCore qualified as PLC
import Test.Tasty (TestTree, testGroup)

goldens :: TestTree
goldens =
  testGroup
    "Trace"
    [ plutarchGoldenAllWith noTraceEnv "10" "Trace Case 1 no trace" case1
    , plutarchGoldenAllWith noTraceEnv "traceDebug \"foo\" 10" "Trace Case 2 no trace" case2
    , plutarchGoldenAllWith noTraceEnv "traceError \"foo\" 10" "Trace Case 3 no trace" case3
    , plutarchGoldenAllWith errorEnv "10" "Trace Case 1 error trace" case1
    , plutarchGoldenAllWith errorEnv "traceDebug \"foo\" 10" "Trace Case 2 error trace" case2
    , plutarchGoldenAllWith errorEnv "traceError \"foo\" 10" "Trace Case 3 error trace" case3
    , plutarchGoldenAllWith debugEnv "10" "Trace Case 1 debug trace" case1
    , plutarchGoldenAllWith debugEnv "traceDebug \"foo\" 10" "Trace Case 2 debug trace" case2
    , plutarchGoldenAllWith debugEnv "traceError \"foo\" 10" "Trace Case 3 debug trace" case3
    ]

-- Cases

case1 :: forall (s :: S). Term s PInteger
case1 = punsafeConstant . PLC.someValue @Integer $ 10

case2 :: forall (s :: S). Term s PInteger
case2 = ptraceDebug (punsafeConstant . PLC.someValue @Text $ "foo") case1

case3 :: forall (s :: S). Term s PInteger
case3 = ptraceError (punsafeConstant . PLC.someValue @Text $ "foo") case1

-- Helpers

noTraceEnv :: TermEnv
noTraceEnv = TermEnv NoTracing

errorEnv :: TermEnv
errorEnv = TermEnv ErrorTracing

debugEnv :: TermEnv
debugEnv = TermEnv DebugTracing
