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
import Plutarch.Test.Golden (plutarchGoldenEvalWith, plutarchGoldenWith)
import PlutusCore qualified as PLC
import Test.Tasty (TestTree, testGroup)

goldens :: TestTree
goldens =
  testGroup
    "Trace"
    [ plutarchGoldenWith noTraceEnv "10" "Trace Case 1 no trace" case1
    , plutarchGoldenEvalWith noTraceEnv "10" "Trace Case 1 no trace" case1
    , plutarchGoldenWith noTraceEnv "traceDebug \"foo\" 10" "Trace Case 2 no trace" case2
    , plutarchGoldenEvalWith noTraceEnv "traceDebug \"foo\" 10" "Trace Case 2 no trace" case2
    , plutarchGoldenWith noTraceEnv "traceError \"foo\" 10" "Trace Case 3 no trace" case3
    , plutarchGoldenEvalWith noTraceEnv "traceError \"foo\" 10" "Trace Case 3 no trace" case3
    , plutarchGoldenWith errorEnv "10" "Trace Case 1 error trace" case1
    , plutarchGoldenEvalWith errorEnv "10" "Trace Case 1 error trace" case1
    , plutarchGoldenWith errorEnv "traceDebug \"foo\" 10" "Trace Case 2 error trace" case2
    , plutarchGoldenEvalWith errorEnv "traceDebug \"foo\" 10" "Trace Case 2 error trace" case2
    , plutarchGoldenWith errorEnv "traceError \"foo\" 10" "Trace Case 3 error trace" case3
    , plutarchGoldenEvalWith errorEnv "traceError \"foo\" 10" "Trace Case 3 error trace" case3
    , plutarchGoldenWith debugEnv "10" "Trace Case 1 debug trace" case1
    , plutarchGoldenEvalWith debugEnv "10" "Trace Case 1 debug trace" case1
    , plutarchGoldenWith debugEnv "traceDebug \"foo\" 10" "Trace Case 2 debug trace" case2
    , plutarchGoldenEvalWith debugEnv "traceDebug \"foo\" 10" "Trace Case 2 debug trace" case2
    , plutarchGoldenWith debugEnv "traceError \"foo\" 10" "Trace Case 3 debug trace" case3
    , plutarchGoldenEvalWith debugEnv "traceError \"foo\" 10" "Trace Case 3 debug trace" case3
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
