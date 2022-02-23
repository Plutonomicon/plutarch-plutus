module Plutarch.TraceSpec (spec) where

import Test.Syd

import Plutarch.Prelude
import Plutarch.Test

spec :: Spec
spec = do
  describe "trace" . plutarchDevFlagDescribe . pgoldenSpec $ do
    "ptrace" @\ do
      "one" @| ptrace "foo" (pcon PUnit) @-> \p ->
        ptraces p ["foo"]
      "two" @| ptrace "foo" (ptrace "bar" (pcon PUnit)) @-> \p ->
        ptraces p ["foo", "bar"]
    "ptraceIfTrue" @\ do
      "true" @| ptraceIfTrue "foo" (pcon PTrue) @-> \p ->
        p `ptraces` ["foo"]
      "false" @| ptraceIfTrue "foo" (pcon PFalse) @-> \p ->
        p `ptraces` []
    "ptraceIfFalse" @\ do
      "true" @| ptraceIfFalse "foo" (pcon PTrue) @-> \p ->
        p `ptraces` []
      "false" @| ptraceIfFalse "foo" (pcon PFalse) @-> \p ->
        p `ptraces` ["foo"]
    "chained" @\ do
      "false.true.false"
        @| ptraceIfFalse "foo" (ptraceIfTrue "bar" $ pcon PFalse)
        @-> \p -> p `ptraces` ["foo"]
      "ptrace.true.false"
        @| ptrace "foo" (ptraceIfTrue "bar" $ pcon PFalse)
        @-> \p -> p `ptraces` ["foo"]
      "ptrace.true.true"
        @| ptrace "foo" (ptraceIfTrue "bar" $ pcon PTrue)
        @-> \p -> p `ptraces` ["foo", "bar"]
