module Plutarch.TraceSpec (spec) where

import Plutarch.Prelude
import Plutarch.Test
import Test.Hspec

spec :: Spec
spec = do
  describe "trace" . pgoldenSpec $ do
    "ptrace" @\ do
      "one"
        @| ptrace "foo" (pcon PUnit)
        @-> \p ->
          ptraces p ["foo"]
      "two"
        @| ptrace "foo" (ptrace "bar" (pcon PUnit))
        @-> \p ->
          ptraces p ["foo", "bar"]
    "ptraceShowId" @\ do
      let x = pcon @(PEither PUnit PInteger) $ PRight 42
      "right-42"
        @| ptraceShowId x
        @-> \p ->
          p `ptraces` ["PRight 42"]
    "ptraceIfTrue" @\ do
      "true"
        @| ptraceIfTrue "foo" (pcon PTrue)
        @-> \p ->
          p `ptraces` ["foo"]
      "false"
        @| ptraceIfTrue "foo" (pcon PFalse)
        @-> \p ->
          p `ptraces` []
    "ptraceIfFalse" @\ do
      "true"
        @| ptraceIfFalse "foo" (pcon PTrue)
        @-> \p ->
          p `ptraces` []
      "false"
        @| ptraceIfFalse "foo" (pcon PFalse)
        @-> \p ->
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
