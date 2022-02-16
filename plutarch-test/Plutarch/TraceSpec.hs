module Plutarch.TraceSpec (spec) where

import Test.Syd

import Plutarch.Prelude
import Plutarch.Test

spec :: Spec
spec = do
  describe "trace" . plutarchDevFlagDescribe $ do
    describe "ptrace" $ do
      let p1 = ptrace "foo" (pcon PUnit)
          p2 = ptrace "foo" (ptrace "bar" (pcon PUnit))
      goldens
        All
        [ ("one", p1)
        , ("two", p2)
        ]
      it "traces one" $ p1 `ptraces` ["foo"]
      it "traces two" $ p2 `ptraces` ["foo", "bar"]
    describe "ptraceIfTrue" $ do
      let p1 = ptraceIfTrue "foo" (pcon PTrue)
          p2 = ptraceIfTrue "foo" (pcon PFalse)
      goldens
        All
        [ ("true", p1)
        , ("false", p2)
        ]
      it "true" $ p1 `ptraces` ["foo"]
      it "false" $ p2 `ptraces` []
    describe "ptraceIfFalse" $ do
      let p1 = ptraceIfFalse "foo" (pcon PTrue)
          p2 = ptraceIfFalse "foo" (pcon PFalse)
      goldens
        All
        [ ("true", p1)
        , ("false", p2)
        ]
      it "true" $ p1 `ptraces` []
      it "false" $ p2 `ptraces` ["foo"]
    describe "more traces" $ do
      it "false.true.false" $
        ptraceIfFalse "foo" (ptraceIfTrue "bar" $ pcon PFalse)
          `ptraces` ["foo"]
      it "ptrace.true.false" $
        ptrace "foo" (ptraceIfTrue "bar" $ pcon PFalse)
          `ptraces` ["foo"]
      it "ptrace.true.true" $
        ptrace "foo" (ptraceIfTrue "bar" $ pcon PTrue)
          `ptraces` ["foo", "bar"]
