{-# LANGUAGE CPP #-}

module Examples.Tracing (traceTests) where

import Plutarch
import Plutarch.Bool (PBool (PFalse, PTrue))
import Plutarch.Trace (ptrace, ptraceIfFalse, ptraceIfTrue)
import Plutarch.Unit (PUnit (PUnit))
import Utils (traces)

traceTests :: IO ()
traceTests = do

-- CPP support isn't great in fourmolu.
{- ORMOLU_DISABLE -}
  ptrace "foo" (pcon PUnit) `traces`
#ifdef Development
    ["foo"]
#else
    []
#endif

  ptrace "foo" (ptrace "bar" $ pcon PUnit) `traces`
#ifdef Development
    ["foo", "bar"]
#else
    []
#endif

  ptraceIfTrue "foo" (pcon PTrue) `traces`
#ifdef Development
    ["foo"]
#else
    []
#endif

  ptraceIfTrue "foo" (pcon PFalse) `traces` []

  ptraceIfTrue "foo" (ptraceIfTrue "bar" $ pcon PTrue) `traces`
#ifdef Development
    ["bar", "foo"]
#else
    []
#endif

  ptraceIfTrue "foo" (ptraceIfTrue "bar" $ pcon PFalse) `traces` []
  ptraceIfFalse "foo" (ptraceIfTrue "bar" $ pcon PFalse) `traces`
#ifdef Development
    ["foo"]
#else
    []
#endif

  ptrace "foo" (ptraceIfTrue "bar" (pcon PTrue)) `traces`
#ifdef Development
    ["foo", "bar"]
#else
    []
#endif

  ptrace "foo" (ptraceIfTrue "bar" (pcon PFalse)) `traces`
#ifdef Development
    ["foo"]
#else
    []
#endif
{- ORMOLU_ENABLE -}
