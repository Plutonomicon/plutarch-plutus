module Plutarch.Trace.Disable (ptrace', ptrace, ptraceIfTrue, ptraceIfFalse) where

import Plutarch.Bool (PBool)
import Plutarch.Prelude
import Plutarch.String (PString)

ptrace' :: Term s (PString :--> a :--> a)
ptrace' = phoistAcyclic $ plam $ \_ y -> y

ptrace :: Term s PString -> Term s a -> Term s a
ptrace _ a = a

ptraceIfTrue :: Term s PString -> Term s PBool -> Term s PBool
ptraceIfTrue _ a = a

ptraceIfFalse :: Term s PString -> Term s PBool -> Term s PBool
ptraceIfFalse _ a = a
