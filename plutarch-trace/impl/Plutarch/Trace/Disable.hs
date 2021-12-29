module Plutarch.Trace.Disable (ptrace', ptrace, ptraceIfTrue, ptraceIfFalse) where

import Plutarch.Bool (PBool)
import Plutarch.Prelude
import Plutarch.String (PString)

pf :: Term s (b :--> a :--> a)
pf = phoistAcyclic $ plam $ \_ y -> y

ptrace' :: Term s (PString :--> a :--> a)
ptrace' = pf

ptrace :: Term s PString -> Term s a -> Term s a
ptrace _ a = a

ptraceIfTrue :: Term s (PString :--> PBool :--> PBool)
ptraceIfTrue = pf

ptraceIfFalse :: Term s (PString :--> PBool :--> PBool)
ptraceIfFalse = pf
