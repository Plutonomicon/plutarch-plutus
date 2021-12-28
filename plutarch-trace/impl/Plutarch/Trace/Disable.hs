module Plutarch.Trace.Disable (ptrace', ptrace, ptraceIfTrue, ptraceIfFalse) where

import Plutarch.Prelude
import Plutarch.Bool (PBool)
import Plutarch.String (PString)

pf :: Term s (b :--> a :--> a)
pf = phoistAcyclic $ plam $ \_ y -> y

-- | A strict version of 'ptrace'.
ptrace' :: Term s (PString :--> a :--> a)
ptrace' = pf

-- | Trace a message, then evaluate and return given argument.
ptrace :: Term s PString -> Term s a -> Term s a
ptrace _ a = a

-- | Trace a message if given argument evaluates to true.
ptraceIfTrue :: Term s (PString :--> PBool :--> PBool)
ptraceIfTrue = pf

-- | Trace a message if given argument evaluates to false.
ptraceIfFalse :: Term s (PString :--> PBool :--> PBool)
ptraceIfFalse = pf
