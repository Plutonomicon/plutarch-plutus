module Plutarch.EnableTrace (ptrace', ptrace, ptraceIfTrue, ptraceIfFalse) where

import Plutarch
import Plutarch.Bool (PBool, pif)
import Plutarch.String (PString)
import qualified PlutusCore as PLC

-- | A strict version of 'ptrace'.
ptrace' :: Term s (PString :--> a :--> a)
ptrace' = phoistAcyclic $ pforce $ punsafeBuiltin PLC.Trace

-- | Trace a message, then evaluate and return given argument.
ptrace :: Term s PString -> Term s a -> Term s a
ptrace s a = pforce $ ptrace' # s # pdelay a

-- | Trace a message if given argument evaluates to true.
ptraceIfTrue :: Term s (PString :--> PBool :--> PBool)
ptraceIfTrue = phoistAcyclic $ plam $ \s a -> pif a (ptrace' # s # a) a

-- | Trace a message if given argument evaluates to false.
ptraceIfFalse :: Term s (PString :--> PBool :--> PBool)
ptraceIfFalse = phoistAcyclic $ plam $ \s a -> pif a a $ ptrace' # s # a
