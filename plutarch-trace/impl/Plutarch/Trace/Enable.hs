module Plutarch.Trace.Enable (ptrace', ptrace, ptraceIfTrue, ptraceIfFalse) where

import Plutarch (punsafeBuiltin)
import Plutarch.Bool (PBool, pif)
import Plutarch.Prelude
import Plutarch.String (PString)
import qualified PlutusCore as PLC

ptrace' :: Term s (PString :--> a :--> a)
ptrace' = phoistAcyclic $ pforce $ punsafeBuiltin PLC.Trace

ptrace :: Term s PString -> Term s a -> Term s a
ptrace s a = pforce $ ptrace' # s # pdelay a

ptraceIfTrue :: Term s (PString :--> PBool :--> PBool)
ptraceIfTrue = phoistAcyclic $ plam $ \s a -> pif a (ptrace' # s # a) a

ptraceIfFalse :: Term s (PString :--> PBool :--> PBool)
ptraceIfFalse = phoistAcyclic $ plam $ \s a -> pif a a $ ptrace' # s # a
