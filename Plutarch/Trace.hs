{-# LANGUAGE CPP #-}

module Plutarch.Trace (ptrace, ptraceIfTrue, ptraceIfFalse) where

-- CPP support isn't great in fourmolu.
{- ORMOLU_DISABLE -}

#ifdef Development
import Plutarch (punsafeBuiltin)
#endif
#ifdef Development
import Plutarch.Bool (PBool, pif)
#else
import Plutarch.Bool (PBool)
#endif
import Plutarch.Prelude
import Plutarch.String (PString)

#ifdef Development
import qualified PlutusCore as PLC
#endif

#ifdef Development
ptrace' :: Term s (PString :--> a :--> a)
ptrace' = phoistAcyclic $ pforce $ punsafeBuiltin PLC.Trace
#endif

ptrace :: Term s PString -> Term s a -> Term s a
#ifdef Development
ptrace s a = pforce $ ptrace' # s # pdelay a
#else
ptrace _ a = a
#endif

ptraceIfTrue :: Term s PString -> Term s PBool -> Term s PBool
#ifdef Development
ptraceIfTrue s a' = plet a' $ \a -> pif a (ptrace' # s # a) a
#else
ptraceIfTrue _ a = a
#endif

ptraceIfFalse :: Term s PString -> Term s PBool -> Term s PBool

#ifdef Development
ptraceIfFalse s a' = plet a' $ \a -> pif a a (ptrace' # s # a)
#else
ptraceIfFalse _ a = a
#endif
