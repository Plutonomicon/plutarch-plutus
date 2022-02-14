{-# LANGUAGE CPP #-}

module Plutarch.Trace (ptrace, ptraceIfTrue, ptraceIfFalse, ptraceError) where

-- CPP support isn't great in fourmolu.
{- ORMOLU_DISABLE -}

import Plutarch.Internal.Other (Term, perror)
#ifdef Development
import Plutarch.Internal.Other (type (:-->), (#), phoistAcyclic, plet, pforce, pdelay)
#endif
#ifdef Development
import Plutarch.Bool (PBool, pif)
#else
import Plutarch.Bool (PBool)
#endif
import Plutarch.String (PString)

#ifdef Development
import Plutarch.Unsafe (punsafeBuiltin)
import qualified PlutusCore as PLC
#endif

#ifdef Development
ptrace' :: Term s (PString :--> a :--> a)
ptrace' = phoistAcyclic $ pforce $ punsafeBuiltin PLC.Trace
#endif

-- | Trace the given message before evaluating the argument.
ptrace :: Term s PString -> Term s a -> Term s a
#ifdef Development
ptrace s a = pforce $ ptrace' # s # pdelay a
#else
ptrace _ a = a
#endif

-- | Trace the given message and terminate evaluation with a 'perror'.
ptraceError :: Term s PString -> Term s a
#ifdef Development
ptraceError s = pforce $ ptrace' # s # pdelay perror
#else
ptraceError _ = perror
#endif

-- | Trace the given message if the argument evaluates to true.
ptraceIfTrue :: Term s PString -> Term s PBool -> Term s PBool
#ifdef Development
ptraceIfTrue s a' = plet a' $ \a -> pif a (ptrace' # s # a) a
#else
ptraceIfTrue _ a = a
#endif

-- | Trace the given message if the argument evaluates to False.
ptraceIfFalse :: Term s PString -> Term s PBool -> Term s PBool
#ifdef Development
ptraceIfFalse s a' = plet a' $ \a -> pif a a (ptrace' # s # a)
#else
ptraceIfFalse _ a = a
#endif
