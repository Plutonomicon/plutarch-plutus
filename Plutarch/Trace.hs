{-# LANGUAGE CPP #-}

module Plutarch.Trace (ptrace', ptrace, ptraceIfTrue, ptraceIfFalse) where

import Plutarch (
  Term,
  phoistAcyclic,
  plam,
#ifdef Development
  pforce,
  pdelay,
  punsafeBuiltin,
  (#),
#endif
  type (:-->)
 )
#ifdef Development
import Plutarch.Bool (PBool, pif)
#else
import Plutarch.Bool (PBool)
#endif
import Plutarch.String (PString)
#ifdef Development
import qualified PlutusCore as PLC
#endif

-- | A strict version of 'ptrace'.
ptrace' :: Term s (PString :--> a :--> a)

#ifdef Development
ptrace' = phoistAcyclic $ pforce $ punsafeBuiltin PLC.Trace
#else
ptrace' = phoistAcyclic $ plam f
  where
    f :: Term s PString -> Term s a -> Term s a
    f _ a = a
#endif

-- | Trace a message, then evaluate and return given argument.
ptrace :: Term s PString -> Term s a -> Term s a

#ifdef Development
ptrace s a = pforce $ ptrace' # s # pdelay a
#else
ptrace _ a = a
#endif

-- | Trace a message if given argument evaluates to true.
ptraceIfTrue :: Term s (PString :--> PBool :--> PBool)

#ifdef Development
ptraceIfTrue = phoistAcyclic $ plam $ \s a -> pif a (ptrace' # s # a) a
#else
ptraceIfTrue = phoistAcyclic $ plam f
  where
    f :: Term s PString -> Term s PBool -> Term s PBool
    f _ a = a
#endif

-- | Trace a message if given argument evaluates to false.
ptraceIfFalse :: Term s (PString :--> PBool :--> PBool)

#ifdef Development
ptraceIfFalse = phoistAcyclic $ plam $ \s a -> pif a a $ ptrace' # s # a
#else
ptraceIfFalse = phoistAcyclic $ plam f
  where
    f :: Term s PString -> Term s PBool -> Term s PBool
    f _ a = a
#endif
