{-# LANGUAGE CPP #-}

module Plutarch.Trace (
  ptrace,
  ptraceShowId,
  ptraceIfTrue,
  ptraceIfFalse,
  ptraceError,
) where

-- CPP support isn't great in fourmolu.
{- ORMOLU_DISABLE -}

import Plutarch.Internal.Other (Term, perror, type (:-->), (#), phoistAcyclic, plet, pforce, pdelay,
  tracingMode,
  pattern NoTracing, pgetConfig)
import Plutarch.Bool (PBool, pif)
import Plutarch.String (PString)
import Plutarch.Show (PShow, pshow)

import Plutarch.Unsafe (punsafeBuiltin)
import qualified PlutusCore as PLC

ptrace' :: Term s (PString :--> a :--> a)
ptrace' = phoistAcyclic $ pforce $ punsafeBuiltin PLC.Trace

-- | Trace the given message before evaluating the argument.
ptrace :: Term s PString -> Term s a -> Term s a
ptrace s a = pgetConfig \c -> case tracingMode c of
  NoTracing -> a
  _ -> pforce $ ptrace' # s # pdelay a

-- | Like Haskell's `traceShowId` but for Plutarch
ptraceShowId :: PShow a => Term s a -> Term s a
ptraceShowId a = pgetConfig \c -> case tracingMode c of
  NoTracing -> a
  _ -> ptrace (pshow a) a

-- | Trace the given message and terminate evaluation with a 'perror'.
ptraceError :: Term s PString -> Term s a
ptraceError s = pgetConfig \c -> case tracingMode c of
  NoTracing -> perror
  _ -> pforce $ ptrace' # s # pdelay perror

-- | Trace the given message if the argument evaluates to true.
ptraceIfTrue :: Term s PString -> Term s PBool -> Term s PBool
ptraceIfTrue s a' = pgetConfig \c -> case tracingMode c of
  NoTracing -> a'
  _ -> plet a' $ \a -> pif a (ptrace' # s # a) a

-- | Trace the given message if the argument evaluates to False.
ptraceIfFalse :: Term s PString -> Term s PBool -> Term s PBool
ptraceIfFalse s a' = pgetConfig \c -> case tracingMode c of
  NoTracing -> a'
  _ -> plet a' $ \a -> pif a a (ptrace' # s # a)
