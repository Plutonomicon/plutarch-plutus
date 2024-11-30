module Plutarch.Internal.Trace (
  ptraceInfo,
  ptraceDebug,
  ptrace',
  ptrace,
) where

import Plutarch.Builtin

import Data.Kind (Type)
import Plutarch.Internal.Term (Config (Tracing), LogLevel (LogDebug), S, Term, pdelay, pforce, pgetConfig, (#))

{- | Backward compatibility synonym for 'ptraceInfo'.

@since 1.6.0
-}
ptrace ::
  forall (a :: S -> Type) (s :: S).
  Term s PString ->
  Term s a ->
  Term s a
ptrace = ptraceInfo
{-# DEPRECATED ptrace "Use ptraceInfo" #-}

{- | Trace the given message at the debug level before evaluating the given
argument.

@since 1.6.0
-}
ptraceDebug ::
  forall (a :: S -> Type) (s :: S).
  Term s PString ->
  Term s a ->
  Term s a
ptraceDebug msg x = pgetConfig $ \case
  Tracing LogDebug _ -> pforce $ ptrace' # msg # pdelay x
  _ -> x
