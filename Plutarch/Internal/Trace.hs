module Plutarch.Internal.Trace (
  ptraceInfo,
  ptraceDebug,
  ptrace',
  ptrace,
) where

import Data.Kind (Type)
import Plutarch.Internal (
  Config (NoTracing, Tracing),
  LogLevel (LogDebug),
  S,
  Term,
  pdelay,
  pforce,
  pgetConfig,
  phoistAcyclic,
  (#),
  type (:-->),
 )
import {-# SOURCE #-} Plutarch.String (PString)
import Plutarch.Unsafe (punsafeBuiltin)
import PlutusCore qualified as PLC

ptrace' :: Term s (PString :--> a :--> a)
ptrace' = phoistAcyclic $ pforce $ punsafeBuiltin PLC.Trace

{- | Trace the given message at the info level before evaluating the given
argument.

@since 1.6.0
-}
ptraceInfo ::
  forall (a :: S -> Type) (s :: S).
  Term s PString ->
  Term s a ->
  Term s a
ptraceInfo msg x = pgetConfig $ \case
  NoTracing -> x
  Tracing _ _ -> pforce $ ptrace' # msg # pdelay x

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

{-
-- | Trace the given message before evaluating the argument.
ptrace :: Term s PString -> Term s a -> Term s a
ptrace s a = pgetConfig \c -> case tracingMode c of
  NoTracing -> a
  _ -> pforce $ ptrace' # s # pdelay a -}
