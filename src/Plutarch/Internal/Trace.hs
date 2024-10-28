module Plutarch.Internal.Trace (
  ptraceInfo,
  ptraceDebug,
  ptrace',
) where

import Data.Kind (Type)
import Plutarch.Internal.Term (
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
