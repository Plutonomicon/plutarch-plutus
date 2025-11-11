module Plutarch.Internal.Trace (
  ptraceInfo,
  ptraceDebug,
  ptrace',
) where

import Data.Kind (Type)
import Plutarch.Builtin.String (PString, ptrace', ptraceInfo)
import Plutarch.Internal.Term (Config (Tracing), LogLevel (LogDebug), S, Term, pdelay, pforce, pgetConfig, (#))

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
