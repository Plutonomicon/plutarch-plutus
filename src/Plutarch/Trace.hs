module Plutarch.Trace (
  -- * Info level
  ptraceInfo,
  ptraceInfoShowId,
  ptraceInfoError,
  ptraceInfoIfTrue,
  ptraceInfoIfFalse,

  -- * Debug level
  ptraceDebug,
  ptraceDebugShowId,
  ptraceDebugIfTrue,
  ptraceDebugIfFalse,
  ptraceDebugError,
) where

import Data.Kind (Type)
import Plutarch.Internal.Builtin (PBool, PString, pbuiltinTrace, pif)
import Plutarch.Internal.Term (
  Config (NoTracing, Tracing),
  LogLevel (LogDebug, LogInfo),
  S,
  Term,
  perror,
  pgetConfig,
  plet,
  (#),
 )
import Plutarch.Internal.Trace (ptraceDebug, ptraceInfo)
import Plutarch.Show (PShow, pshow)

{- | Like Haskell's @traceShowId@ but for Plutarch, at the info level.

@since 1.6.0
-}
ptraceInfoShowId ::
  forall (a :: S -> Type) (s :: S).
  PShow a =>
  Term s a ->
  Term s a
ptraceInfoShowId x = pgetConfig $ \case
  NoTracing -> x
  Tracing _ _ -> ptraceInfo (pshow x) x

{- | Like Haskell's @traceShowId@ but for Plutarch, at the debug level.

@since 1.6.0
-}
ptraceDebugShowId ::
  forall (a :: S -> Type) (s :: S).
  PShow a =>
  Term s a ->
  Term s a
ptraceDebugShowId x = pgetConfig $ \case
  NoTracing -> x
  _ -> ptraceDebug (pshow x) x

{- | Trace the given message at the info level, then terminate with 'perror'.

@since 1.6.0
-}
ptraceInfoError ::
  forall (a :: S -> Type) (s :: S).
  Term s PString ->
  Term s a
ptraceInfoError = flip ptraceInfo perror

{- | Trace the given message at the debug level, then terminate with 'perror'.

@since 1.6.0
-}
ptraceDebugError ::
  forall (a :: S -> Type) (s :: S).
  Term s PString ->
  Term s a
ptraceDebugError = flip ptraceDebug perror

{- | Trace the given message at the info level if the argument is true.

@since 1.6.0
-}
ptraceInfoIfTrue ::
  forall (s :: S).
  Term s PString ->
  Term s PBool ->
  Term s PBool
ptraceInfoIfTrue msg x = pgetConfig $ \case
  NoTracing -> x
  _ -> plet x $ \x' -> pif x' (pbuiltinTrace # msg # x') x'

{- | Trace the given message at the debug level if the argument is true.

@since 1.6.0
-}
ptraceDebugIfTrue ::
  forall (s :: S).
  Term s PString ->
  Term s PBool ->
  Term s PBool
ptraceDebugIfTrue msg x = pgetConfig $ \case
  NoTracing -> x
  Tracing ll _ -> case ll of
    LogInfo -> x
    LogDebug -> plet x $ \x' -> pif x' (pbuiltinTrace # msg # x') x'

{- | Trace the given message at the info level if the argument is false.

@since 1.6.0
-}
ptraceInfoIfFalse ::
  forall (s :: S).
  Term s PString ->
  Term s PBool ->
  Term s PBool
ptraceInfoIfFalse msg x = pgetConfig $ \case
  NoTracing -> x
  _ -> plet x $ \x' -> pif x' x' (pbuiltinTrace # msg # x')

{- | Trace the given message at the debug level if the argument is false.

@since 1.6.0
-}
ptraceDebugIfFalse ::
  forall (s :: S).
  Term s PString ->
  Term s PBool ->
  Term s PBool
ptraceDebugIfFalse msg x = pgetConfig $ \case
  NoTracing -> x
  Tracing ll _ -> case ll of
    LogInfo -> x
    LogDebug -> plet x $ \x' -> pif x' x' (pbuiltinTrace # msg # x')
