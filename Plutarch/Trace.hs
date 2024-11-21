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

  -- * Deprecated
  ptrace,
  ptraceShowId,
  ptraceError,
  ptraceIfTrue,
  ptraceIfFalse,
) where

import Data.Kind (Type)
import Plutarch.Bool (pif)
import Plutarch.Builtin.Bool (PBool)
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
import Plutarch.Internal.Trace (ptrace, ptrace', ptraceDebug, ptraceInfo)
import Plutarch.Show (PShow, pshow)
import Plutarch.String (PString)

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

{- | Synonym for 'ptraceInfoShowId'.

@since 1.6.0
-}
ptraceShowId ::
  forall (a :: S -> Type) (s :: S).
  PShow a =>
  Term s a ->
  Term s a
ptraceShowId = ptraceInfoShowId
{-# DEPRECATED ptraceShowId "Use ptraceInfoShowId" #-}

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

{- | Synonym for 'ptraceInfoError'.

@since 1.6.0
-}
ptraceError ::
  forall (a :: S -> Type) (s :: S).
  Term s PString ->
  Term s a
ptraceError = ptraceInfoError
{-# DEPRECATED ptraceError "Use ptraceInfoError" #-}

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
  _ -> plet x $ \x' -> pif x' (ptrace' # msg # x') x'

{- | Synonym for 'ptraceInfoIfTrue'.

@since 1.6.0
-}
ptraceIfTrue ::
  forall (s :: S).
  Term s PString ->
  Term s PBool ->
  Term s PBool
ptraceIfTrue = ptraceInfoIfTrue
{-# DEPRECATED ptraceIfTrue "Use ptraceInfoIfTrue" #-}

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
    LogDebug -> plet x $ \x' -> pif x' (ptrace' # msg # x') x'

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
  _ -> plet x $ \x' -> pif x' x' (ptrace' # msg # x')

{- | Synonym for 'ptraceInfoIfFalse'.

@since 1.6.0
-}
ptraceIfFalse ::
  forall (s :: S).
  Term s PString ->
  Term s PBool ->
  Term s PBool
ptraceIfFalse = ptraceInfoIfFalse
{-# DEPRECATED ptraceIfFalse "Use ptraceInfoIfFalse" #-}

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
    LogDebug -> plet x $ \x' -> pif x' x' (ptrace' # msg # x')
