module Plutarch.Internal.Trace (
  ptrace,
  ptrace',
) where

import Plutarch.Internal (
  Term,
  pdelay,
  pforce,
  pgetConfig,
  phoistAcyclic,
  tracingMode,
  (#),
  pattern NoTracing,
  type (:-->),
 )
import {-# SOURCE #-} Plutarch.String (PString)

import Plutarch.Unsafe (punsafeBuiltin)
import PlutusCore qualified as PLC

ptrace' :: Term s (PString :--> a :--> a)
ptrace' = phoistAcyclic $ pforce $ punsafeBuiltin PLC.Trace

-- | Trace the given message before evaluating the argument.
ptrace :: Term s PString -> Term s a -> Term s a
ptrace s a = pgetConfig \c -> case tracingMode c of
  NoTracing -> a
  _ -> pforce $ ptrace' # s # pdelay a
