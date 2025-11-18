module Plutarch.Internal.Case (punsafeCase) where

import Data.Kind (Type)
import Data.Semialign (unzipWith)
import Plutarch.Builtin.Opaque (POpaque)
import Plutarch.Internal.Term (
  RawTerm (RCase),
  S,
  Term (Term),
  TermResult (TermResult),
  asRawTerm,
 )

{- | Construct a @Case@ statement in UPLC. The first argument is what will be
matched on, while the second argument will be used as handlers.

= Important note

No attempt will (or even /can/) be made to check that the handlers have
correct types, or that the number of handlers is appropriate for the type
being handled. Crashes or misbehaviour can and will occur if you get this
wrong!

@since wip
-}
punsafeCase ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s a ->
  [Term s POpaque] ->
  Term s b
punsafeCase scrutinee handlers = Term $ \level -> do
  TermResult rawScrutinee depsScrutinee <- asRawTerm scrutinee level
  (rawHandlers, depsHandlers) <- fmap (unzipWith (\(TermResult x y) -> (x, y))) . traverse (`asRawTerm` level) $ handlers
  let allDeps = depsScrutinee <> mconcat depsHandlers
  pure . TermResult (RCase rawScrutinee rawHandlers) $ allDeps
