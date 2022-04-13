{- | TermCont-related adapters for Plutarch functions.

  TODO: More functions (pletFieldsC, ptraceC, ...) need to be added here.
-}
module Plutarch.Extra.TermCont (
  pletC,
  pmatchC,
) where

import Plutarch.Prelude

-- | Like `plet` but works in a `TermCont` monad
pletC :: Term s a -> TermCont s (Term s a)
pletC = tcont . plet

-- | Like `pmatch` but works in a `TermCont` monad
pmatchC :: PMatch a => Term s a -> TermCont s (a s)
pmatchC = tcont . pmatch
