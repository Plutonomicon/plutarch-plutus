{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | TermCont-related adapters for Plutarch functions.
module Plutarch.Extra.Monad (
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
