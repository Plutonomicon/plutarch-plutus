{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | TermCont-related adapters for Plutarch functions.
module Plutarch.Extra.Monad (
  tlet,
  tmatch,
) where

import Plutarch.Prelude

-- | Like `plet` but works in a `TermCont` monad
tlet :: Term s a -> TermCont s (Term s a)
tlet = tcont . plet

-- | Like `pmatch` but works in a `TermCont` monad
tmatch :: PMatch a => Term s a -> TermCont s (a s)
tmatch = tcont . pmatch
