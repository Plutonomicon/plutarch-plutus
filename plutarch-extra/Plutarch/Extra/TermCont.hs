{- | TermCont-related adapters for Plutarch functions.

  TODO: More functions (pletFieldsC, ptraceC, ...) need to be added here.
-}
module Plutarch.Extra.TermCont (
  pletC,
  pmatchC,
  ptryFromC,
  ptryFromC',
) where

import Plutarch.Prelude
import Plutarch.Reducible (Reducible (Reduce))
import Plutarch.TryFrom (PTryFrom (PTryFromExcess), ptryFrom)

-- | Like `plet` but works in a `TermCont` monad
pletC :: Term s a -> TermCont s (Term s a)
pletC = tcont . plet

-- | Like `pmatch` but works in a `TermCont` monad
pmatchC :: PMatch a => Term s a -> TermCont s (a s)
pmatchC = tcont . pmatch

-- | Like `ptryFrom` but works in a `TermCont` monad
ptryFromC :: forall b a s. PTryFrom a b => Term s a -> TermCont s (Term s b, Reduce (PTryFromExcess a b s))
ptryFromC = tcont . ptryFrom

-- | Like `ptryFom'` but works in a `TermCont` monad
ptryFromC' :: forall a b s. PTryFrom a b => Term s a -> TermCont s (Term s b, Reduce (PTryFromExcess a b s))
ptryFromC' = tcont . ptryFrom
