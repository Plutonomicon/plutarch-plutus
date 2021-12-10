module Plutarch.Opaque (POpaque) where

import Plutarch.Prelude
import Plutarch (PlutusType(PInner, pCon', pMatch'))

data POpaque = POpaque (Term POpaque)

instance PlutusType POpaque where
  type PInner POpaque _ = POpaque
  pCon' (POpaque x) = x
  pMatch' :: Term POpaque -> (POpaque -> Term b) -> Term b
  pMatch' x f = f (POpaque x)
