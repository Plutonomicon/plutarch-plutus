module Plutarch.Opaque (POpaque(..), pOpaque, pUnsafeFromOpaque) where

import Plutarch.Prelude
import Plutarch (PlutusType(PInner, pCon', pMatch'))

data POpaque = POpaque (Term POpaque)

instance PlutusType POpaque where
  type PInner POpaque _ = POpaque
  pCon' (POpaque x) = x
  pMatch' :: Term POpaque -> (POpaque -> Term b) -> Term b
  pMatch' x f = f (POpaque x)

pOpaque :: Term a -> Term POpaque
pOpaque = pUnsafeCoerce

pUnsafeFromOpaque :: Term POpaque -> Term a
pUnsafeFromOpaque = pUnsafeCoerce
