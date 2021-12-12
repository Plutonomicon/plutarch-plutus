module Plutarch.Opaque (POpaque(..), pOpaque, pUnsafeFromOpaque) where

import Plutarch.Prelude
import Plutarch (PlutusType(PInner, pCon', pMatch'))

data POpaque s = POpaque (Term s POpaque)

instance PlutusType POpaque where
  type PInner POpaque _ = POpaque
  pCon' (POpaque x) = x
  pMatch' x f = f (POpaque x)

pOpaque :: Term s a -> Term s POpaque
pOpaque = pUnsafeCoerce

pUnsafeFromOpaque :: Term s POpaque -> Term s a
pUnsafeFromOpaque = pUnsafeCoerce
