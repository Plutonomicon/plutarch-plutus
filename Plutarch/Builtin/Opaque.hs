module Plutarch.Builtin.Opaque where

import Plutarch.Internal.Term

-- | An Arbitrary Term with an unknown type
newtype POpaque s = POpaque (Term s POpaque)

-- | Erase the type of a Term
popaque :: Term s a -> Term s POpaque
popaque = punsafeCoerce
