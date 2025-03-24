module Plutarch.Internal.Quantification (PForall (PForall)) where

import Plutarch.Internal.Term (PType, Term)

type PForall :: (a -> PType) -> PType
newtype PForall (b :: a -> PType) s = PForall (forall (x :: a). Term s (b x))
