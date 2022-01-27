module Plutarch.Either (PEither (..)) where

import qualified GHC.Generics as GHC
import Generics.SOP (Generic, I (I))
import Plutarch (PType, PlutusType, S, Term)

data PEither (a :: PType) (b :: PType) (s :: S)
  = PLeft (Term s a)
  | PRight (Term s b)
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PlutusType)
