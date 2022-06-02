module Plutarch.These (PThese (..)) where

import qualified GHC.Generics as GHC
import Generics.SOP
import Plutarch.Prelude

-- | Plutus These type with Scott-encoded representation.
data PThese (a :: PType) (b :: PType) (s :: S)
  = PThis (Term s a)
  | PThat (Term s b)
  | PThese (Term s a) (Term s b)
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PlutusType)
