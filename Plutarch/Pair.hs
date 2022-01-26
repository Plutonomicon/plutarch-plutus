module Plutarch.Pair (PPair (..)) where

import qualified GHC.Generics as GHC
import Generics.SOP (Generic, I (I))
import Plutarch (PType, PlutusType, S, Term)

{- |
  Plutus encoding of Pairs.

  Note: This is represented differently than 'BuiltinPair'
-}
data PPair (a :: PType) (b :: PType) (s :: S)
  = PPair (Term s a) (Term s b)
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PlutusType)
