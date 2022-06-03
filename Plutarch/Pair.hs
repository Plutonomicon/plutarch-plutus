module Plutarch.Pair (PPair (..)) where

import qualified GHC.Generics as GHC
import Generics.SOP (Generic, HasDatatypeInfo, I (I))
import Plutarch.Bool (PEq)
import Plutarch.Internal (PType, S, Term)
import Plutarch.Internal.PlutusType (PlutusType)
import Plutarch.Show (PShow)

{- |
  Plutus encoding of Pairs.

  Note: This is represented differently than 'BuiltinPair'. It is scott-encoded.
-}
data PPair (a :: PType) (b :: PType) (s :: S)
  = PPair (Term s a) (Term s b)
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, HasDatatypeInfo, PlutusType, PEq, PShow)
