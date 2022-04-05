module Plutarch.Either (PEither (..), peither) where

import qualified GHC.Generics as GHC
import Generics.SOP (Generic, HasDatatypeInfo, I (I))
import Plutarch (
  PType,
  PlutusType,
  S,
  Term,
  phoistAcyclic,
  plam,
  pmatch,
  (#),
  type (:-->),
 )
import Plutarch.Bool (PEq)
import Plutarch.Show (PShow)

data PEither (a :: PType) (b :: PType) (s :: S)
  = PLeft (Term s a)
  | PRight (Term s b)
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, HasDatatypeInfo, PlutusType, PEq, PShow)

-- | 'PEither' destructor equivalent to 'either'
peither :: Term (s :: S) ((a :--> c) :--> (b :--> c) :--> PEither a b :--> c)
peither = phoistAcyclic $
  plam $ \left right either -> pmatch either $ \case
    PLeft x -> left # x
    PRight x -> right # x
