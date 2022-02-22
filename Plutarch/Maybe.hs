module Plutarch.Maybe ( PMaybe (..)
                      , pfromMaybe 
                      ) where

import qualified GHC.Generics as GHC
import Generics.SOP (Generic, I (I))
import Plutarch.Util ( type (:$) )
import Plutarch (
  PType,
  PlutusType,
  S,
  Term,
  type (:-->),
  phoistAcyclic,
  pmatch,
  plam,
  unTermCont,
  perror,
 )

import Plutarch.TermCont (
  tcont
 )



-- | Plutus Maybe type, with Scott-encoded repr
data PMaybe (a :: PType) (s :: S)
  = PJust (Term s a)
  | PNothing
  deriving stock (GHC.Generic)
  deriving anyclass (Generic, PlutusType)

pfromMaybe :: Term s :$ PMaybe a :--> a
pfromMaybe = phoistAcyclic $
  plam $ \maybe -> unTermCont $ do
    res <- tcont $ pmatch maybe
    pure $ case res of
      PNothing -> perror
      PJust a -> a

