{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Api.V1.Maybe (
  PMaybeData (PDJust, PDNothing),
  pfromMaybe,
) where

import qualified GHC.Generics as GHC
import Generics.SOP (Generic, I (I))

import Plutarch.DataRepr (PIsDataReprInstances (PIsDataReprInstances))
import Plutarch.Prelude
import Plutarch.Util (type (:$))

-- | Data encoded Maybe type. Used in various ledger api types.
data PMaybeData a (s :: S)
  = PDJust (Term s (PDataRecord '["_0" ':= a]))
  | PDNothing (Term s (PDataRecord '[]))
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)
  deriving anyclass (PIsDataRepr)
  deriving
    (PlutusType, PIsData)
    via PIsDataReprInstances (PMaybeData a)

pfromMaybe :: Term s :$ PMaybe a :--> a
pfromMaybe = phoistAcyclic $
  plam $ \maybe -> unTermCont $ do
    res <- tcont $ pmatch maybe
    pure $ case res of
      PNothing -> perror
      PJust a -> a
