{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Plutarch.Maybe (PMaybe (..)) where

import qualified GHC.Generics as GHC
import Generics.SOP
import Plutarch (
  PInner,
  PType,
  PlutusType,
  S,
  Term,
  gpcon,
  pcon',
  pdelay,
  plam,
  pmatch',
  (#),
 )

-- | Plutus Maybe type, with Scott-encoded repr
data PMaybe (a :: PType) (s :: S)
  = PJust (Term s a)
  | PNothing
  deriving stock (GHC.Generic)
  deriving anyclass (Generic)

instance PlutusType (PMaybe a) where
  pcon' :: forall s. PMaybe a s -> forall b. Term s (PInner (PMaybe a) b)
  pcon' x = gpcon @(PMaybe a) @s @_ @(Code (PMaybe a s)) $ from x

  pmatch' x f = x # plam (f . PJust) # pdelay (f PNothing)
