module Plutarch.Natural (
  PNatural (..),
  pnatToInt,
  pnatFromInt,
) where

import Plutarch (PlutusType (..), punsafeCoerce)
import Plutarch.Bool (
  PEq ((#==)),
  POrd ((#<), (#<=)),
  pif,
 )
import Plutarch.Builtin (
  PAsData,
  PIsData (..),
  pasInt,
  pforgetData,
 )
import Plutarch.Integer (PInteger)
import Plutarch.Lift (pconstant)
import Plutarch.Maybe (PMaybe (PJust, PNothing))
import Plutarch.Prelude

newtype PNatural (s :: S) = PNatural (Term s PInteger)

instance PIsData PNatural where
  pfromData x =
    phoistAcyclic
      ( plam $ \x' ->
          pmatch (pnatFromInt $ pasInt # pforgetData x') $ \case
            PJust n -> n
            PNothing -> perror
      )
      # x
  pdata x =
    phoistAcyclic
      ( plam $ \x' ->
          (punsafeCoerce :: Term _ (PAsData PInteger) -> Term _ (PAsData PNatural)) $
            pdata $ pnatToInt x'
      )
      # x

instance PlutusType PNatural where
  type PInner PNatural _ = PInteger
  pcon' (PNatural n) = n
  pmatch' p f = f $ PNatural p

instance PEq PNatural where
  l #== r =
    phoistAcyclic
      ( plam $ \l' r' ->
          pnatToInt l'
            #== pnatToInt r'
      )
      # l
      # r

instance POrd PNatural where
  l #<= r =
    phoistAcyclic
      ( plam $ \l' r' ->
          pnatToInt l'
            #<= pnatToInt r'
      )
      # l
      # r

  l #< r =
    phoistAcyclic
      ( plam $ \l' r' ->
          pnatToInt l'
            #< pnatToInt r'
      )
      # l
      # r

pnatToInt :: Term s PNatural -> Term s PInteger
pnatToInt nat = phoistAcyclic (plam $ \x -> pmatch x $ \(PNatural n) -> n) # nat

pnatFromInt :: Term s PInteger -> Term s (PMaybe PNatural)
pnatFromInt x =
  phoistAcyclic
    ( plam $ \i ->
        pif
          (i #< pconstant 0)
          (pcon PNothing)
          (pcon . PJust . pcon . PNatural $ i)
    )
    # x
