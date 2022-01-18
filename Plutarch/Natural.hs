module Plutarch.Natural (
  PNatural,
  pnatToInt,
  pnatFromInt,
  pmonusNat,
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
import Plutarch.Prelude
import qualified PlutusTx.Numeric as PTx

newtype PNatural (s :: S) = PNatural (Term s PInteger)

instance PIsData PNatural where
  pfromData x =
    phoistAcyclic
      ( plam $ \x' ->
          pnatFromInt #$ pasInt # pforgetData x'
      )
      # x
  pdata x =
    phoistAcyclic
      ( plam $ \x' ->
          (punsafeCoerce :: Term _ (PAsData PInteger) -> Term _ (PAsData PNatural)) $
            pdata $ pnatToInt # x'
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
          pnatToInt # l'
            #== pnatToInt # r'
      )
      # l
      # r

instance POrd PNatural where
  l #<= r =
    phoistAcyclic
      ( plam $ \l' r' ->
          pnatToInt # l'
            #<= pnatToInt # r'
      )
      # l
      # r

  l #< r =
    phoistAcyclic
      ( plam $ \l' r' ->
          pnatToInt # l'
            #< pnatToInt # r'
      )
      # l
      # r

instance PTx.AdditiveSemigroup (Term s PNatural) where
  l + r =
    phoistAcyclic
      ( plam $ \l' r' ->
          pnatFromInt
            #$ pnatToInt # l'
            + pnatToInt # r'
      )
      # l
      # r

instance PTx.AdditiveMonoid (Term s PNatural) where
  zero = pnatFromInt # 0

instance PTx.MultiplicativeSemigroup (Term s PNatural) where
  l * r =
    phoistAcyclic
      ( plam $ \l' r' ->
          pnatFromInt
            #$ pnatToInt # l'
            * pnatToInt # r'
      )
      # l
      # r

instance PTx.MultiplicativeMonoid (Term s PNatural) where
  one = pnatFromInt # 1

pnatToInt :: Term s (PNatural :--> PInteger)
pnatToInt = phoistAcyclic (plam $ \x -> pmatch x $ \(PNatural n) -> n)

pnatFromInt :: Term s (PInteger :--> PNatural)
pnatFromInt = phoistAcyclic (plam $ pcon . PNatural)

pmonusNat :: Term s PNatural -> Term s PNatural -> Term s PNatural
pmonusNat x y =
  phoistAcyclic
    ( plam $ \n1 n2 ->
        plet (pnatToInt # n1) $ \n1' ->
          plet (pnatToInt # n2) $ \n2' ->
            pnatFromInt
              #$ pif (n1' #<= n2') 0 (n1' - n2')
    )
    # x
    # y
