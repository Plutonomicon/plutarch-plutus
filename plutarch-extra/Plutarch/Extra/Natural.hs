-- This prevents errors on 'instance IntegralDomain PInteger PNatural'
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Extra.Natural (
  PNatural (..),
  pscaleNat,
  ppowNat,
  pnatToInt,
  pnatFromInt,
) where

import Plutarch (PlutusType (pcon', pmatch'))
import Plutarch.Unsafe (punsafeCoerce)
import Plutarch.Extra.Numeric
import Plutarch.Builtin (
  pasInt,
  pforgetData,
 )
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

instance PAdditiveSemigroup PNatural where
  (#+) x y =
    phoistAcyclic
      ( plam $ \x' y' ->
          pnatFromIntUnsafe $ pnatToInt x' #+ pnatToInt y'
      )
      # x
      # y

instance PAdditiveMonoid PNatural where
  pzero = pnatFromIntUnsafe pzero

instance PMultiplicativeSemigroup PNatural where
  (#*) x y =
    phoistAcyclic
      ( plam $ \x' y' ->
          pnatFromIntUnsafe $ pnatToInt x' #* pnatToInt y'
      )
      # x
      # y

instance PMultiplicativeMonoid PNatural where
  pone = pnatFromIntUnsafe pone

instance PAdditiveHemigroup PNatural where
  x #^- y =
    phoistAcyclic
      ( plam $ \n1 n2 ->
          plet (pnatToInt n1) $ \n1' ->
            plet (pnatToInt n2) $ \n2' ->
              pnatFromIntUnsafe $
                pif (n1' #<= n2') pzero (n1' #- n2')
      )
      # x
      # y

instance PEuclideanClosed PNatural where
  pdivMod =
    phoistAcyclic $
      ( plam $ \x y ->
          pmatch (pdivMod # (pnatToInt x) # (pnatToInt y)) $ \(PPair d r) ->
            pcon $
              PPair
                (pnatFromIntUnsafe d)
                (pnatFromIntUnsafe r)
      )

-- | Scale by a 'Natural' multiplier.
pscaleNat ::
  forall s a.
  (PAdditiveMonoid a) =>
  Term s PNatural ->
  Term s a ->
  Term s a
pscaleNat n a =
  phoistAcyclic
    ( pfix
        #$ plam
        $ \self nat x ->
          pif
            (nat #== pzero)
            pzero
            (x #+ (self # (nat #^- pone) # x))
    )
    # n
    # a

instance PIntegralDomain PInteger PNatural where
  pprojectAbs = pnatFromIntUnsafe . pabs
  paddExtend = pnatToInt
  prestrictMay = pnatFromInt
  pabs x' =
    phoistAcyclic
      ( plam $ \x ->
          pif (x #< pzero) (pnegate x) x
      )
      # x'

-- | Raise by a 'PNatural' power.
ppowNat :: (PMultiplicativeMonoid a) => Term s a -> Term s PNatural -> Term s a
ppowNat a nat =
  phoistAcyclic
    ( plam $ \x n ->
        pif
          (n #== pzero)
          pone
          (pmatch n $ \(PNatural i) -> pexpBySquaring # x # i)
    )
    # a
    # nat

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

-- We made sure that PInteger >= zero as it goes from PNatural.
pnatFromIntUnsafe :: Term s PInteger -> Term s PNatural
pnatFromIntUnsafe = pcon . PNatural

-- We secretly know that i is always positive.
pexpBySquaring ::
  forall s a.
  (PMultiplicativeMonoid a) =>
  Term s (a :--> PInteger :--> a)
pexpBySquaring = pfix #$ plam f
  where
    f :: Term s (a :--> PInteger :--> a) -> Term s a -> Term s PInteger -> Term s a
    f self acc i =
      pif (i #== pone) acc $
        plet (self # (acc #* acc) # (pdiv' # i # 2)) $ \x ->
          pif (peven # i) x (acc #* x)
