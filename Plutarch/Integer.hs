{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Integer (
  PInteger,
  ppowInteger,
  (#^),
  pdivideInteger,
  pmodInteger,
  pquotientInteger,
  premainderInteger,
) where

import Plutarch (punsafeBuiltin)
import Plutarch.Bool (PEq, POrd, pif, (#<), (#<=), (#==))
import Plutarch.Lift (
  DerivePConstantViaCoercible (DerivePConstantViaCoercible),
  PConstant,
  PLifted,
  PUnsafeLiftDecl,
  pconstant,
 )
import Plutarch.Numeric (
  PAdditiveGroup ((#-)),
  PAdditiveMonoid (pzero),
  PAdditiveSemigroup ((#+)),
  PEuclideanClosed (pdivMod),
  PMultiplicativeGroup (preciprocal),
  PMultiplicativeMonoid (pone),
  PMultiplicativeSemigroup ((#*)),
  pdiv,
  peven,
  pnegate,
 )
import Plutarch.Pair (PPair (PPair))
import Plutarch.Prelude
import qualified PlutusCore as PLC

-- | Plutus BuiltinInteger
data PInteger s

instance PUnsafeLiftDecl PInteger where type PLifted PInteger = Integer
deriving via (DerivePConstantViaCoercible Integer PInteger Integer) instance (PConstant Integer)

instance PEq PInteger where
  x #== y = punsafeBuiltin PLC.EqualsInteger # x # y

instance POrd PInteger where
  x #<= y = punsafeBuiltin PLC.LessThanEqualsInteger # x # y
  x #< y = punsafeBuiltin PLC.LessThanInteger # x # y

instance PAdditiveSemigroup PInteger where
  x #+ y = punsafeBuiltin PLC.AddInteger # x # y

instance PAdditiveMonoid PInteger where
  pzero = phoistAcyclic (pconstant 0)

instance PAdditiveGroup PInteger where
  x #- y = punsafeBuiltin PLC.SubtractInteger # x # y

instance PMultiplicativeSemigroup PInteger where
  x #* y = punsafeBuiltin PLC.MultiplyInteger # x # y

instance PMultiplicativeMonoid PInteger where
  pone = phoistAcyclic (pconstant 1)

instance PEuclideanClosed PInteger where
  pdivMod =
    phoistAcyclic
      ( plam $ \x y ->
          pif
            (y #== pzero)
            (pcon $ PPair pzero x)
            ( pcon $
                PPair
                  (punsafeBuiltin PLC.QuotientInteger # x # y)
                  (punsafeBuiltin PLC.RemainderInteger # x # y)
            )
      )

-- | Raise by an 'Integer' power.
ppowInteger :: PMultiplicativeGroup a => Term s a -> Term s PInteger -> Term s a
ppowInteger a int =
  phoistAcyclic
    ( plam $ \x i ->
        plet i $ \i' ->
          plet x $ \x' ->
            pif (i' #== pzero) pone $
              pif (i' #== pone) x' $
                plet (pexpBySquaring # x') $ \sqX ->
                  pif
                    (i' #< pzero)
                    (preciprocal $ sqX #$ pnegate i')
                    (sqX # i')
    )
    # a
    # int

-- | Operator version of 'ppowInteger'.
(#^) :: (PMultiplicativeGroup a) => Term s a -> Term s PInteger -> Term s a
(#^) = ppowInteger

infixr 8 #^

pdivideInteger :: Term s (PInteger :--> PInteger :--> PInteger)
pdivideInteger = punsafeBuiltin PLC.DivideInteger

pmodInteger :: Term s (PInteger :--> PInteger :--> PInteger)
pmodInteger = punsafeBuiltin PLC.ModInteger

pquotientInteger :: Term s (PInteger :--> PInteger :--> PInteger)
pquotientInteger = punsafeBuiltin PLC.QuotientInteger

premainderInteger :: Term s (PInteger :--> PInteger :--> PInteger)
premainderInteger = punsafeBuiltin PLC.RemainderInteger

-- Helpers

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
        plet (self # (acc #* acc) # (pdiv i (pconstant 2))) $ \x ->
          pif (peven # i) x (acc #* x)
