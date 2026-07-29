{-# LANGUAGE UndecidableSuperClasses #-}

module Plutarch.Numeric.Euclidean (
  PEuclidean (..),
) where

import Data.Kind (Type)
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (
  Term,
  pfix,
  plam',
  punsafeCoerce,
 )
import Plutarch.Numeric.Multiplicative (PMultiplicativeMonoid)
import Plutarch.Numeric.Zeroable (
  PAbs (pabs),
  PZeroable (PNonZero, ptoNonZero),
 )
import Plutarch.Primitive.Apply (pcoerce, (#), (#$))
import Plutarch.Primitive.BuiltinFun (
  pdivideInteger,
  pmodInteger,
  pquotientInteger,
  premainderInteger,
 )
import Plutarch.Primitive.Function ((:-->))
import Plutarch.Primitive.Numeric (PInteger, PNatural)

{- | = Laws

1. @(('pdiv' # x # y) #* 'pcoerce' y) #+ ('pmod' # x # y)@ @=@ @x@
2. @'pdiv' # 'pzero' # x@ @=@ @'pmod' # 'pzero' # x@ @=@ @'pzero'@
3. @'pgcd' # pcoerce x # x@ @=@ @x@
4. @'pgcd' # ('pmod' # x # y) # y@ @=@ @'pgcd' # x # y@
5. @'pgcd' # x # y@ @=@ @'pgcd' # ('pabs' # x) # y@
6. @'pgcd' # 'pzero' # y@ @=@ @y@

Additionally, 'pgcd' should be /morally/ commutative and associative. It is
not possible to state these laws in general due to the 'PNonZero' requirement
for the second argument.

@since wip
-}
class (PZeroable a, PAbs a, PAbs (PNonZero a), PMultiplicativeMonoid a) => PEuclidean (a :: S -> Type) where
  pdiv :: Term s (a :--> PNonZero a :--> a)
  pmod :: Term s (a :--> PNonZero a :--> a)
  pgcd :: Term s (a :--> PNonZero a :--> PNonZero a)
  pgcd = plam' $ \x -> plam' $ \y ->
    ptoNonZero x y (plam' $ \xnz -> go # xnz # pcoerce y)
    where
      go :: Term s (PNonZero a :--> a :--> PNonZero a)
      go = pfix $ \self -> plam' $ \x -> plam' $ \y ->
        ptoNonZero y (pabs # x) (plam' $ \ynz -> self # ynz #$ pmod # pcoerce x # ynz)

-- | @since wip
instance PEuclidean PNatural where
  pdiv = punsafeCoerce pquotientInteger
  pmod = punsafeCoerce premainderInteger

-- | @since wip
instance PEuclidean PInteger where
  pdiv = punsafeCoerce pdivideInteger
  pmod = punsafeCoerce pmodInteger
