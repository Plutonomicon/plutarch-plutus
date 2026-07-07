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
  punsafeConstant,
 )
import Plutarch.Numeric.Additive (pnegate)
import Plutarch.Numeric.Helpers (pizero)
import Plutarch.Numeric.Multiplicative (PMultiplicativeMonoid)
import Plutarch.Numeric.Zeroable (PZeroable (PNonZero))
import Plutarch.Primitive.Apply ((#), (#$))
import Plutarch.Primitive.Bool (pif)
import Plutarch.Primitive.BuiltinFun (
  pdivideInteger,
  pequalsInteger,
  plessThanEqualsInteger,
  pmodInteger,
  pquotientInteger,
  premainderInteger,
 )
import Plutarch.Primitive.Function ((:-->))
import Plutarch.Primitive.Numeric (PInteger, PNatural)
import PlutusCore qualified as PLC

{- | = Laws

1. @(('pdiv' # x # y) #* 'pcoerce' y) #+ ('pmod' # x # y)@ @=@ @x@
2. @'pdiv' # 'pzero' # x@ @=@ @'pmod' # 'pzero' # x@ @=@ @'pzero'@
3. @'pgcd' # pcoerce x # x@ @=@ @'pcoerce' x@
4. @'pgcd' # ('pmod' # x # y) # y@ @=@ @'pgcd' # x # y@

Additionally, 'pgcd' should be /morally/ commutative and associative. It is
not possible to state these laws in general due to the 'PNonZero' requirement
for the second argument.

@since wip
-}
class (PZeroable a, PMultiplicativeMonoid a) => PEuclidean (a :: S -> Type) where
  pdiv :: Term s (a :--> PNonZero a :--> a)
  pmod :: Term s (a :--> PNonZero a :--> a)
  pgcd :: Term s (a :--> PNonZero a :--> a)
  pdegree :: Term s (a :--> PNatural)

-- | @since wip
instance PEuclidean PNatural where
  pdiv = punsafeCoerce pquotientInteger
  pmod = punsafeCoerce premainderInteger
  pgcd = punsafeCoerce pgcdInteger
  pdegree = plam' id

-- | @since wip
instance PEuclidean PInteger where
  pdiv = punsafeCoerce pdivideInteger
  pmod = punsafeCoerce pmodInteger
  pgcd = punsafeCoerce pgcdInteger
  pdegree = plam' $ \x ->
    punsafeCoerce $
      pif
        (plessThanEqualsInteger # x # punsafeConstant (PLC.someValue @Integer (-1)))
        (pnegate # x)
        x

-- Helpers

pgcdInteger ::
  forall (s :: S).
  Term s (PInteger :--> PInteger :--> PInteger)
pgcdInteger = pfix $ \self -> plam' $ \x -> plam' $ \y ->
  pif
    (pequalsInteger # y # pizero)
    (punsafeCoerce $ pdegree # x)
    (self # y #$ pmod # x # punsafeCoerce y)
