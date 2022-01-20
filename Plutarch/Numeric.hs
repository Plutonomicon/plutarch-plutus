{-# LANGUAGE FunctionalDependencies #-}

module Plutarch.Numeric (
  -- * Type classes
  PAdditiveSemigroup (..),
  PAdditiveMonoid (..),
  PAdditiveGroup (..),
  PMultiplicativeSemigroup (..),
  PMultiplicativeMonoid (..),
  PAdditiveHemigroup (..),
  PEuclideanClosed (..),
  PMultiplicativeGroup (..),
  PIntegralDomain (..),
  
  -- * Helper types
  PField,
  PHemifield,
  PHemiring,

  -- * Helper functions
  pnegate,
  ppowInteger,
  pdiv,
  prem,
  pscaleNat,
  ppowNat,
  peven,
  (#^-),
  (#^),

) where

import Plutarch.Bool (
  PBool,
  PEq ((#==)),
  POrd ((#<), (#<=)),
  pif,
 )
import Plutarch.Maybe (PMaybe (PJust, PNothing))
import Plutarch.Pair (PPair (PPair))
import Plutarch.Natural (PNatural (PNatural), pnatToInt, pnatFromInt)
import Plutarch.Integer (PInteger)
import qualified Plutarch.Integer as PInteger (pquot, prem)
import Plutarch.Prelude

{- | A Plutrch level 'Semigroup' that it is sensible to describe
 using addition.
-}
class PAdditiveSemigroup a where
  (#+) :: Term s a -> Term s a -> Term s a

infixl 6 #+

instance PAdditiveSemigroup PInteger where
  (#+) = (+)

instance PAdditiveSemigroup PNatural where
  (#+) x y =
    phoistAcyclic
      ( plam $ \x' y' ->
          pnatFromInt $ pnatToInt x' + pnatToInt y'
      )
      # x
      # y

{- | A Plutarch level 'Monoid' that it is sensible to describe
 using addition and zero.
-}
class PAdditiveSemigroup a => PAdditiveMonoid a where
  pzero :: Term s a

instance PAdditiveMonoid PInteger where
  pzero = 0

instance PAdditiveMonoid PNatural where
  pzero = pnatFromInt 0

{- | A Plutarch level 'Group' that it is sensible to describe
 using addition, pzero, and subtraction.
-}
class PAdditiveMonoid a => PAdditiveGroup a where
  (#-) :: Term s a -> Term s a -> Term s a

instance PAdditiveGroup PInteger where
  (#-) = (-)

infixl 6 #-

pnegate :: PAdditiveGroup a => Term s a -> Term s a
pnegate x = pzero #- x

{- | A Plutarch level 'Semigroup' that it is sensible to describe
 using multiplication.
-}
class PMultiplicativeSemigroup a where
  (#*) :: Term s a -> Term s a -> Term s a

infixl 7 #*

instance PMultiplicativeSemigroup PInteger where
  (#*) = (*)

instance PMultiplicativeSemigroup PNatural where
  (#*) x y =
    phoistAcyclic
      ( plam $ \x' y' ->
          pnatFromInt $ pnatToInt x' * pnatToInt y'
      )
      # x
      # y

{- | A Plutarch level 'Semigroup' that it is sensible to describe
 using multiplication and one.
-}
class PMultiplicativeSemigroup a => PMultiplicativeMonoid a where
  pone :: Term s a

instance PMultiplicativeMonoid PInteger where
  pone = 1

instance PMultiplicativeMonoid PNatural where
  pone = pnatFromInt 1

-- | A Plutarch level semiring.
type PSemiring a = (PAdditiveMonoid a, PMultiplicativeMonoid a)

-- | A Plutarch level ring.
type PRing a = (PAdditiveGroup a, PMultiplicativeMonoid a)

-- | An 'PAdditiveMonoid' with a notion of Plutarch level monus.
class (PAdditiveMonoid a) => PAdditiveHemigroup a where
  pmonus :: Term s a -> Term s a -> Term s a

instance PAdditiveHemigroup PNatural where
  pmonus x y =
    phoistAcyclic
      ( plam $ \n1 n2 ->
          plet (pnatToInt n1) $ \n1' ->
            plet (pnatToInt n2) $ \n2' ->
              pnatFromInt $
                pif (n1' #<= n2') 0 (n1' - n2')
      )
      # x
      # y

type PHemiring a = (PAdditiveHemigroup a, PMultiplicativeMonoid a)

{- | A 'PSemiring' with a notion of (kind of) Euclidean division. This differs from
 the mathematical notion of this, as we do not exclude zero.
-}
class (POrd a, PSemiring a) => PEuclideanClosed a where
  pdivMod :: Term s (a :--> a :--> PPair a a)

instance PEuclideanClosed PInteger where
  pdivMod =
    phoistAcyclic
      ( plam $ \x y ->
          pif (y #== pzero)
            (pcon $ PPair pzero x)
            (pcon $ PPair (PInteger.pquot # x # y) (PInteger.prem # x # y))
      )
      
instance PEuclideanClosed PNatural where
  pdivMod =
    phoistAcyclic $
      ( plam $ \x y ->
          pmatch (pdivMod # (pnatToInt x) # (pnatToInt y)) $ \(PPair d r) ->
            pcon $ PPair (pnatFromInt d) (pnatFromInt r)
      )
             
{- | A 'PMultiplicativeMonoid' with a notion of multiplicative inverse (for
 non-zero values).

 We have to exclude division by zero, as it leads to paradoxical situations.
 This does mean that '#/' and 'preciprocal' aren't total, but there's not much
 we can do about that.
-}
class (PMultiplicativeMonoid a) => PMultiplicativeGroup a where
  {-# MINIMAL (#/) | preciprocal #-}
  (#/) :: Term s a -> Term s a -> Term s a
  x #/ y = x #* preciprocal y
  preciprocal :: Term s a -> Term s a
  preciprocal x = pone #/ x

-- | Raise by an 'Integer' power.
ppowInteger :: PMultiplicativeGroup a => Term s a -> Term s PInteger -> Term s a
ppowInteger a i =
  phoistAcyclic
    ( plam $ \ x i -> 
        pif (i #== 0) pone $
          pif (i #== 1) x $
            plet (pexpBySquaring # x) $ \ sqX ->
              pif (i #< 0)
                (preciprocal $ sqX #$ pabs i)
                (sqX # i)
    )
    # a
    # i

type PField a = (PAdditiveGroup a, PMultiplicativeGroup a)

type PHemifield a = (PAdditiveHemigroup a, PMultiplicativeGroup a)

{- | A 'PRing' with a notion of \'sign\' or \'direction\' separate from magnitude.

 We extend the notion of integral domain with the idea of an \'additive
 restriction\', which is a type representing \'strictly positive\' values. For
 example, the \'additive restriction\' of 'PInteger' is 'PNatural', and the
 \'additive restricton\' of 'PRational' is 'PNatRatio'. This gives us the
 ability to move between these two types while preserving magnitude.

-}
class (PEq a, POrd a, PRing a) => PIntegralDomain a r | a -> r, r -> a where
  pabs :: Term s a -> Term s a
  pprojectAbs :: Term s a -> Term s r
  paddExtend :: Term s r -> Term s a
  prestrictMay :: Term s a -> Term s (PMaybe r)
  prestrictMay x =
    phoistAcyclic 
      ( plam $ \x ->
          pif (pabs x #== x)
            (pcon . PJust $ pprojectAbs x)
            (pcon PNothing)
      )
      # x
  psignum :: Term s a -> Term s a
  psignum x =
    phoistAcyclic
      ( plam $ \x ->
          pif (x #== pzero) pzero $
            pif (x #< pzero) (pnegate pone) (pone)
      )
      # x

instance PIntegralDomain PInteger PNatural where
  pabs = abs
  pprojectAbs = pnatFromInt . abs
  paddExtend = pnatToInt
  psignum = signum

-- | Operator version of 'pmonus'.
(#^-) :: (PAdditiveHemigroup a) =>  Term s a -> Term s a -> Term s a
x #^- y = pmonus x y

infixl 6 #^-

-- | Gets only the division part of a 'pdivMod'.
pdiv :: (PEuclideanClosed a) => Term s a -> Term s a -> Term s a
pdiv a1 a2 =
  phoistAcyclic
    ( plam $ \x y -> pmatch (pdivMod # x # y) $ \(PPair d _) -> d )
    # a1
    # a2

-- | Gets only the remainder part of a 'divMod'.
prem :: (PEuclideanClosed a) => Term s a -> Term s a -> Term s a
prem a1 a2 =
  phoistAcyclic
    ( plam $ \x y -> pmatch (pdivMod # x # y) $ \(PPair _ r) -> r )
    # a1
    # a2

-- | Operator version of 'ppowInteger'.
(#^) :: (PMultiplicativeGroup a) => Term s a -> Term s PInteger -> Term s a
(#^) = ppowInteger

infixr 8 #^

-- | Scale by a 'Natural' multiplier.
pscaleNat ::
  forall s a.
  (PAdditiveMonoid a) =>
  Term s PNatural ->
  Term s a ->
  Term s a
pscaleNat n a =
  phoistAcyclic
    ( pfix #$
        plam $ \self nat x ->
          pif (nat #== pzero)
            pzero
            (x #+ (self # (nat #^- pone) # x))
    )
    # n
    # a

-- | Raise by a 'PNatural' power.
ppowNat :: (PMultiplicativeMonoid a) => Term s a -> Term s PNatural -> Term s a
ppowNat a nat =
  phoistAcyclic
    ( plam $ \x n ->
        pif (n #== pzero)
          pone
          (pmatch n $ \(PNatural i) -> pexpBySquaring # x # i)
    )
    # a
    # nat

peven :: (PEq a, PEuclideanClosed a) => Term s (a :--> PBool)
peven =
  phoistAcyclic $
    ( plam $ \x ->
        prem x (pone #+ pone) #== pzero
    )

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
        pif (i #== 1) acc $
            plet (self # (acc #* acc) # (pdiv i 2)) $ \x ->
              pif (peven # i) x (acc #* x)