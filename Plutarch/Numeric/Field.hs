{-# LANGUAGE UndecidableSuperClasses #-}

module Plutarch.Numeric.Field (
  PDistributive (..),
  PSemiring (..),
  PRing (..),
  PField (..),
) where

import Data.Kind (Type)
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (Term, plam')
import Plutarch.Numeric.Additive (
  PAdditiveGroup,
  PAdditiveMonoid,
  PAdditiveSemigroup,
 )
import Plutarch.Numeric.Euclidean (PEuclidean)
import Plutarch.Numeric.Multiplicative (PMultiplicativeMonoid)
import Plutarch.Numeric.Zeroable (PNonZero)
import Plutarch.Primitive.Apply (pcoerce)
import Plutarch.Primitive.Function ((:-->))
import Plutarch.Primitive.Numeric (PInteger, PNatural, PPositive)

{- | = Laws

1. @'pfromPositive' '#' 'pone'@ @=@ @'pone'@
2. @('pfromPositive' '#' x) '#+' ('pfromPositive' '#' y)@ @=@ @'pfromPositive' '#' (x '#+' y)@
3. @('pfromPositive' '#' x) '#*' ('pfromPositive' '#' y)@ @=@ @'pfromPositive' '#' (x '#*' y)@

@since wip
-}
class (PAdditiveSemigroup a, PMultiplicativeMonoid a) => PDistributive (a :: S -> Type) where
  pfromPositive :: Term s (PPositive :--> a)

-- | @since wip
instance PDistributive PPositive where
  pfromPositive = plam' id

-- | @since wip
instance PDistributive PNatural where
  pfromPositive = plam' pcoerce

-- | @since wip
instance PDistributive PInteger where
  pfromPositive = plam' $ pcoerce . pcoerce

{- | = Laws

1. @'pfromNatural' '#' 'pzero'@ @=@ @'pzero'@

@since wip
-}
class (PDistributive a, PAdditiveMonoid a) => PSemiring (a :: S -> Type) where
  pfromNatural :: Term s (PNatural :--> a)

-- | @since wip
instance PSemiring PNatural where
  pfromNatural = plam' id

-- | @since wip
instance PSemiring PInteger where
  pfromNatural = plam' pcoerce

{- | = Laws

1. @'pfromInteger' '#$' 'pnegate' '#' x@ @=@ @'pnegate' '#$' 'pfromInteger' '#' x@

@since wip
-}
class (PSemiring a, PAdditiveGroup a) => PRing (a :: S -> Type) where
  pfromInteger :: Term s (PInteger :--> a)

-- | @since wip
instance PRing PInteger where
  pfromInteger = plam' id

{- | = Laws

1. @'precip' '#$' 'precip' '#' x@ @=@ @x@
2. @x '#*' ('precip' '#' y)@ @=@ @'pdiv' '#' x '#' y@
3. @'precip' '#' 'pone'@ @=@ @'pone'@
4. @'ptoNonZero' z f ('pmod' '#' x '#' y)@ @=@ @z@
5. @'pdiv' '#' pone '#' y@ @=@ @'precip' '#' y@

@since wip
-}
class PEuclidean a => PField (a :: S -> Type) where
  precip :: Term s (PNonZero a :--> PNonZero a)
