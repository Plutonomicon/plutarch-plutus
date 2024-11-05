module Plutarch.Internal.Ord (
  -- * Type classes
  PPartialOrd (..),
  POrd (..),
) where

import Plutarch.Builtin.Unit (PUnit)
import Plutarch.Internal.Builtin (
  PBool (PFalse, PTrue),
  PByteString,
  PInteger,
  pbuiltinIfThenElse,
  pbuiltinLessThanByteString,
  pbuiltinLessThanEqualsByteString,
  pbuiltinLessThanEqualsInteger,
  pbuiltinLessThanInteger,
  pnot,
  pto,
 )
import Plutarch.Internal.Eq (PEq)
import Plutarch.Internal.Lift (pconstant)
import Plutarch.Internal.PlutusType (PInner, pcon)
import Plutarch.Internal.Term (
  S,
  Term,
  plet,
  (#),
  (#$),
 )

{- | Partial ordering relation.

= Laws

'#<=' must form a partial order. More precisely:

1. @x #<= x@ @=@ @True@ (@#<=@ is reflexive)
2. @(x #<= y) #&& (y #<= x)@ @=@ @x #== y@ (@#<=@ is anti-symmetric)
3. @(x #<= y) #&& (y #<= z)@ @=@ @x #<= z@ (@#<= is transitive)

Furthermore, '#<' must be an equivalent strict partial order to '#<=':

4. @x #<= y@ @=@ @(x #< y) #|| (x #== y)@
5. @x #< x@ @=@ @False@ (@#<@ is irreflexive)
6. @x #< y@ @=@ @pnot (y #< x)@ (@#<@ is asymmetric)
7. @(x #< y) #&& (y #< z)@ @=@ @x #< z@ (@#<@ is transitive)

Lastly, if you define '#>=' or '#>', ensure that the following also hold:

8. @x #> y@ @=@ @y #< x@
9. @x #>= y@ @=@ @pnot (x #< y)@

The default implementations of '#>=' and '#>' ensure these laws.
-}
class PEq t => PPartialOrd t where
  (#<=) :: Term s t -> Term s t -> Term s PBool
  default (#<=) :: POrd (PInner t) => Term s t -> Term s t -> Term s PBool
  x #<= y = pto x #<= pto y
  (#<) :: Term s t -> Term s t -> Term s PBool
  default (#<) :: POrd (PInner t) => Term s t -> Term s t -> Term s PBool
  x #< y = pto x #< pto y

  -- | @since WIP
  (#>=) :: forall (s :: S). Term s t -> Term s t -> Term s PBool
  x #>= y = pnot #$ x #< y

  -- | @since WIP
  (#>) :: forall (s :: S). Term s t -> Term s t -> Term s PBool
  x #> y = y #< x

infix 4 #<=
infix 4 #<
infix 4 #>=
infix 4 #>

{- | Total ordering relation.

= Laws

'pmax' and 'pmin' must form a commutative semiring without identity
elements, where addition also distributes over multiplication. More
precisely:

1. @pmax x y@ @=@ @pmax y x@ (@pmax@ is commutative)
2. @pmin x y@ @=@ @pmin y x@ (@pmin@ is commutative)
3. @pmax x (pmax y z)@ @=@ @pmax (pmax x y) z@ (@pmax@ is associative)
4. @pmin x (pmin y z)@ @=@ @pmin (pmin x y) z@ (@pmin@ is associative)
5. @pmax x (pmin y z)@ @=@ @pmin (pmax x y) (pmax x z)@ (@pmax@ distributes
   over @pmin@)
6. @pmin x (pmax y z)@ @=@ @pmax (pmin x y) (pmin x z)@ (@pmin@ distributes
   over @pmax@)

Furthermore, the following must hold relative '#<':

7. @pmin x y@ @=@ @if (x #< y) then x else y@
8. @pmax x y@ @=@ @if (x #< y) then y else x@

Laws 7 and 8 are also the defaults, as for most types, this is the best you
can do.
-}
class PPartialOrd t => POrd t where
  -- | @since WIP
  pmax :: forall (s :: S). Term s t -> Term s t -> Term s t
  pmax x y = pbuiltinIfThenElse # (x #< y) # y # x

  -- | @since WIP
  pmin :: forall (s :: S). Term s t -> Term s t -> Term s t
  pmin x y = pbuiltinIfThenElse # (x #< y) # x # y

-- | @since WIP
instance PPartialOrd PBool where
  x #< y = pbuiltinIfThenElse # x # pconstant False # y
  x #<= y = pbuiltinIfThenElse # x # y # pconstant True

-- | @since WIP
instance POrd PBool

-- | @since WIP
instance PPartialOrd PUnit where
  {-# INLINEABLE (#<=) #-}
  x #<= y = plet x $ \_ ->
    plet y $ \_ ->
      pcon PTrue
  {-# INLINEABLE (#<) #-}
  x #< y = plet x $ \_ ->
    plet y $ \_ ->
      pcon PFalse

-- | @since WIP
instance POrd PUnit

-- | @since WIP
instance PPartialOrd PInteger where
  x #<= y = pbuiltinLessThanEqualsInteger # x # y
  x #< y = pbuiltinLessThanInteger # x # y

-- | @since WIP
instance POrd PInteger

-- | @since WIP
instance PPartialOrd PByteString where
  x #<= y = pbuiltinLessThanEqualsByteString # x # y
  x #< y = pbuiltinLessThanByteString # x # y

-- | @since WIP
instance POrd PByteString
