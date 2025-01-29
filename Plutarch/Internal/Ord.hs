module Plutarch.Internal.Ord (
  POrd (..),
  (#>),
  (#>=),
) where

import Data.Kind (Type)
import Plutarch.Builtin.Bool (
  PBool,
  pand',
  pfalse,
  pif',
  por',
  ptrue,
 )
import Plutarch.Builtin.ByteString (
  PByte,
  PByteString,
  PEndianness,
  PLogicOpSemantics,
 )
import Plutarch.Builtin.Integer (
  PInteger,
  pleInteger,
  pltInteger,
 )
import Plutarch.Builtin.Unit (PUnit)
import Plutarch.Internal.Eq (PEq)
import Plutarch.Internal.Lift (pconstant)
import Plutarch.Internal.Other (pto)
import Plutarch.Internal.PlutusType (PInner)
import Plutarch.Internal.Term (
  S,
  Term,
  plet,
  punsafeBuiltin,
  (#),
 )
import PlutusCore qualified as PLC

{- | Total ordering relation.

= Laws

'#<=' must form a total order. More precisely:

1. @x #<= x@ @=@ @pcon PTrue@ (reflexivity)
2. @(y #< x) #|| (z #< y) #|| (x #<= z)@ @=@ @pcon PTrue@ (transitivity)
3. @(x #<= y) #|| (y #<= x)@ @=@ @pcon PTrue@ (totality)

Furthermore, '#<' must be an equivalent strict total order to '#<=':

4. @x #< x@ @=@ @pcon PFalse@ (irreflexivity)
5. @(y #<= x) #|| (z #<= y) #|| (x #< z)@ @=@ @pcon PTrue@ (transitivity)
6. @(x #< y) #|| (y #< x) #|| (x #== z)@ @=@ @pcon PTrue@ (trichotomy)
7. @x #<= y@ @=@ @(x #< y) #|| (x #== y)@ (strict equivalence)

If you define 'pmax' or 'pmin', ensure the following also hold:

8. @pmax # x # y@ @=@ @pmax # y # x@ (commutativity, also for @pmin)
9. @pmax # x #$ pmax y z@ @=@ @pmax # (pmax # x # y) # z@ (associativity,
    also for @pmin)
10. @pmax # x #$ pmin # y # z@ @=@ @pmin # (pmax # x # y) # (pmax # x # z)@
    ('pmax' distributes over 'pmin', also equivalent for 'pmin')
11. @pmin x y@ @=@ @pif' (x #<= y) x y@
12. @pmax x y@ @=@ @pif' (x #<= y) y x@

Laws 8-12 hold if you use the defaults provided by this type class.

@since 1.10.0
-}
class PEq t => POrd t where
  -- | @since 1.10.0
  {-# INLINEABLE (#<=) #-}
  (#<=) :: Term s t -> Term s t -> Term s PBool
  default (#<=) :: POrd (PInner t) => Term s t -> Term s t -> Term s PBool
  x #<= y = pto x #<= pto y

  -- | @since 1.10.0
  {-# INLINEABLE (#<) #-}
  (#<) :: Term s t -> Term s t -> Term s PBool
  default (#<) :: POrd (PInner t) => Term s t -> Term s t -> Term s PBool
  x #< y = pto x #< pto y

  -- | @since 1.10.0
  {-# INLINEABLE pmax #-}
  pmax :: forall (s :: S). Term s t -> Term s t -> Term s t
  pmax x y = pif' # (x #<= y) # y # x

  -- | @since 1.10.0
  {-# INLINEABLE pmin #-}
  pmin :: forall (s :: S). Term s t -> Term s t -> Term s t
  pmin x y = pif' # (x #<= y) # x # y

infix 4 #<=
infix 4 #<

-- | @since 1.10.0
(#>) ::
  forall (a :: S -> Type) (s :: S).
  POrd a =>
  Term s a ->
  Term s a ->
  Term s PBool
x #> y = y #< x

infix 4 #>

-- | @since 1.10.0
(#>=) ::
  forall (a :: S -> Type) (s :: S).
  POrd a =>
  Term s a ->
  Term s a ->
  Term s PBool
x #>= y = y #<= x

infix 4 #>=

instance POrd PBool where
  {-# INLINEABLE (#<) #-}
  x #< y = pif' # x # pconstant False # y
  {-# INLINEABLE (#<=) #-}
  x #<= y = pif' # x # y # pconstant True
  {-# INLINEABLE pmin #-}
  pmin x y = pand' # x # y
  {-# INLINEABLE pmax #-}
  pmax x y = por' # x # y

instance POrd PInteger where
  {-# INLINEABLE (#<=) #-}
  x #<= y = pleInteger # x # y
  {-# INLINEABLE (#<) #-}
  x #< y = pltInteger # x # y

-- | @since 1.10.0
instance POrd PByteString where
  {-# INLINEABLE (#<=) #-}
  x #<= y = punsafeBuiltin PLC.LessThanEqualsByteString # x # y
  {-# INLINEABLE (#<) #-}
  x #< y = punsafeBuiltin PLC.LessThanByteString # x # y

-- | @since 1.10.0
instance POrd PByte where
  {-# INLINEABLE (#<=) #-}
  x #<= y = punsafeBuiltin PLC.LessThanEqualsInteger # x # y
  {-# INLINEABLE (#<) #-}
  x #< y = punsafeBuiltin PLC.LessThanInteger # x # y

deriving anyclass instance POrd PLogicOpSemantics

-- | @since 1.10.0
instance POrd PUnit where
  {-# INLINEABLE (#<=) #-}
  x #<= y = plet x $ \_ -> plet y $ const ptrue
  {-# INLINEABLE (#<) #-}
  x #< y = plet x $ \_ -> plet y $ const pfalse
  {-# INLINEABLE pmax #-}
  pmax x y = plet x $ \_ -> plet y $ const x
  {-# INLINEABLE pmin #-}
  pmin = pmax

-- | @since 1.10.0
deriving anyclass instance POrd PEndianness
