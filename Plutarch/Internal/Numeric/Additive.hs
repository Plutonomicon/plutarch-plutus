{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Internal.Numeric.Additive (
  -- * Types
  PPositive,
  Positive,

  -- * Type classes
  PAdditiveSemigroup (..),
  PAdditiveMonoid (..),
  PAdditiveGroup (..),

  -- * Functions
  ptryPositive,
  ppositive,
  pbySquaringDefault,
) where

import Data.Coerce (coerce)
import Data.Kind (Type)
import GHC.Generics (Generic)
import Plutarch.Builtin.BLS (
  PBuiltinBLS12_381_G1_Element,
  PBuiltinBLS12_381_G2_Element,
  pbls12_381_G1_add,
  pbls12_381_G1_compressed_zero,
  pbls12_381_G1_neg,
  pbls12_381_G1_scalarMul,
  pbls12_381_G1_uncompress,
  pbls12_381_G2_add,
  pbls12_381_G2_compressed_zero,
  pbls12_381_G2_neg,
  pbls12_381_G2_scalarMul,
  pbls12_381_G2_uncompress,
 )
import Plutarch.Builtin.Bool (pif)
import Plutarch.Builtin.Integer (
  PInteger,
  paddInteger,
  pconstantInteger,
  pmultiplyInteger,
  pquotientInteger,
  premainderInteger,
  psubtractInteger,
 )
import Plutarch.Builtin.String (ptraceInfo)
import Plutarch.Internal.Eq (PEq ((#==)))
import Plutarch.Internal.Fix (pfix)
import Plutarch.Internal.IsData (PIsData)
import Plutarch.Internal.Lift (
  DeriveNewtypePLiftable,
  PLiftable,
  PLifted (PLifted),
 )
import Plutarch.Internal.Newtype (PlutusTypeNewtype)
import Plutarch.Internal.Ord (POrd ((#<=)))
import Plutarch.Internal.Other (pto)
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.PlutusType (
  DerivePlutusType (DPTStrat),
  PInner,
  PlutusType,
  pcon,
 )
import Plutarch.Internal.Term (
  S,
  Term,
  perror,
  phoistAcyclic,
  plet,
  punsafeBuiltin,
  punsafeCoerce,
  (#),
  (#$),
  (:-->),
 )
import Plutarch.Maybe (PMaybe (PJust, PNothing))
import Plutarch.Unsafe (punsafeDowncast)
import PlutusCore qualified as PLC
import Prettyprinter (Pretty)
import Test.QuickCheck (
  Arbitrary,
  CoArbitrary,
  Function (function),
  functionMap,
 )
import Test.QuickCheck qualified as QuickCheck

-- | @since WIP
newtype PPositive (s :: S) = PPositive (Term s PInteger)
  deriving stock
    ( -- | @since WIP
      Generic
    )
  deriving anyclass
    ( -- | @since WIP
      PlutusType
    , -- | @since WIP
      PIsData
    , -- | @since WIP
      PEq
    , -- | @since WIP
      POrd
    )

-- | @since WIP
instance DerivePlutusType PPositive where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
deriving via
  DeriveNewtypePLiftable PPositive PInteger Positive
  instance
    PLiftable PPositive

-- | @since WIP
newtype Positive = UnsafeMkPositive {getPositive :: Integer}
  deriving stock
    ( -- | @since WIP
      Show
    , -- | @since WIP
      Eq
    , -- | @since WIP
      Ord
    )
  deriving
    ( -- | @since WIP
      Arbitrary
    )
    via QuickCheck.Positive Integer
  deriving
    ( -- | @since WIP
      CoArbitrary
    , -- | @since WIP
      Pretty
    )
    via Integer

-- | @since WIP
instance Function Positive where
  {-# INLINEABLE function #-}
  function = functionMap @Integer coerce coerce

{- | The addition operation.

= Laws

1. @x #+ y@ @=@ @y #+ x@ (commutativity of @#+@)
2. @x #+ (y #+ z)@ @=@ @(x #+ y) #+ z@ (associativity of @#+@)

If you define a custom @pscalePositive@, ensure the following also hold:

3. @pscalePositive # x # pone@ @=@ @x@
4. @(pscalePositive # x # n) #+ (pscalePositive # x # m)@ @=@
   @pscalePositive # x # (n #+ m)@
5. @pscalePositive # (pscalePositive # x # n) # m@ @=@
   @pscalePositive # x # (n #* m)@

The default implementation ensures these laws are satisfied.

@since WIP
-}
class PAdditiveSemigroup (a :: S -> Type) where
  (#+) :: forall (s :: S). Term s a -> Term s a -> Term s a
  default (#+) ::
    forall (s :: S).
    PAdditiveSemigroup (PInner a) =>
    Term s a ->
    Term s a ->
    Term s a
  x #+ y = punsafeDowncast $ pto x #+ pto y
  {-# INLINEABLE pscalePositive #-}

  -- | This defaults to exponentiation-by-squaring, which in general is the best
  -- we can do.
  pscalePositive ::
    forall (s :: S).
    Term s (a :--> PPositive :--> a)
  pscalePositive = phoistAcyclic $ plam $ \b e ->
    pbySquaringDefault (#+) # b # e

-- | @since WIP
infix 6 #+

-- | @since WIP
instance PAdditiveSemigroup PPositive where
  {-# INLINEABLE (#+) #-}
  x #+ y = punsafeCoerce $ paddInteger # pto x # pto y
  {-# INLINEABLE pscalePositive #-}
  pscalePositive = punsafeBuiltin PLC.MultiplyInteger

-- | @since WIP
instance PAdditiveSemigroup PInteger where
  {-# INLINEABLE (#+) #-}
  x #+ y = paddInteger # x # y
  {-# INLINEABLE pscalePositive #-}
  pscalePositive = punsafeBuiltin PLC.MultiplyInteger

-- | @since WIP
instance PAdditiveSemigroup PBuiltinBLS12_381_G1_Element where
  {-# INLINEABLE (#+) #-}
  x #+ y = pbls12_381_G1_add # x # y
  {-# INLINEABLE pscalePositive #-}
  pscalePositive = phoistAcyclic $ plam $ \x p ->
    pbls12_381_G1_scalarMul # pto p # x

-- | @since WIP
instance PAdditiveSemigroup PBuiltinBLS12_381_G2_Element where
  {-# INLINEABLE (#+) #-}
  x #+ y = pbls12_381_G2_add # x # y
  {-# INLINEABLE pscalePositive #-}
  pscalePositive = phoistAcyclic $ plam $ \x p ->
    pbls12_381_G2_scalarMul # pto p # x

{- | The notion of zero.

= Laws

1. @pzero #+ x@ @=@ @x@ (@pzero@ is the identity of @#+@)

@since WIP
-}
class PAdditiveSemigroup a => PAdditiveMonoid (a :: S -> Type) where
  pzero :: forall (s :: S). Term s a

-- | @since WIP
instance PAdditiveMonoid PInteger where
  {-# INLINEABLE pzero #-}
  pzero = pconstantInteger 0

-- | @since WIP
instance PAdditiveMonoid PBuiltinBLS12_381_G1_Element where
  {-# INLINEABLE pzero #-}
  pzero = pbls12_381_G1_uncompress # pbls12_381_G1_compressed_zero

-- | @since WIP
instance PAdditiveMonoid PBuiltinBLS12_381_G2_Element where
  {-# INLINEABLE pzero #-}
  pzero = pbls12_381_G2_uncompress # pbls12_381_G2_compressed_zero

{- | The notion of additive inverses, and the subtraction operation.

= Laws

If you define @pnegate@, the following laws must hold:

1. @(pnegate # x) #+ x@ @=@ @pzero@ (@pnegate@ is an additive inverse)
2. @pnegate #$ pnegate # x@ @=@ @x@ (@pnegate@ is self-inverting)

If you define @#-@, the following law must hold:

3. @x #- x@ @=@ @pzero@

Additionally, the following \'consistency laws\' must hold. Default
implementations of both @pnegate@ and @#-@ uphold these.

4. @pnegate # x@ @=@ @pzero #- x@
5. @x #- y@ @=@ @x #+ (pnegate # y)@

Lastly, if you define a custom @pscaleInteger@, the following laws
must hold:

6. @pscaleInteger # x # pzero@ @=@ @pzero@
7. @pscaleInteger # x #$ pnegate y@ @=@
   @pnegate #$ pscaleInteger # x # y@

@since WIP
-}
class PAdditiveMonoid a => PAdditiveGroup (a :: S -> Type) where
  {-# MINIMAL pnegate | (#-) #-}
  {-# INLINEABLE pnegate #-}
  pnegate :: forall (s :: S). Term s (a :--> a)
  pnegate = phoistAcyclic $ plam $ \x -> pzero #- x
  {-# INLINEABLE (#-) #-}
  (#-) :: forall (s :: S). Term s a -> Term s a -> Term s a
  x' #- y' = inner # x' # y'
    where
      inner :: forall (s' :: S). Term s' (a :--> a :--> a)
      inner = phoistAcyclic $ plam $ \x y ->
        x #+ (pnegate # y)
  {-# INLINEABLE pscaleInteger #-}
  pscaleInteger :: forall (s :: S). Term s (a :--> PInteger :--> a)
  pscaleInteger = phoistAcyclic $ plam $ \b e ->
    pif
      (e #== pzero)
      pzero
      ( pif
          (e #<= pzero)
          (pnegate #$ pscalePositive # b # punsafeDowncast (pnegate # e))
          (pscalePositive # b # punsafeDowncast e)
      )

-- | @since WIP
infix 6 #-

-- | @since WIP
instance PAdditiveGroup PInteger where
  {-# INLINEABLE (#-) #-}
  x #- y = psubtractInteger # x # y
  {-# INLINEABLE pscaleInteger #-}
  pscaleInteger = pmultiplyInteger

-- | @since WIP
instance PAdditiveGroup PBuiltinBLS12_381_G1_Element where
  {-# INLINEABLE pnegate #-}
  pnegate = pbls12_381_G1_neg
  {-# INLINEABLE pscaleInteger #-}
  pscaleInteger = phoistAcyclic $ plam $ \b e ->
    pbls12_381_G1_scalarMul # e # b

-- | @since WIP
instance PAdditiveGroup PBuiltinBLS12_381_G2_Element where
  {-# INLINEABLE pnegate #-}
  pnegate = pbls12_381_G2_neg
  {-# INLINEABLE pscaleInteger #-}
  pscaleInteger = phoistAcyclic $ plam $ \b e ->
    pbls12_381_G2_scalarMul # e # b

{- | Partial version of 'PPositive'. Errors if argument is zero.

@since WIP
-}
ptryPositive :: forall (s :: S). Term s (PInteger :--> PPositive)
ptryPositive = phoistAcyclic $
  plam $ \i ->
    pif
      (i #<= pzero)
      (ptraceInfo "ptryPositive: building with non positive" perror)
      (punsafeCoerce i)

-- | Build a 'PPositive' from a 'PInteger'. Yields 'PNothing' if argument is zero.
ppositive :: Term s (PInteger :--> PMaybe PPositive)
ppositive = phoistAcyclic $
  plam $ \i ->
    pif
      (i #<= pzero)
      (pcon PNothing)
      $ pcon . PJust . pcon
      $ PPositive i

{- | A default implementation of exponentiation-by-squaring with a
strictly-positive exponent.

= Important note

This implementation assumes that the operation argument is associative.

@since WIP
-}
pbySquaringDefault ::
  forall (a :: S -> Type) (s :: S).
  (forall (s' :: S). Term s' a -> Term s' a -> Term s' a) ->
  Term s (a :--> PPositive :--> a)
pbySquaringDefault f = phoistAcyclic $ pfix #$ plam $ \self b e ->
  -- We know that we can never have a value less than 1 for e, due to the type
  -- constraint. Thus, we make two assumptions:
  --
  -- 1. The stopping condition is equality to 1
  -- 2. The exponent is never negative
  --
  -- This allows us to use `pquot` and `prem` instead of `pdiv` and `pmod`,
  -- which is a bit faster.
  pif
    (pto e #== pconstantInteger 1)
    b
    ( plet (self # b #$ punsafeDowncast (pquotientInteger # pto e # pconstantInteger 2)) $ \below ->
        plet (f below below) $ \res ->
          pif
            ((premainderInteger # pto e # pconstantInteger 2) #== pconstantInteger 1)
            (f b res)
            res
    )
