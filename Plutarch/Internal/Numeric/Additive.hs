{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Internal.Numeric.Additive (
  -- * Types
  PPositive,
  Positive,

  -- * Type classes
  PAdditiveSemigroup (..),
  PAdditiveMonoid (..),
  PAdditiveGroup (..),
  PAbs (..),

  -- * Functions
  ptryPositive,
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
 )
import Plutarch.Internal.Term (
  S,
  Term,
  perror,
  phoistAcyclic,
  punsafeBuiltin,
  punsafeCoerce,
  (#),
  (#$),
  (:-->),
 )
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

{- | = Laws

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
  pscalePositive ::
    forall (s :: S).
    Term s (a :--> PPositive :--> a)
  pscalePositive = phoistAcyclic $ plam $ \x p ->
    go # x # x # pto p
    where
      go ::
        forall (s' :: S).
        Term s' (a :--> a :--> PInteger :--> a)
      go = phoistAcyclic $ pfix #$ plam $ \self original acc step ->
        pif
          (step #== pconstantInteger 1)
          acc
          (self # original # (acc #+ original) #$ psubtractInteger # step # pconstantInteger 1)

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

-- TODO: PRational

{- | = Laws

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

-- TODO: PRational

{- | = Laws

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

-- | @since WIP
infix 6 #-

-- | @since WIP
instance PAdditiveGroup PInteger where
  {-# INLINEABLE (#-) #-}
  x #- y = psubtractInteger # x # y

-- | @since WIP
instance PAdditiveGroup PBuiltinBLS12_381_G1_Element where
  {-# INLINEABLE pnegate #-}
  pnegate = pbls12_381_G1_neg

-- | @since WIP
instance PAdditiveGroup PBuiltinBLS12_381_G2_Element where
  {-# INLINEABLE pnegate #-}
  pnegate = pbls12_381_G2_neg

-- TODO: PRational

{- | = Laws

= Note

This functionality really should be in 'PAdditiveGroup'. However, BLS12-381
elements are drawn from a finite field, which by definition doesn't admit a
(linear) ordering, and no primitive we have can distinguish a positive
element from a negative one. Thus, we have to separate this functionality
into its own type class.

@since WIP
-}
class PAdditiveGroup a => PAbs (a :: S -> Type) where
  pabs :: forall (s :: S). Term s (a :--> a)
  default pabs :: forall (s :: S). POrd a => Term s (a :--> a)
  pabs = phoistAcyclic $ plam $ \x ->
    pif (x #<= pzero) (pnegate # x) x

-- | @since WIP
instance PAbs PInteger

-- TODO: PRational

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
