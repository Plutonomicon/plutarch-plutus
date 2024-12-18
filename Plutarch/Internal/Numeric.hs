{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Internal.Numeric (
  -- * Types
  PPositive,
  Positive,
  PNatural,

  -- * Type classes
  PAdditiveSemigroup (..),
  PAdditiveMonoid (..),
  PAdditiveGroup (..),
  PMultiplicativeSemigroup (..),
  PMultiplicativeMonoid (..),
  PRing (..),
  PIntegralDomain (..),

  -- * Functions
  ptryPositive,
  ppositive,
  ptryNatural,
  pnatural,
  pbySquaringDefault,
  pdiv,
  pmod,
  pquot,
  prem,
) where

import Data.Coerce (coerce)
import Data.Kind (Type)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
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
import Plutarch.Builtin.Bool (pcond, pif)
import Plutarch.Builtin.Integer (
  PInteger,
  paddInteger,
  pconstantInteger,
  pmultiplyInteger,
  pquotientInteger,
  premainderInteger,
  psubtractInteger,
 )
import Plutarch.Internal.Eq (PEq ((#==)))
import Plutarch.Internal.Fix (pfix)
import Plutarch.Internal.IsData (PIsData)
import Plutarch.Internal.Lift (
  DeriveNewtypePLiftable,
  PLiftable (
    AsHaskell,
    PlutusRepr,
    fromPlutarch,
    fromPlutarchRepr,
    toPlutarch,
    toPlutarchRepr
  ),
  PLifted (PLifted),
  punsafeCoercePLifted,
 )
import Plutarch.Internal.Newtype (PlutusTypeNewtype)
import Plutarch.Internal.Ord (POrd ((#<=)))
import Plutarch.Internal.Other (pto)
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.PlutusType (
  DerivePlutusType (DPTStrat),
  PlutusType (PInner),
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
import Plutarch.Internal.Trace (ptraceInfo)
import Plutarch.Maybe (PMaybe (PJust, PNothing))
import Plutarch.Unsafe (punsafeDowncast)
import PlutusCore qualified as PLC
import Prettyprinter (Pretty)
import Test.QuickCheck (
  Arbitrary,
  CoArbitrary,
  Function,
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

-- | @since WIP
newtype PNatural (s :: S) = PNatural (Term s PInteger)
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
instance DerivePlutusType PNatural where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
instance PLiftable PNatural where
  type AsHaskell PNatural = Natural
  type PlutusRepr PNatural = Integer
  {-# INLINEABLE toPlutarchRepr #-}
  toPlutarchRepr = fromIntegral
  {-# INLINEABLE toPlutarch #-}
  toPlutarch = punsafeCoercePLifted . toPlutarch @PInteger . fromIntegral
  {-# INLINEABLE fromPlutarchRepr #-}
  fromPlutarchRepr x =
    if x < 0
      then Nothing
      else Just . fromIntegral $ x
  {-# INLINEABLE fromPlutarch #-}
  fromPlutarch t = fromIntegral <$> fromPlutarch @PInteger (punsafeCoercePLifted t)

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
instance PAdditiveSemigroup PNatural where
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

{- | The notion of zero, as well as a (kind of) reversal of 'pscalePositive',
similar to floor division by positive integers.

= Laws

1. @pzero #+ x@ @=@ @x@ (@pzero@ is the identity of @#+@)
2. @pscalePositive # pzero # n@ @=@
   @pzero@ (@pzero@ does not scale up)

@since WIP
-}
class PAdditiveSemigroup a => PAdditiveMonoid (a :: S -> Type) where
  pzero :: forall (s :: S). Term s a

-- | @since WIP
instance PAdditiveMonoid PInteger where
  {-# INLINEABLE pzero #-}
  pzero = pconstantInteger 0

-- | @since WIP
instance PAdditiveMonoid PNatural where
  {-# INLINEABLE pzero #-}
  pzero = punsafeCoerce . pconstantInteger $ 0

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

{- | The multiplication operation.

= Laws

1. @x #* (y #* z)@ @=@ @(x #* y) #* z@ (associativity of @#*@)

If you define a custom @ppowPositive@, ensure the following also hold:

3. @ppowPositive # x # pone@ @=@ @x@
4. @(ppowPositive # x # n) #* (ppowPositive # x # m)@ @=@
   @pscalePositive # x # (n #+ m)@
5. @ppowPositive # (ppowPositive # x # n) # m@ @=@
   @pscalePositive # x # (n #* m)@

The default implementation ensures these laws are satisfied.

= Note

Unlike 'PAdditiveSemigroup', the multiplication operation doesn't need to be
commutative. Currently, all Plutarch-provided instances are, but this need
not be true for other instances.

@since WIP
-}
class PMultiplicativeSemigroup (a :: S -> Type) where
  {-# INLINEABLE (#*) #-}
  (#*) :: forall (s :: S). Term s a -> Term s a -> Term s a
  default (#*) ::
    forall (s :: S).
    PMultiplicativeSemigroup (PInner a) =>
    Term s a ->
    Term s a ->
    Term s a
  x #* y = punsafeDowncast $ pto x #* pto y
  {-# INLINEABLE ppowPositive #-}
  ppowPositive ::
    forall (s :: S).
    Term s (a :--> PPositive :--> a)
  ppowPositive = phoistAcyclic $ plam $ \x p ->
    pbySquaringDefault (#*) # x # p

-- | @since WIP
infix 6 #*

-- | @since WIP
instance PMultiplicativeSemigroup PPositive where
  {-# INLINEABLE (#*) #-}
  x #* y = punsafeCoerce $ pmultiplyInteger # pto x # pto y

-- | @since WIP
instance PMultiplicativeSemigroup PNatural where
  {-# INLINEABLE (#*) #-}
  x #* y = punsafeCoerce $ pmultiplyInteger # pto x # pto y

-- | @since WIP
instance PMultiplicativeSemigroup PInteger where
  {-# INLINEABLE (#*) #-}
  x #* y = pmultiplyInteger # x # y

{- | The notion of one (multiplicative identity).

= Laws

1. @pone #* x@ @=@ @x@ (@pone@ is the left identity of @#*@)
2. @x #* pone@ @=@ @x@ (@pone@ is the right identity of @#*@)

@since WIP
-}
class PMultiplicativeSemigroup a => PMultiplicativeMonoid (a :: S -> Type) where
  pone :: forall (s :: S). Term s a

-- | @since WIP
instance PMultiplicativeMonoid PPositive where
  {-# INLINEABLE pone #-}
  pone = punsafeCoerce $ pconstantInteger 1

-- | @since WIP
instance PMultiplicativeMonoid PNatural where
  {-# INLINEABLE pone #-}
  pone = punsafeCoerce $ pconstantInteger 1

-- | @since WIP
instance PMultiplicativeMonoid PInteger where
  {-# INLINEABLE pone #-}
  pone = pconstantInteger 1

{- | Partial version of 'ppositive'. Errors if argument is not positive.

@since WIP
-}
ptryPositive :: forall (s :: S). Term s (PInteger :--> PPositive)
ptryPositive = phoistAcyclic $
  plam $ \i ->
    pif
      (i #<= pconstantInteger 0)
      (ptraceInfo "ptryPositive: building with non positive" perror)
      (punsafeCoerce i)

-- | Build a 'PPositive' from a 'PInteger'. Yields 'PNothing' if argument is not positive.
ppositive :: Term s (PInteger :--> PMaybe PPositive)
ppositive = phoistAcyclic $
  plam $ \i ->
    pif
      (i #<= pconstantInteger 0)
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

{- | = Laws

1. @pfromInteger 0@ @=@ @pzero@
2. @pfromInteger 1@ @=@ @pone@
3. @pfromInteger (x + y)@ @=@ @pfromInteger x #+ pfromInteger y@
4. @pfromInteger (x * y)@ @=@ @pfromInteger x #* pfromInteger y@

Additionally, the following \'interaction laws\' must hold between the
instances of 'PAdditiveGroup' and 'PMultiplicativeMonoid' for @a@:

5. @x #* (y #+ z)@ @=@ @(x #* y) #+ (x #* z)@ (@#*@ left-distributes over
   @#+@)
6. @(y #+ z) #* x@ @=@ @(y #* x) #+ (z #* x)@ (@#*@ right-distributes over
   @#+@)

@since WIP
-}
class
  (PAdditiveGroup a, PMultiplicativeMonoid a) =>
  PRing (a :: S -> Type)
  where
  pfromInteger :: forall (s :: S). Integer -> Term s a
  default pfromInteger :: forall (s :: S). PRing (PInner a) => Integer -> Term s a
  pfromInteger x = punsafeDowncast $ pfromInteger x

-- | @since WIP
instance PRing PInteger where
  {-# INLINEABLE pfromInteger #-}
  pfromInteger = pconstantInteger

{- | = Laws

= Pedantry note

Technically, the requirements here are too strong: we demand an /ordered/
ring, which integral domains don't necessarily have to be. However, in our
case, our hand is forced by expected semantics: in abstract algebra, both the
absolute value and the signum are real numbers (which are always totally
ordered) but in our case, both must be elements of the integral domain
itself. Thus, in order for the laws to make any sense, we have to ensure a
total order on the integral domain. Since all of our integral domains are
\'at least as big\' as the integers, this doesn't pose a huge problem.

@since WIP
-}
class (PRing a, POrd a) => PIntegralDomain (a :: S -> Type) where
  {-# INLINEABLE psignum #-}
  psignum :: forall (s :: S). Term s (a :--> a)
  default psignum :: forall (s :: S). Term s (a :--> a)
  psignum = phoistAcyclic $ plam $ \x ->
    pcond
      [ (x #== pzero, pzero)
      , (x #<= pzero, pnegate # pone)
      ]
      pone
  {-# INLINEABLE pabs #-}
  pabs :: forall (s :: S). Term s (a :--> a)
  default pabs :: forall (s :: S). Term s (a :--> a)
  pabs = phoistAcyclic $ plam $ \x ->
    pif
      (x #<= pzero)
      (pnegate # x)
      x

-- | @since WIP
instance PIntegralDomain PInteger where
  {-# INLINEABLE psignum #-}
  psignum = phoistAcyclic $ plam $ \x ->
    pcond
      [ (x #== pconstantInteger 0, pconstantInteger 0)
      , (x #<= pconstantInteger 0, pconstantInteger (-1))
      ]
      (pconstantInteger 1)

-- orphan instance, but only visibly orphan when importing internal modules
instance PIntegralDomain a => Num (Term s a) where
  {-# INLINEABLE (+) #-}
  (+) = (#+)
  {-# INLINEABLE (-) #-}
  (-) = (#-)
  {-# INLINEABLE (*) #-}
  (*) = (#*)
  {-# INLINEABLE abs #-}
  abs x = pabs # x
  {-# INLINEABLE negate #-}
  negate x = pnegate # x
  {-# INLINEABLE signum #-}
  signum x = psignum # x
  {-# INLINEABLE fromInteger #-}
  fromInteger = pfromInteger

-- | @since WIP
pdiv :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
pdiv = punsafeBuiltin PLC.DivideInteger

-- | @since WIP
pmod :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
pmod = punsafeBuiltin PLC.ModInteger

-- | @since WIP
pquot :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
pquot = punsafeBuiltin PLC.QuotientInteger

-- | @since WIP
prem :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
prem = punsafeBuiltin PLC.RemainderInteger

{- | Partial version of 'pnatural'. Errors if argument is negative.

@since WIP
-}
ptryNatural :: forall (s :: S). Term s (PInteger :--> PNatural)
ptryNatural = phoistAcyclic $ plam $ \i ->
  pif
    (i #<= pconstantInteger (-1))
    (ptraceInfo "ptryNatural: building with negative" perror)
    (punsafeCoerce i)

{- | Build a 'PNatural' from a 'PInteger'. Yields 'PNothing' if given a negative
value.

@since WIP
-}
pnatural :: forall (s :: S). Term s (PInteger :--> PMaybe PNatural)
pnatural = phoistAcyclic $ plam $ \i ->
  pif
    (i #<= pconstantInteger (-1))
    (pcon PNothing)
    (pcon . PJust . pcon . PNatural $ i)
