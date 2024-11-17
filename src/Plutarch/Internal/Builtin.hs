{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Types and functions that are integral to Plutarch, or primitive to UPLC.
module Plutarch.Internal.Builtin (
  -- * Types
  PString (..),
  PByteString (..),
  PBLS12_381_G1_Element (..),
  PBLS12_381_G2_Element (..),
  PBLS12_381_MlResult (..),

  -- * Functions

  -- ** PInteger

  -- ** PBool
  pnot,
  pand,
  por,
  pand',
  por',
  (#&&),
  (#||),
  pif,

  -- ** PString
  pbuiltinEqualsString,
  pbuiltinAppendString,
  pbuiltinEncodeUtf8,
  pbuiltinDecodeUtf8,

  -- ** PByteString
  pbuiltinAppendByteString,
  pbuiltinConsByteString,
  pbuiltinSliceByteString,
  pbuiltinLengthOfByteString,
  pbuiltinIndexByteString,
  pbuiltinEqualsByteString,
  pbuiltinLessThanByteString,
  pbuiltinLessThanEqualsByteString,

  -- ** BLS
  pbuiltinBls12_381_G1_add,
  pbuiltinBls12_381_G1_scalarMul,
  pbuiltinBls12_381_G1_neg,
  pbuiltinBls12_381_G1_compress,
  pbuiltinBls12_381_G1_uncompress,
  pbuiltinBls12_381_G1_hashToGroup,
  pbuiltinBls12_381_G1_equal,
  pbuiltinBls12_381_G2_add,
  pbuiltinBls12_381_G2_scalarMul,
  pbuiltinBls12_381_G2_neg,
  pbuiltinBls12_381_G2_compress,
  pbuiltinBls12_381_G2_uncompress,
  pbuiltinBls12_381_G2_hashToGroup,
  pbuiltinBls12_381_G2_equal,
  pbuiltinBls12_381_millerLoop,
  pbuiltinBls12_381_mulMlResult,
  pbuiltinBls12_381_finalVerify,

  -- ** Other
  pfix,
  pto,
  pbuiltinTrace,
  plam,
  pinl,
  punsafeDowncast,
) where

import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Exts (IsString (fromString))
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack, callStack, withFrozenCallStack)
import Plutarch.Builtin.Bool (PBool (PFalse, PTrue), pbuiltinIfThenElse)
import Plutarch.Builtin.Integer (PInteger)
import Plutarch.Builtin.Lift (
  DerivePConstantDirect (DerivePConstantDirect),
  PConstantDecl,
  PUnsafeLiftDecl (PLifted),
  pconstant,
 )
import Plutarch.Builtin.Opaque (POpaque)
import Plutarch.Internal.Newtype (PlutusTypeNewtype)
import Plutarch.Internal.PlutusType (
  DerivePlutusType (DPTStrat),
  PlutusType (PInner),
  pcon,
  pmatch,
 )
import Plutarch.Internal.PrettyStack (prettyStack)
import Plutarch.Internal.Term (
  Config (Tracing),
  PDelayed,
  S,
  Term,
  TracingMode (DoTracingAndBinds),
  pdelay,
  pforce,
  pgetConfig,
  phoistAcyclic,
  plam',
  punsafeBuiltin,
  punsafeCoerce,
  punsafeConstantInternal,
  (#),
  (:-->),
 )
import PlutusCore qualified as PLC
import PlutusCore.Crypto.BLS12_381.G1 qualified as BLS12_381_G1
import PlutusCore.Crypto.BLS12_381.G2 qualified as BLS12_381_G2
import PlutusCore.Crypto.BLS12_381.Pairing qualified as Pairing

{- | A Plutus string.

@since WIP
-}
newtype PString (s :: S) = PString (Term s POpaque)
  deriving stock
    ( -- | @since WIP
      Generic
    )
  deriving anyclass
    ( -- | @since WIP
      PlutusType
    )

-- | @since WIP
instance DerivePlutusType PString where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
instance PUnsafeLiftDecl PString where
  type PLifted PString = Text

-- | @since WIP
deriving via
  (DerivePConstantDirect Text PString)
  instance
    PConstantDecl Text

-- | @since WIP
instance IsString (Term s PString) where
  {-# INLINEABLE fromString #-}
  fromString = pconstant . Text.pack

-- | @since WIP
instance Semigroup (Term s PString) where
  {-# INLINEABLE (<>) #-}
  x <> y = pbuiltinAppendString # x # y

-- | @since WIP
instance Monoid (Term s PString) where
  {-# INLINEABLE mempty #-}
  mempty = pconstant ""

{- | A Plutus bytestring.

@since WIP
-}
newtype PByteString (s :: S) = PByteString (Term s POpaque)
  deriving stock
    ( -- | @since WIP
      Generic
    )
  deriving anyclass
    ( -- | @since WIP
      PlutusType
    )

-- | @since WIP
instance DerivePlutusType PByteString where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
instance PUnsafeLiftDecl PByteString where
  type PLifted PByteString = ByteString

-- | @since WIP
deriving via
  (DerivePConstantDirect ByteString PByteString)
  instance
    PConstantDecl ByteString

-- | @since WIP
instance Semigroup (Term s PByteString) where
  {-# INLINEABLE (<>) #-}
  x <> y = pbuiltinAppendByteString # x # y

-- | @since WIP
instance Monoid (Term s PByteString) where
  {-# INLINEABLE mempty #-}
  mempty = pconstant ""

{- | A point on the BLS12-381 G1 curve.

@since WIP
-}
newtype PBLS12_381_G1_Element (s :: S)
  = PBLS12_381_G1_Element (Term s POpaque)
  deriving stock
    ( -- | @since WIP
      Generic
    )
  deriving anyclass
    ( -- | @since WIP
      PlutusType
    )

-- | @since WIP
instance DerivePlutusType PBLS12_381_G1_Element where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
instance PUnsafeLiftDecl PBLS12_381_G1_Element where
  type PLifted PBLS12_381_G1_Element = BLS12_381_G1.Element

-- | @since WIP
deriving via
  (DerivePConstantDirect BLS12_381_G1.Element PBLS12_381_G1_Element)
  instance
    PConstantDecl BLS12_381_G1.Element

{- | A point on the BLS12-381 G2 curve.

@since WIP
-}
newtype PBLS12_381_G2_Element (s :: S)
  = PBLS12_381_G2_Element (Term s POpaque)
  deriving stock
    ( -- | @since WIP
      Generic
    )
  deriving anyclass
    ( -- | @since WIP
      PlutusType
    )

-- | @since WIP
instance DerivePlutusType PBLS12_381_G2_Element where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
instance PUnsafeLiftDecl PBLS12_381_G2_Element where
  type PLifted PBLS12_381_G2_Element = BLS12_381_G2.Element

-- | @since WIP
deriving via
  (DerivePConstantDirect BLS12_381_G2.Element PBLS12_381_G2_Element)
  instance
    PConstantDecl BLS12_381_G2.Element

{- | The result of a Miller loop in BLS12-381 pairing.

@since WIP
-}
newtype PBLS12_381_MlResult (s :: S)
  = PBLS12_381_MlResult (Term s POpaque)
  deriving stock
    ( -- | @since WIP
      Generic
    )
  deriving anyclass
    ( -- | @since WIP
      PlutusType
    )

-- | @since WIP
instance DerivePlutusType PBLS12_381_MlResult where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
instance PUnsafeLiftDecl PBLS12_381_MlResult where
  type PLifted PBLS12_381_MlResult = Pairing.MlResult

-- | @since WIP
deriving via
  (DerivePConstantDirect Pairing.MlResult PBLS12_381_MlResult)
  instance
    PConstantDecl Pairing.MlResult

{- |
  Fixpoint recursion. Used to encode recursive functions.

  Example:

  > iterateN' ::
  >  Term s (PInteger :--> (a :--> a) :--> a :--> a) ->
  >  Term s PInteger ->
  >  Term s (a :--> a) ->
  >  Term s a
  > iterateN' self n f x =
  >   pif (n #== 0) x (self # n - 1 #$ f x)
  >
  > iterateN :: Term s (PInteger :--> (a :--> a) :--> a :--> a)
  > iterateN = pfix #$ plam iterateN'
  >

  Further examples can be found in examples/Recursion.hs.

  @since WIP
-}
pfix :: Term s (((a :--> b) :--> a :--> b) :--> a :--> b)
pfix = phoistAcyclic $
  punsafeCoerce $
    plam' $ \f ->
      plam' (\(x :: Term s POpaque) -> f # plam' (\(v :: Term s POpaque) -> punsafeCoerce x # x # v))
        # punsafeCoerce (plam' $ \(x :: Term s POpaque) -> f # plam' (\(v :: Term s POpaque) -> punsafeCoerce x # x # v))

{- |
  Safely coerce from a Term to it's 'PInner' representation.

  @since WIP
-}
pto :: Term s a -> Term s (PInner a)
pto = punsafeCoerce

{- | A lazy if-then-else.

@since WIP
-}
pif ::
  forall (a :: S -> Type) (s :: S).
  Term s PBool ->
  Term s a ->
  Term s a ->
  Term s a
pif b ifTrue ifFalse = pmatch b $ \case
  PTrue -> ifTrue
  PFalse -> ifFalse

-- | @since WIP
pnot ::
  forall (s :: S).
  Term s (PBool :--> PBool)
pnot = phoistAcyclic $ plam $ \x ->
  pbuiltinIfThenElse # x # pcon PFalse # pcon PTrue

-- | @since WIP
pbuiltinTrace :: Term s (PString :--> a :--> a)
pbuiltinTrace = phoistAcyclic $ pforce $ punsafeBuiltin PLC.Trace

-- Helpers

class PLamN (a :: Type) (b :: S -> Type) (s :: S) | a -> b, s b -> a where
  -- | Lambda abstraction with any number of arguments.
  -- @since WIP
  plam ::
    forall (c :: S -> Type).
    HasCallStack =>
    (Term s c -> a) ->
    Term s (c :--> b)

instance {-# OVERLAPPABLE #-} a' ~ Term s a => PLamN a' a s where
  {-# INLINEABLE plam #-}
  plam f =
    let cs = callStack
     in plam' $ \x -> pgetConfig $ \case
          -- Note: This works because at the moment, DoTracingAndBinds is the most
          -- general tracing mode.
          Tracing _ DoTracingAndBinds ->
            pforce $ pbuiltinTrace # mkString (prettyStack "L" cs) # pdelay (f x)
          _ -> f x

mkString :: forall (a :: S -> Type) (s :: S). Text -> Term s a
mkString x = punsafeConstantInternal $ PLC.someValue @Text @PLC.DefaultUni x

instance (a' ~ Term s a, PLamN b' b s) => PLamN (a' -> b') (a :--> b) s where
  {-# INLINEABLE plam #-}
  plam f = withFrozenCallStack $ plam' $ \x -> plam (f x)

{- | Flipped application for terms.

@since WIP
-}
pinl ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s a ->
  (Term s a -> Term s b) ->
  Term s b
pinl v f = f v

{- | Lazy boolean and as a Plutarch function.

@since WIP
-}
pand ::
  forall (s :: S).
  Term s (PBool :--> PDelayed PBool :--> PDelayed PBool)
pand = phoistAcyclic $ plam $ \x y ->
  pbuiltinIfThenElse # x # y # phoistAcyclic (pdelay $ pcon PFalse)

{- | Lazy boolean and on 'Term's.

@since WIP
-}
(#&&) ::
  forall (s :: S).
  Term s PBool ->
  Term s PBool ->
  Term s PBool
x #&& y = pforce $ pand # x # pdelay y

infixr 3 #&&

{- | As 'pand', but strict.

@since WIP
-}
pand' ::
  forall (s :: S).
  Term s (PBool :--> PBool :--> PBool)
pand' = phoistAcyclic $ plam $ \x y ->
  pbuiltinIfThenElse # x # y # x

{- | Lazy boolean inclusive or as a Plutarch function.

@since WIP
-}
por ::
  forall (s :: S).
  Term s (PBool :--> PDelayed PBool :--> PDelayed PBool)
por = phoistAcyclic $ plam $ \x ->
  pbuiltinIfThenElse # x # phoistAcyclic (pdelay $ pcon PTrue)

{- | Lazy boolean inclusive or on 'Term's.

@since WIP
-}
(#||) ::
  forall (s :: S).
  Term s PBool ->
  Term s PBool ->
  Term s PBool
x #|| y = pforce $ por # x # pdelay y

infixr 2 #||

{- | As 'por', but strict.

@since WIP
-}
por' ::
  forall (s :: S).
  Term s (PBool :--> PBool :--> PBool)
por' = phoistAcyclic $ plam $ \x y ->
  pbuiltinIfThenElse # x # x # y

-- | @since WIP
pbuiltinEqualsString ::
  forall (s :: S).
  Term s (PString :--> PString :--> PBool)
pbuiltinEqualsString = punsafeBuiltin PLC.EqualsString

-- | @since WIP
pbuiltinAppendByteString ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PByteString)
pbuiltinAppendByteString = punsafeBuiltin PLC.AppendByteString

-- | @since WIP
pbuiltinConsByteString ::
  forall (s :: S).
  Term s (PInteger :--> PByteString :--> PByteString)
pbuiltinConsByteString = punsafeBuiltin PLC.ConsByteString

-- | @since WIP
pbuiltinSliceByteString ::
  forall (s :: S).
  Term s (PInteger :--> PInteger :--> PByteString :--> PByteString)
pbuiltinSliceByteString = punsafeBuiltin PLC.SliceByteString

-- | @since WIP
pbuiltinLengthOfByteString ::
  forall (s :: S).
  Term s (PByteString :--> PInteger)
pbuiltinLengthOfByteString = punsafeBuiltin PLC.LengthOfByteString

-- | @since WIP
pbuiltinIndexByteString ::
  forall (s :: S).
  Term s (PByteString :--> PInteger :--> PInteger)
pbuiltinIndexByteString = punsafeBuiltin PLC.IndexByteString

-- | @since WIP
pbuiltinEqualsByteString ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PBool)
pbuiltinEqualsByteString = punsafeBuiltin PLC.EqualsByteString

-- | @since WIP
pbuiltinLessThanByteString ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PBool)
pbuiltinLessThanByteString = punsafeBuiltin PLC.LessThanByteString

-- | @since WIP
pbuiltinLessThanEqualsByteString ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PBool)
pbuiltinLessThanEqualsByteString = punsafeBuiltin PLC.LessThanEqualsByteString

-- | @since WIP
pbuiltinAppendString ::
  forall (s :: S).
  Term s (PString :--> PString :--> PString)
pbuiltinAppendString = punsafeBuiltin PLC.AppendString

-- | @since WIP
pbuiltinEncodeUtf8 ::
  forall (s :: S).
  Term s (PString :--> PByteString)
pbuiltinEncodeUtf8 = punsafeBuiltin PLC.EncodeUtf8

-- | @since WIP
pbuiltinDecodeUtf8 ::
  forall (s :: S).
  Term s (PByteString :--> PString)
pbuiltinDecodeUtf8 = punsafeBuiltin PLC.DecodeUtf8

-- | @since WIP
punsafeDowncast ::
  forall (a :: S -> Type) (s :: S).
  Term s (PInner a) ->
  Term s a
punsafeDowncast = punsafeCoerce

-- | @since WIP
pbuiltinBls12_381_G1_add ::
  forall (s :: S).
  Term s (PBLS12_381_G1_Element :--> PBLS12_381_G1_Element :--> PBLS12_381_G1_Element)
pbuiltinBls12_381_G1_add = punsafeBuiltin PLC.Bls12_381_G1_add

-- | @since WIP
pbuiltinBls12_381_G1_scalarMul ::
  forall (s :: S).
  Term s (PInteger :--> PBLS12_381_G1_Element :--> PBLS12_381_G1_Element)
pbuiltinBls12_381_G1_scalarMul = punsafeBuiltin PLC.Bls12_381_G1_scalarMul

-- | @since WIP
pbuiltinBls12_381_G1_neg ::
  forall (s :: S).
  Term s (PBLS12_381_G1_Element :--> PBLS12_381_G1_Element)
pbuiltinBls12_381_G1_neg = punsafeBuiltin PLC.Bls12_381_G1_neg

-- | @since WIP
pbuiltinBls12_381_G1_compress ::
  forall (s :: S).
  Term s (PBLS12_381_G1_Element :--> PByteString)
pbuiltinBls12_381_G1_compress = punsafeBuiltin PLC.Bls12_381_G1_compress

-- | @since WIP
pbuiltinBls12_381_G1_uncompress ::
  forall (s :: S).
  Term s (PByteString :--> PBLS12_381_G1_Element)
pbuiltinBls12_381_G1_uncompress = punsafeBuiltin PLC.Bls12_381_G1_uncompress

-- | @since WIP
pbuiltinBls12_381_G1_hashToGroup ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PBLS12_381_G1_Element)
pbuiltinBls12_381_G1_hashToGroup = punsafeBuiltin PLC.Bls12_381_G1_hashToGroup

-- | @since WIP
pbuiltinBls12_381_G1_equal ::
  forall (s :: S).
  Term s (PBLS12_381_G1_Element :--> PBLS12_381_G1_Element :--> PBool)
pbuiltinBls12_381_G1_equal = punsafeBuiltin PLC.Bls12_381_G1_equal

-- | @since WIP
pbuiltinBls12_381_G2_add ::
  forall (s :: S).
  Term s (PBLS12_381_G2_Element :--> PBLS12_381_G2_Element :--> PBLS12_381_G2_Element)
pbuiltinBls12_381_G2_add = punsafeBuiltin PLC.Bls12_381_G2_add

-- | @since WIP
pbuiltinBls12_381_G2_scalarMul ::
  forall (s :: S).
  Term s (PInteger :--> PBLS12_381_G2_Element :--> PBLS12_381_G2_Element)
pbuiltinBls12_381_G2_scalarMul = punsafeBuiltin PLC.Bls12_381_G2_scalarMul

-- | @since WIP
pbuiltinBls12_381_G2_neg ::
  forall (s :: S).
  Term s (PBLS12_381_G2_Element :--> PBLS12_381_G2_Element)
pbuiltinBls12_381_G2_neg = punsafeBuiltin PLC.Bls12_381_G2_neg

-- | @since WIP
pbuiltinBls12_381_G2_compress ::
  forall (s :: S).
  Term s (PBLS12_381_G2_Element :--> PByteString)
pbuiltinBls12_381_G2_compress = punsafeBuiltin PLC.Bls12_381_G2_compress

-- | @since WIP
pbuiltinBls12_381_G2_uncompress ::
  forall (s :: S).
  Term s (PByteString :--> PBLS12_381_G2_Element)
pbuiltinBls12_381_G2_uncompress = punsafeBuiltin PLC.Bls12_381_G2_uncompress

-- | @since WIP
pbuiltinBls12_381_G2_hashToGroup ::
  forall (s :: S).
  Term s (PByteString :--> PByteString :--> PBLS12_381_G2_Element)
pbuiltinBls12_381_G2_hashToGroup = punsafeBuiltin PLC.Bls12_381_G2_hashToGroup

-- | @since WIP
pbuiltinBls12_381_G2_equal ::
  forall (s :: S).
  Term s (PBLS12_381_G2_Element :--> PBLS12_381_G2_Element :--> PBool)
pbuiltinBls12_381_G2_equal = punsafeBuiltin PLC.Bls12_381_G2_equal

-- | @since WIP
pbuiltinBls12_381_millerLoop ::
  forall (s :: S).
  Term s (PBLS12_381_G1_Element :--> PBLS12_381_G2_Element :--> PBLS12_381_MlResult)
pbuiltinBls12_381_millerLoop = punsafeBuiltin PLC.Bls12_381_millerLoop

-- | @since WIP
pbuiltinBls12_381_mulMlResult ::
  forall (s :: S).
  Term s (PBLS12_381_MlResult :--> PBLS12_381_MlResult :--> PBLS12_381_MlResult)
pbuiltinBls12_381_mulMlResult = punsafeBuiltin PLC.Bls12_381_mulMlResult

-- | @since WIP
pbuiltinBls12_381_finalVerify ::
  forall (s :: S).
  Term s (PBLS12_381_MlResult :--> PBLS12_381_MlResult :--> PBool)
pbuiltinBls12_381_finalVerify = punsafeBuiltin PLC.Bls12_381_finalVerify
