{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
-- Because of the weird way the PlutusType derivation mechanisms work, we lose
-- the PlutusType constraint. Kind of annoying, but we can't convince GHC
-- otherwise.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Plutarch.Internal.Lift (
  -- * Type class
  PLiftable (..),

  -- * Functions
  pconstant,
  plift,

  -- * Derivation

  -- ** Via-helpers
  DeriveBuiltinPLiftable (..),
  DeriveDataPLiftable (..),
  DeriveNewtypePLiftable (..),

  -- ** Manual instance helpers
  unsafeToUni,
  fromPlutarchUni,
  toPlutarchUni,
  fromPlutarchReprClosed,
  toPlutarchReprClosed,
  PLifted (PLifted),
  mkPLifted,
  getPLifted,
  PLiftedClosed (..),
  LiftError (..),
) where

import Plutarch.Builtin

import Data.ByteString (ByteString)
import Data.Coerce (Coercible, coerce)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word8)
import GHC.Generics (Generic)
import Plutarch.Internal.Evaluate (EvalError, evalScriptHuge)
import {-# SOURCE #-} Plutarch.Internal.IsData (PIsData)
import Plutarch.Internal.Newtype (PlutusTypeNewtype)
import Plutarch.Internal.PlutusType (DPTStrat, DerivePlutusType, PInner, PlutusType)
import Plutarch.Internal.Subtype (PSubtype)
import Plutarch.Internal.Term (
  Config (Tracing),
  LogLevel (LogInfo),
  S,
  Term,
  TracingMode (DoTracing),
  compile,
  punsafeConstantInternal,
 )
import Plutarch.Script (Script (Script))
import Plutarch.Unsafe (punsafeCoerce)
import PlutusCore qualified as PLC
import PlutusCore.Builtin (BuiltinError, readKnownConstant)
import PlutusCore.Crypto.BLS12_381.G1 qualified as BLS12_381.G1
import PlutusCore.Crypto.BLS12_381.G2 qualified as BLS12_381.G2
import PlutusCore.Crypto.BLS12_381.Pairing qualified as BLS12_381.Pairing
import PlutusTx qualified as PTx
import PlutusTx.Builtins.Internal (BuiltinByteString (BuiltinByteString), BuiltinData (BuiltinData))
import Universe (Includes)
import UntypedPlutusCore qualified as UPLC

{- | Used with 'fromPlutarch' methods to give additional information about why
evaluating a Plutarch term into a Haskell value went wrong.

@since WIP
-}
data LiftError
  = -- | Evaluation failed for some reason.
    CouldNotEvaluate EvalError
  | -- | We tried to use a builtin not part of the Plutus universe.
    TypeError BuiltinError
  | -- | Compiling the term into a script failed.
    CouldNotCompile Text
  | -- | @Data@ encoding was invalid for our type
    CouldNotDecodeData
  deriving stock
    ( -- | @since WIP
      Eq
    , -- | @since WIP
      Show
    )

{- | Indicates that the given Plutarch type has an equivalent in Haskell (and
Plutus by extension), and we have the ability to move between them.

= Important note

Calling methods of 'PLiftable' directly should rarely, if ever, be a
thing you do, unless defining your own instances without @via@-deriving
helpers (below). Prefer using 'pconstant' and 'plift', as these handle
some of the oddities required without you having to think about them.

You should rarely, if ever, need to define 'PLiftable' instances by hand.
Whenever possible, prefer using 'DeriveBuiltinPLiftable',
'DeriveDataPLiftable', and `DeriveNewtypePLiftable` as they have fewer
complexities and caveats. See their documentation for when to use them.

If you do want to define the methods yourself, there's a few key factors to
keep in mind:

1. You still shouldn't write every method by hand, there are helpers
   @fromPlutarch*@ and @toPlutarch*@ to cover common cases like types
   in Plutus universe or Scott encoding
2. If defining 'toPlutarchRepr' and 'fromPlutarchRepr' you will need to define
   an associated 'PlutusRepr' type, this is a Hasekll level type that is included
   in the Plutus default universe.
3. If defining 'toPlutarch' and 'fromPlutarch' for Scott encoded type you need to
   set @'PlutusRepr' PMyType = 'PLiftedClosed' PMyType@
4. When choosing a type for 'AsHaskell', /any/ value of that type /must/ be
   representable in Plutarch. If you have internal invariants to maintain on
   the Haskell side, make sure you do so with great care.

= Laws

1. @'fromPlutarchRepr' '.' 'toPlutarchRepr'@ @=@ @'Just'@
2. @'fmap' 'toPlutarchRepr' '.' 'fromPlutarchRepr'@ @=@ @'Just'@
3. @'fromPlutarch' '.' 'toPlutarch'@ @=@ @'Right'@
4. @'fmap' 'toPlutarch' '.' 'fromPlutarch'@ @=@ @'Right'@

Any derivations via 'DeriveBuiltinPLiftable', 'DeriveDataPLiftable', and
'DeriveNewtypePLiftable' automatically follow these laws.

@since WIP
-}
class PlutusType a => PLiftable (a :: S -> Type) where
  type AsHaskell a :: Type

  -- Implementation note: we need this second repr type because builtin
  -- containers like 'PBuiltinList' and 'PBuiltinPair' are not actually
  -- polymorphic. They can only hold types that are in 'DefaultUni'.
  -- Thus to convert e.g. a list to plutarch we first need to convert
  -- list elements to something that is in Plutus universe before it can
  -- be processed further
  type PlutusRepr a :: Type

  haskToRepr :: AsHaskell a -> PlutusRepr a
  reprToHask :: PlutusRepr a -> Maybe (AsHaskell a)

  reprToPlut :: PlutusRepr a -> PLifted s a
  plutToRepr :: (forall s. PLifted s a) -> Either LiftError (PlutusRepr a)

{- | Valid definition for 'toPlutarchRepr' if 'PlutusRepr' is Scott encoded

@since WIP
-}
toPlutarchReprClosed ::
  forall (a :: S -> Type).
  (PLiftable a, PlutusRepr a ~ PLiftedClosed a) =>
  AsHaskell a ->
  PlutusRepr a
toPlutarchReprClosed _p = undefined -- PLiftedClosed $ toPlutarch @a p

{- | Valid definition for 'fromPlutarchRepr' if 'PlutusRepr' is Scott encoded

@since WIP
-}
fromPlutarchReprClosed ::
  forall (a :: S -> Type).
  (PLiftable a, PlutusRepr a ~ PLiftedClosed a) =>
  PlutusRepr a ->
  Maybe (AsHaskell a)
fromPlutarchReprClosed (PLiftedClosed _t) = undefined -- either (const Nothing) Just $ fromPlutarch @a t

{- | Valid definition for 'toPlutarch' if 'PlutusRepr' is in Plutus universe

@since WIP
-}
toPlutarchUni ::
  forall (a :: S -> Type) (s :: S).
  (PLiftable a, PLC.DefaultUni `Includes` PlutusRepr a) =>
  PlutusRepr a ->
  PLifted s a
toPlutarchUni p =
  PLifted $ popaque $ punsafeCoerce $ punsafeConstantInternal $ PLC.someValue p

unsafeToUni ::
  forall (h :: Type) (a :: S -> Type) (s :: S).
  PLC.DefaultUni `Includes` h =>
  h ->
  PLifted s a
unsafeToUni x = PLifted $ popaque $ punsafeCoerce $ punsafeConstantInternal $ PLC.someValue x

{- | Valid definition for 'fromPlutarch' if 'PlutusRepr' is in Plutus universe

@since WIP
-}
fromPlutarchUni ::
  forall (a :: S -> Type).
  (PLiftable a, PLC.DefaultUni `Includes` PlutusRepr a) =>
  (forall s. PLifted s a) ->
  Either LiftError (PlutusRepr a)
fromPlutarchUni t =
  case compile (Tracing LogInfo DoTracing) $ unPLifted t of
    Left err -> Left . CouldNotCompile $ err
    Right compiled -> case evalScriptHuge compiled of
      (evaluated, _, _) -> case evaluated of
        Left err -> Left . CouldNotEvaluate $ err
        Right (Script (UPLC.Program _ _ term)) ->
          case readKnownConstant term of
            Left err -> Left . TypeError $ err
            Right res -> Right res

{- | Given a Haskell-level representation of a Plutarch term, transform it into
its equivalent term.

@since WIP
-}
pconstant ::
  forall (a :: S -> Type) (s :: S).
  PLiftable a =>
  AsHaskell a ->
  Term s a
pconstant = getPLifted . reprToPlut . haskToRepr @a

{- | Given a closed Plutarch term, compile and evaluate it, then produce the
corresponding Haskell value. If compilation or evaluation fails somehow, this
will call 'error': if you need to \'trap\' these outcomes and handle them
differently somehow, use 'fromPlutarch'.

@since WIP
-}
plift ::
  forall (a :: S -> Type).
  PLiftable a =>
  (forall (s :: S). Term s a) ->
  AsHaskell a
plift t = case reprToHask @a <$> (plutToRepr @a $ mkPLifted t) of
  Right Nothing -> error "failed to convert from representation to haskell type"
  Left err ->
    error $
      "plift failed: "
        <> ( case err of
              CouldNotEvaluate evalErr -> "term errored: " <> show evalErr
              TypeError builtinError -> "incorrect type: " <> show builtinError
              CouldNotCompile compErr -> "could not compile: " <> Text.unpack compErr
              CouldNotDecodeData -> "Data value is not a valid encoding for this type"
           )
  Right (Just res) -> res

{- | @via@-deriving helper, indicating that @a@ has a Haskell-level equivalent
@h@ that is directly part of the Plutus default universe (instead of by way
of a @Data@ encoding).

@since WIP
-}
newtype DeriveBuiltinPLiftable (a :: S -> Type) (h :: Type) (s :: S)
  = DeriveBuiltinPLiftable (a s)
  deriving stock (Generic)
  deriving anyclass (PlutusType)

-- | @since WIP
instance DerivePlutusType (DeriveBuiltinPLiftable a h) where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
instance
  ( PlutusType a
  , PLC.DefaultUni `Includes` h
  ) =>
  PLiftable (DeriveBuiltinPLiftable a h)
  where
  type AsHaskell (DeriveBuiltinPLiftable a h) = h
  type PlutusRepr (DeriveBuiltinPLiftable a h) = h

  {-# INLINEABLE haskToRepr #-}
  haskToRepr = id

  {-# INLINEABLE reprToHask #-}
  reprToHask = Just

  {-# INLINEABLE reprToPlut #-}
  reprToPlut = toPlutarchUni

  {-# INLINEABLE plutToRepr #-}
  plutToRepr = fromPlutarchUni

{- | @via@-deriving helper, indicating that @a@ has a Haskell-level equivalent
@h@ by way of its @Data@ encoding, rather than by @h@ being directly part of
the Plutus default universe.

@since WIP
-}
newtype DeriveDataPLiftable (a :: S -> Type) (h :: Type) (s :: S)
  = DeriveDataPLiftable (a s)
  deriving stock (Generic)
  deriving anyclass (PlutusType)

-- | @since WIP
instance DerivePlutusType (DeriveDataPLiftable a h) where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
instance
  ( PlutusType a
  , PSubtype PData a
  , PTx.ToData h
  , PTx.FromData h
  ) =>
  PLiftable (DeriveDataPLiftable a h)
  where
  type AsHaskell (DeriveDataPLiftable a h) = h
  type PlutusRepr (DeriveDataPLiftable a h) = PTx.Data

  {-# INLINEABLE haskToRepr #-}
  haskToRepr = PTx.toData

  {-# INLINEABLE reprToHask #-}
  reprToHask = PTx.fromData

  {-# INLINEABLE reprToPlut #-}
  reprToPlut = toPlutarchUni

  {-# INLINEABLE plutToRepr #-}
  plutToRepr = fromPlutarchUni

{- | @via@-deriving helper, indicating that @wrapper@ has a Haskell-level equivalent
@h@ by way of encoding of @inner@. It requires that @AsHaskell inner@ has the same
Haskell representation as @h@

@since WIP
-}
newtype DeriveNewtypePLiftable (wrapper :: S -> Type) (h :: Type) (s :: S)
  = DeriveNewtypePLiftable (wrapper s)
  deriving stock (Generic)
  deriving anyclass (PlutusType)

-- | @since WIP
instance DerivePlutusType (DeriveNewtypePLiftable w h) where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
instance (PLiftable (PInner w), Coercible (AsHaskell (PInner w)) h, PLC.DefaultUni `Includes` PlutusRepr (PInner w)) => PLiftable (DeriveNewtypePLiftable w h) where
  type AsHaskell (DeriveNewtypePLiftable w h) = h
  type PlutusRepr (DeriveNewtypePLiftable w h) = PlutusRepr (PInner w)

  {-# INLINEABLE haskToRepr #-}
  haskToRepr = haskToRepr @(PInner w) . coerce

  {-# INLINEABLE reprToHask #-}
  reprToHask = coerce . reprToHask @(PInner w)

  {-# INLINEABLE reprToPlut #-}
  reprToPlut = punsafeCoercePLifted . toPlutarchUni @(PInner w)

  {-# INLINEABLE plutToRepr #-}
  plutToRepr x = fromPlutarchUni @(PInner w) $ punsafeCoercePLifted x

{- | Similar to 'Identity', but at the level of Plutarch. Only needed when
writing manual instances of 'PLiftable', or if you want to use 'toPlutarch'
and 'fromPlutarch' directly.

This is used for coercing Plutarch terms in Haskell level with
`coerce :: PLifted s a -> PLifted s b` for @via@-deriving helpers

@since WIP
-}
type role PLifted nominal nominal

newtype PLifted (s :: S) (a :: S -> Type) = PLifted {unPLifted :: Term s POpaque}

-- | @since WIP
punsafeCoercePLifted :: PLifted s a -> PLifted s b
punsafeCoercePLifted (PLifted t) = PLifted t

-- | @since WIP
getPLifted :: PLifted s a -> Term s a
getPLifted (PLifted t) = punsafeCoerce t

-- | @since WIP
mkPLifted :: Term s a -> PLifted s a
mkPLifted t = PLifted (popaque t)

{- |  Use this as 'PlutusRepr' when defining 'PLiftable' instance for Scott encoded type

@since WIP
-}
newtype PLiftedClosed (a :: S -> Type) = PLiftedClosed {unPLiftedClosed :: forall (s :: S). Term s POpaque}

deriving via
  (DeriveBuiltinPLiftable PInteger Integer)
  instance
    PLiftable PInteger

-- | @since WIP
deriving via
  (DeriveBuiltinPLiftable PBool Bool)
  instance
    PLiftable PBool

-- instance {-# OVERLAPPING #-} PLiftable (PAsData PByteString) where
--   type AsHaskell (PAsData PByteString) = AsHaskell PByteString
--   type PlutusRepr (PAsData PByteString) = PTx.Data
--   {-# INLINEABLE haskToRepr #-}
--   haskToRepr = PTx.toData . BuiltinByteString
--   {-# INLINEABLE reprToHask #-}
--   reprToHask = toPlutarchUni
--   {-# INLINEABLE reprToPlut #-}
--   reprToPlut x = (\(BuiltinByteString str) -> str) <$> PTx.fromData x
--   {-# INLINEABLE plutToRepr #-}
--   plutToRepr = fromPlutarchUni

-- instance {-# OVERLAPPING #-} PLiftable (PAsData PData) where
--   type AsHaskell (PAsData PData) = AsHaskell PData
--   type PlutusRepr (PAsData PData) = PTx.Data
--   {-# INLINEABLE haskToRepr #-}
--   haskToRepr = PTx.toData . BuiltinData
--   {-# INLINEABLE reprToHask #-}
--   reprToHask = toPlutarchUni
--   {-# INLINEABLE reprToPlut #-}
--   reprToPlut x = (\(BuiltinData str) -> str) <$> PTx.fromData x
--   {-# INLINEABLE plutToRepr #-}
--   plutToRepr= fromPlutarchUni

instance (PTx.ToData (AsHaskell a), PTx.FromData (AsHaskell a), PIsData a) => PLiftable (PAsData a) where
  type AsHaskell (PAsData a) = AsHaskell a
  type PlutusRepr (PAsData a) = PTx.Data

  {-# INLINEABLE haskToRepr #-}
  haskToRepr = PTx.toData

  {-# INLINEABLE reprToHask #-}
  reprToHask = PTx.fromData

  {-# INLINEABLE reprToPlut #-}
  reprToPlut = toPlutarchUni

  {-# INLINEABLE plutToRepr #-}
  plutToRepr = fromPlutarchUni

-- | @since WIP
instance
  ( PLiftable a
  , PLC.Contains PLC.DefaultUni (PlutusRepr a)
  , PLiftable b
  , PLC.Contains PLC.DefaultUni (PlutusRepr b)
  ) =>
  PLiftable (PBuiltinPair a b)
  where
  type AsHaskell (PBuiltinPair a b) = (AsHaskell a, AsHaskell b)
  type PlutusRepr (PBuiltinPair a b) = (PlutusRepr a, PlutusRepr b)

  {-# INLINEABLE haskToRepr #-}
  haskToRepr (a, b) = (haskToRepr @a a, haskToRepr @b b)

  {-# INLINEABLE reprToHask #-}
  reprToHask (a, b) = (,) <$> reprToHask @a a <*> reprToHask @b b

  {-# INLINEABLE reprToPlut #-}
  reprToPlut = toPlutarchUni

  {-# INLINEABLE plutToRepr #-}
  plutToRepr = fromPlutarchUni

-- | @since WIP
instance (PLiftable a, PLC.Contains PLC.DefaultUni (PlutusRepr a)) => PLiftable (PBuiltinList a) where
  type AsHaskell (PBuiltinList a) = [AsHaskell a]
  type PlutusRepr (PBuiltinList a) = [PlutusRepr a]

  {-# INLINEABLE haskToRepr #-}
  haskToRepr = map (haskToRepr @a)

  {-# INLINEABLE reprToHask #-}
  reprToHask = traverse (reprToHask @a)

  {-# INLINEABLE reprToPlut #-}
  reprToPlut = toPlutarchUni

  {-# INLINEABLE plutToRepr #-}
  plutToRepr = fromPlutarchUni

{- | @since WIP
deriving via
  (DeriveBuiltinPLiftable PByteString ByteString)
  instance
    PLiftable PByteString
-}
instance PLiftable PByteString where
  type AsHaskell PByteString = BuiltinByteString
  type PlutusRepr PByteString = ByteString

  {-# INLINEABLE haskToRepr #-}
  haskToRepr (BuiltinByteString str) = str

  {-# INLINEABLE reprToHask #-}
  reprToHask = Just . BuiltinByteString

  {-# INLINEABLE reprToPlut #-}
  reprToPlut = toPlutarchUni

  {-# INLINEABLE plutToRepr #-}
  plutToRepr = fromPlutarchUni

instance PLiftable PData where
  type AsHaskell PData = BuiltinData
  type PlutusRepr PData = PTx.Data

  {-# INLINEABLE haskToRepr #-}
  haskToRepr (BuiltinData str) = str

  {-# INLINEABLE reprToHask #-}
  reprToHask = Just . BuiltinData

  {-# INLINEABLE reprToPlut #-}
  reprToPlut = toPlutarchUni

  {-# INLINEABLE plutToRepr #-}
  plutToRepr = fromPlutarchUni

-- | @since WIP
instance PLiftable PByte where
  type AsHaskell PByte = Word8
  type PlutusRepr PByte = Integer

  {-# INLINEABLE haskToRepr #-}
  haskToRepr = fromIntegral @_ @Integer

  {-# INLINEABLE reprToHask #-}
  reprToHask x = fromIntegral <$> reprToHask @PInteger x

  {-# INLINEABLE reprToPlut #-}
  reprToPlut = toPlutarchUni

  {-# INLINEABLE plutToRepr #-}
  plutToRepr = fromPlutarchUni

-- | @since WIP
deriving via
  (DeriveBuiltinPLiftable PUnit ())
  instance
    PLiftable PUnit

deriving via
  (DeriveBuiltinPLiftable PString Text)
  instance
    PLiftable PString

-- | @since WIP
deriving via
  (DeriveBuiltinPLiftable PBuiltinBLS12_381_G1_Element BLS12_381.G1.Element)
  instance
    PLiftable PBuiltinBLS12_381_G1_Element

-- | @since WIP
deriving via
  (DeriveBuiltinPLiftable PBuiltinBLS12_381_G2_Element BLS12_381.G2.Element)
  instance
    PLiftable PBuiltinBLS12_381_G2_Element

-- | @since WIP
deriving via
  (DeriveBuiltinPLiftable PBuiltinBLS12_381_MlResult BLS12_381.Pairing.MlResult)
  instance
    PLiftable PBuiltinBLS12_381_MlResult
