{-# LANGUAGE AllowAmbiguousTypes #-}
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

import Data.Coerce (Coercible, coerce)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import {-# SOURCE #-} Plutarch.Builtin (PData)
import Plutarch.Internal (
  Config (Tracing),
  LogLevel (LogInfo),
  S,
  Term,
  TracingMode (DoTracing),
  compile,
  punsafeConstantInternal,
 )
import Plutarch.Internal.Evaluate (EvalError, evalScriptHuge)
import Plutarch.Internal.Newtype (PlutusTypeNewtype)
import Plutarch.Internal.Other (POpaque, popaque)
import Plutarch.Internal.PlutusType (DPTStrat, DerivePlutusType, PlutusType)
import Plutarch.Script (Script (Script))
import Plutarch.TryFrom (PSubtype)
import Plutarch.Unsafe (punsafeCoerce)
import PlutusCore qualified as PLC
import PlutusCore.Builtin (BuiltinError, readKnownConstant)
import PlutusTx qualified as PTx
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

  toPlutarchRepr :: AsHaskell a -> PlutusRepr a
  toPlutarch :: AsHaskell a -> PLifted s a
  fromPlutarchRepr :: PlutusRepr a -> Maybe (AsHaskell a)
  fromPlutarch :: (forall s. PLifted s a) -> Either LiftError (AsHaskell a)

{- | Valid definition for 'toPlutarchRepr' if 'PlutusRepr' is Scott encoded

@since WIP
-}
toPlutarchReprClosed ::
  forall (a :: S -> Type).
  (PLiftable a, PlutusRepr a ~ PLiftedClosed a) =>
  AsHaskell a ->
  PlutusRepr a
toPlutarchReprClosed p = PLiftedClosed $ toPlutarch @a p

{- | Valid definition for 'fromPlutarchRepr' if 'PlutusRepr' is Scott encoded

@since WIP
-}
fromPlutarchReprClosed ::
  forall (a :: S -> Type).
  (PLiftable a, PlutusRepr a ~ PLiftedClosed a) =>
  PlutusRepr a ->
  Maybe (AsHaskell a)
fromPlutarchReprClosed (PLiftedClosed t) = either (const Nothing) Just $ fromPlutarch @a t

{- | Valid definition for 'toPlutarch' if 'PlutusRepr' is in Plutus universe

@since WIP
-}
toPlutarchUni ::
  forall (a :: S -> Type) (s :: S).
  (PLiftable a, PLC.DefaultUni `Includes` PlutusRepr a) =>
  AsHaskell a ->
  PLifted s a
toPlutarchUni p =
  PLifted $ popaque $ punsafeCoerce $ punsafeConstantInternal $ PLC.someValue $ toPlutarchRepr @a p

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
  Either LiftError (AsHaskell a)
fromPlutarchUni t =
  case compile (Tracing LogInfo DoTracing) $ unPLifted t of
    Left err -> Left . CouldNotCompile $ err
    Right compiled -> case evalScriptHuge compiled of
      (evaluated, _, _) -> case evaluated of
        Left err -> Left . CouldNotEvaluate $ err
        Right (Script (UPLC.Program _ _ term)) -> case readKnownConstant term of
          Left err -> Left . TypeError $ err
          Right res -> maybe (Left CouldNotDecodeData) Right $ fromPlutarchRepr @a res

{- | Given a Haskell-level representation of a Plutarch term, transform it into
its equivalent term.

@since WIP
-}
pconstant ::
  forall (a :: S -> Type) (s :: S).
  PLiftable a =>
  AsHaskell a ->
  Term s a
pconstant = getPLifted . toPlutarch @a

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
plift t = case fromPlutarch @a $ mkPLifted t of
  Left err ->
    error $
      "plift failed: "
        <> ( case err of
              CouldNotEvaluate evalErr -> "term errored: " <> show evalErr
              TypeError builtinError -> "incorrect type: " <> show builtinError
              CouldNotCompile compErr -> "could not compile: " <> Text.unpack compErr
              CouldNotDecodeData -> "Data value is not a valid encoding for this type"
           )
  Right res -> res

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

  {-# INLINEABLE toPlutarchRepr #-}
  toPlutarchRepr = id

  {-# INLINEABLE toPlutarch #-}
  toPlutarch = toPlutarchUni

  {-# INLINEABLE fromPlutarchRepr #-}
  fromPlutarchRepr = Just

  {-# INLINEABLE fromPlutarch #-}
  fromPlutarch = fromPlutarchUni

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

  {-# INLINEABLE toPlutarchRepr #-}
  toPlutarchRepr = PTx.toData

  {-# INLINEABLE toPlutarch #-}
  toPlutarch = toPlutarchUni

  {-# INLINEABLE fromPlutarchRepr #-}
  fromPlutarchRepr = PTx.fromData

  {-# INLINEABLE fromPlutarch #-}
  fromPlutarch = fromPlutarchUni

{- | @via@-deriving helper, indicating that @wrapper@ has a Haskell-level equivalent
@h@ by way of encoding of @inner@. It requires that @AsHaskell inner@ has the same
Haskell representation as @h@

@since WIP
-}
newtype DeriveNewtypePLiftable (wrapper :: S -> Type) (inner :: S -> Type) (h :: Type) (s :: S)
  = DeriveNewtypePLiftable (wrapper s)
  deriving stock (Generic)
  deriving anyclass (PlutusType)

-- | @since WIP
instance DerivePlutusType (DeriveNewtypePLiftable w i h) where
  type DPTStrat _ = PlutusTypeNewtype

-- | @since WIP
instance (PLiftable inner, Coercible (AsHaskell inner) h) => PLiftable (DeriveNewtypePLiftable wrapper inner h) where
  type AsHaskell (DeriveNewtypePLiftable wrapper inner h) = h
  type PlutusRepr (DeriveNewtypePLiftable wrapper inner h) = PlutusRepr inner

  {-# INLINEABLE toPlutarchRepr #-}
  toPlutarchRepr = toPlutarchRepr @inner . coerce @h @(AsHaskell inner)

  {-# INLINEABLE toPlutarch #-}
  toPlutarch = punsafeCoercePLifted . toPlutarch @inner . coerce @h @(AsHaskell inner)

  {-# INLINEABLE fromPlutarchRepr #-}
  fromPlutarchRepr = coerce . fromPlutarchRepr @inner

  {-# INLINEABLE fromPlutarch #-}
  fromPlutarch p = coerce . fromPlutarch @inner $ punsafeCoercePLifted p

{- | Similar to 'Identity', but at the level of Plutarch. Only needed when
writing manual instances of 'PLiftable', or if you want to use 'toPlutarch'
and 'fromPlutarch' directly.

This is used for coercing Plutarch terms in Haskell level with
`coerce :: PLifted s a -> PLifted s b` for @via@-deriving helpers

@since WIP
-}
type role PLifted nominal nominal

newtype PLifted s a = PLifted {unPLifted :: Term s POpaque}

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
newtype PLiftedClosed (a :: S -> Type) = PLiftedClosed {unPLiftedClosed :: forall (s :: S). PLifted s a}
