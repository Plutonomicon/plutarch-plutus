{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Internal.Lift (
  -- * Error type
  LiftError (..),

  -- * Type class
  PLiftable (..),

  -- * Functions
  pconstant,
  plift,

  -- * Deriving vias
  DeriveBuiltinPLiftable,
  DeriveDataPLiftable,
) where

import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Plutarch.Builtin (PData)
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
import Plutarch.Internal.Other (POpaque)
import Plutarch.Internal.PlutusType (DPTStrat, DerivePlutusType, PInner, PlutusType)
import Plutarch.Internal.Witness (witness)
import Plutarch.Script (Script (Script))
import Plutarch.Unsafe (punsafeCoerce)
import PlutusCore qualified as PLC
import PlutusCore.Builtin (BuiltinError, readKnownConstant)
import PlutusCore.Data qualified as PLCData
import PlutusTx qualified as PTx
import PlutusTx.IsData.Class (unsafeFromData)
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
  deriving stock (Eq)

{- | Indicates that the given Plutarch type has an equivalent in Haskell (and
Plutus by extension), and we have the ability to move between them.

= Important note

You should rarely, if ever, need to define an instance of this type class
manually. In general, for any given Plutarch type, there are two
possibilities:

* The type is backed by a builtin Plutus type (for example, @PInteger@ is
  backed by @Integer@); or
* The type is backed by a @Data@ encoded type (for example, @PScriptContext@
  is backed by @ScriptContext@).

To that end, we provide some helpers to allow a \'cookbook\' definition of
both kinds of instance. For something backed by a builtin Plutus type (like
@PInteger@), the instance would look like this:

@
instance PLiftable PInteger where
   type AsHaskell PInteger = Integer
   toPlutarch = toPlutarchDefaultUni
   fromPlutarch = fromPlutarchDefaultUni
@

For something backed by a @Data@ encoding (like @PScriptContext@), the
instance would look like this instead:

@
instance PLiftable PScriptContext where
   type AsHaskell PScriptContext = ScriptContext
   toPlutarch = toPlutarchData
   fromPlutarch = fromPlutarchData
@

= Laws

1. @'fromPlutarch' '.' 'toPlutarch'@ @=@ @'Right'@
2. @'fmap' 'toPlutarch' '.' 'fromPlutarch'@ @=@ @'Right'@

Both of the \'cookbook\' methods described above automatically satisfy these
laws.

@since WIP
-}
newtype PLifted a s = PLifted (Term s POpaque)

unPLifted :: PLifted a s -> Term s a
unPLifted (PLifted t) = punsafeCoerce t

class PlutusType a => PLiftable (a :: S -> Type) where
  type AsHaskell a :: Type
  toPlutarch :: forall (s :: S). AsHaskell a -> PLifted a s
  fromPlutarch :: (forall (s :: S). PLifted a s) -> Either LiftError (AsHaskell a)

{- | Backwards-compatible synonym for 'toPlutarch'.

@since WIP
-}
pconstant ::
  forall (a :: S -> Type) (s :: S).
  PLiftable a =>
  AsHaskell a ->
  Term s a
pconstant = punsafeCoerce . unPLifted . toPlutarch @a

{- | Backwards-compatible functionality similar to 'fromPlutarch', but
transforms any 'LiftError' into an error message.

@since WIP
-}
plift ::
  forall (a :: S -> Type).
  PLiftable a =>
  (forall (s :: S). Term s a) ->
  AsHaskell a
plift t = case fromPlutarch (PLifted @a (punsafeCoerce t)) of
  Left err ->
    error $
      "plift failed: "
        <> ( case err of
              CouldNotEvaluate evalErr -> "term errored: " <> show evalErr
              TypeError builtinError -> "incorrect type: " <> show builtinError
              CouldNotCompile compErr -> "could not compile: " <> Text.unpack compErr
           )
  Right res -> res

newtype DeriveBuiltinPLiftable (a :: S -> Type) (h :: Type) (s :: S) = DeriveBuiltinPLiftable (a s)
  deriving stock (Generic)
  deriving anyclass (PlutusType)

instance DerivePlutusType (DeriveBuiltinPLiftable a h) where type DPTStrat _ = PlutusTypeNewtype

instance
  ( PlutusType a
  , PLC.DefaultUni `Includes` h
  ) =>
  PLiftable (DeriveBuiltinPLiftable a h)
  where
  type AsHaskell (DeriveBuiltinPLiftable a h) = h
  toPlutarch =
    let _ = witness (Proxy @(PlutusType a))
     in PLifted . punsafeConstantInternal . PLC.someValue @h @PLC.DefaultUni
  fromPlutarch t =
    case compile (Tracing LogInfo DoTracing) (unPLifted t) of
      Left err -> Left . CouldNotCompile $ err
      Right compiled -> case evalScriptHuge compiled of
        (evaluated, _, _) -> case evaluated of
          Left err -> Left . CouldNotEvaluate $ err
          Right (Script (UPLC.Program _ _ term)) -> case readKnownConstant term of
            Left err -> Left . TypeError $ err
            Right res -> pure res

newtype DeriveDataPLiftable (a :: S -> Type) (h :: Type) (s :: S) = DeriveDataPLiftable (a s)
  deriving stock (Generic)
  deriving anyclass (PlutusType)

instance DerivePlutusType (DeriveDataPLiftable a h) where type DPTStrat _ = PlutusTypeNewtype

-- Technically can use `PLiftable` instance of PData to remove duplication
instance
  ( PlutusType a
  , PInner a ~ PData
  , PTx.ToData h
  , PTx.UnsafeFromData h
  , PLC.DefaultUni `Includes` h
  ) =>
  PLiftable (DeriveDataPLiftable a h)
  where
  type AsHaskell (DeriveDataPLiftable a h) = h
  toPlutarch =
    let _ = witness (Proxy @(PlutusType a))
     in PLifted . punsafeConstantInternal . PLC.someValue @PLCData.Data @PLC.DefaultUni . PTx.toData
  fromPlutarch t =
    case compile (Tracing LogInfo DoTracing) (unPLifted t) of
      Left err -> Left . CouldNotCompile $ err
      Right compiled -> case evalScriptHuge compiled of
        (evaluated, _, _) -> case evaluated of
          Left err -> Left . CouldNotEvaluate $ err
          Right (Script (UPLC.Program _ _ term)) -> case readKnownConstant term of
            Left err -> Left . TypeError $ err
            Right res -> pure . unsafeFromData $ res

---------------------------------------------------------------------- Little tests

-- data MyPInteger (s :: S) =
--   MyPInteger (Term s POpaque)
--   deriving stock (Generic)
--   deriving anyclass (PlutusType)

-- instance DerivePlutusType MyPInteger where type DPTStrat _ = PlutusTypeNewtype

-- deriving via DeriveBuiltinPLiftable MyPInteger Integer instance PLiftable MyPInteger

-- deriving via DeriveBuiltinPLiftable PInteger Integer instance PLiftable PInteger
-- deriving via DeriveDataPLiftable (PAsData PInteger) Integer instance PLiftable (PAsData PInteger)
