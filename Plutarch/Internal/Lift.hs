{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Internal.Lift (
  -- * Error type
  LiftError (..),

  -- * Type class
  PLiftable (..),

  -- * Functions
  pconstant,
  plift,

  -- * Derivation helpers
  toPlutarchDefaultUni,
  fromPlutarchDefaultUni,
  toPlutarchData,
  fromPlutarchData,
) where

import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text
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
import Plutarch.Internal.PlutusType (PlutusType)
import Plutarch.Script (Script (Script))
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
class PlutusType a => PLiftable (a :: S -> Type) where
  type AsHaskell a :: Type
  toPlutarch :: forall (s :: S). AsHaskell a -> Term s a
  fromPlutarch :: (forall (s :: S). Term s a) -> Either LiftError (AsHaskell a)

{- | Backwards-compatible synonym for 'toPlutarch'.

@since WIP
-}
pconstant ::
  forall (a :: S -> Type) (s :: S).
  PLiftable a =>
  AsHaskell a ->
  Term s a
pconstant = toPlutarch

{- | Backwards-compatible functionality similar to 'fromPlutarch', but
transforms any 'LiftError' into an error message.

@since WIP
-}
plift ::
  forall (a :: S -> Type).
  PLiftable a =>
  (forall (s :: S). Term s a) ->
  AsHaskell a
plift t = case fromPlutarch t of
  Left err ->
    error $
      "plift failed: "
        <> ( case err of
              CouldNotEvaluate evalErr -> "term errored: " <> show evalErr
              TypeError builtinError -> "incorrect type: " <> show builtinError
              CouldNotCompile compErr -> "could not compile: " <> Text.unpack compErr
           )
  Right res -> res

{- | \'Cookbook\' definition of 'toPlutarch' for a type whose Haskell equivalent
is part of the Plutus universe. Use together with 'fromPlutarchDefaultUni' to
define an instance.

@since WIP
-}
toPlutarchDefaultUni ::
  forall (a :: S -> Type) (r :: Type) (s :: S).
  PLC.DefaultUni `Includes` r =>
  r ->
  Term s a
toPlutarchDefaultUni x = punsafeConstantInternal $ PLC.someValue @r @PLC.DefaultUni $ x

{- | \'Cookbook\' definition of 'fromPlutarch' for a type whose Haskell
equivalent is part of the Plutus universe. Use together with
'toPlutarchDefaultUni' to define an instance.

@since WIP
-}
fromPlutarchDefaultUni ::
  forall (a :: S -> Type) (r :: Type).
  PLC.DefaultUni `Includes` r =>
  (forall (s :: S). Term s a) ->
  Either LiftError r
fromPlutarchDefaultUni t = case compile (Tracing LogInfo DoTracing) t of
  Left err -> Left . CouldNotCompile $ err
  Right compiled -> case evalScriptHuge compiled of
    (evaluated, _, _) -> case evaluated of
      Left err -> Left . CouldNotEvaluate $ err
      Right (Script (UPLC.Program _ _ term)) -> case readKnownConstant term of
        Left err -> Left . TypeError $ err
        Right res -> pure res

{- | \'Cookbook\' definition of 'toPlutarch' for a type whose Haskell equivalent
is @Data@ encoded. Use together with 'fromPlutarchData' to define an
instance.

@since WIP
-}
toPlutarchData ::
  forall (a :: S -> Type) (r :: Type) (s :: S).
  PTx.ToData r =>
  r ->
  Term s a
toPlutarchData x =
  punsafeConstantInternal $ PLC.someValue @PLCData.Data @PLC.DefaultUni $ PTx.toData x

{- | \'Cookbook\' definition of 'fromPlutarch' for a type whose Haskell
equivalent is @Data@ encoded. Use together with 'toPlutarchData' to define an
instance.

@since WIP
-}
fromPlutarchData ::
  forall (a :: S -> Type) (r :: Type).
  PTx.UnsafeFromData r =>
  (forall (s :: S). Term s a) ->
  Either LiftError r
fromPlutarchData t = case compile (Tracing LogInfo DoTracing) t of
  Left err -> Left . CouldNotCompile $ err
  Right compiled -> case evalScriptHuge compiled of
    (evaluated, _, _) -> case evaluated of
      Left err -> Left . CouldNotEvaluate $ err
      Right (Script (UPLC.Program _ _ term)) -> case readKnownConstant term of
        Left err -> Left . TypeError $ err
        Right res -> pure . unsafeFromData $ res
