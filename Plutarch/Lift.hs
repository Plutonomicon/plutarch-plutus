{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{- |
Module: Plutarch.Lift
Description: Conversion to and from Plutarch terms and Haskell types

This module defines functions, associated type families, and newtypes for use with
[@DerivingVia@](https://ryanglscott.github.io/papers/deriving-via.pdf) to allow
Plutarch to convert to and from PTypes and Haskell types.
-}
module Plutarch.Lift (
  -- * Converstion between Plutarch terms and Haskell types
  pconstant,
  plift,
  plift',  
  pliftTrace,
  LiftError (..),

  -- * Define your own conversion
  PConstantDecl (..),
  PLift,
  PConstant,
  DerivePConstantDirect (..),
  DerivePConstantViaNewtype (..),
  DerivePConstantViaBuiltin (..),

  -- * Internal use
  PUnsafeLiftDecl (..),
) where

import Control.Lens ((^?))
import Data.Coerce (Coercible, coerce)
import Data.Kind (Constraint, Type)
import Data.Text (Text)
import GHC.Stack (HasCallStack)
import Plutarch.Evaluate (EvalError)
import Plutarch.Internal (
  ClosedTerm,
  CompiledTerm (..),
  Config (Config, tracingMode),
  PType,
  S,
  Term,
  TracingMode (DoTracing),
  compileTerm',
  evalCompiled,
  punsafeConstantInternal,
 )
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget)    
import qualified PlutusCore as PLC
import PlutusCore.Builtin (KnownTypeError, readKnownConstant)
import PlutusCore.Evaluation.Machine.Exception (_UnliftingErrorE)
import PlutusTx (
  BuiltinData,
  Data,
  builtinDataToData,
  dataToBuiltinData,
 )
import PlutusTx.Builtins.Class (
  FromBuiltin,
  ToBuiltin,
  fromBuiltin,
  toBuiltin,
 )
import qualified Prettyprinter as P

{- |
Laws:
 - It must be that @PConstantRepr (PLifted p)@ when encoded as a constant
   in UPLC (via the 'UntypedPlutusCore.Constant' constructor) is a valid @p@.
-}
class (PConstantDecl (PLifted p), PConstanted (PLifted p) ~ p) => PUnsafeLiftDecl (p :: PType) where
  type PLifted p = (r :: Type) | r -> p

{- | Class of Haskell types `h` that can be represented as a Plutus core builtin
and converted to a Plutarch type.

The Plutarch type is determined by `PConstanted h`. Its Plutus Core representation is given by `PConstantRepr h`.

This typeclass is closely tied with 'PLift'.

Laws:
 - @pconstantFromRepr . pconstantToRepr ≡ Just@
 - @(pconstantToRepr <$>) . pconstantFromRepr ≡ Just@
 - @plift . pfromData . flip ptryFrom fst . pconstant . PlutusTx.toData ≡ id@
 - @PlutusTx.fromData . plift . pforgetData . pdata . pconstant ≡ Just@

These laws must be upheld for the sake of soundness of the type system.
-}
class
  ( PUnsafeLiftDecl (PConstanted h)
  , PLC.DefaultUni `PLC.Includes` PConstantRepr h
  ) =>
  PConstantDecl (h :: Type)
  where
  type PConstantRepr h :: Type
  type PConstanted h :: PType
  pconstantToRepr :: h -> PConstantRepr h
  pconstantFromRepr :: PConstantRepr h -> Maybe h

{- | Class of Plutarch types `p` that can be converted to/from a Haskell type.

The Haskell type is determined by `PLifted p`.

This typeclass is closely tied with 'PConstant'.
-}
type PLift :: PType -> Constraint
type PLift = PUnsafeLiftDecl

{- | Create a Plutarch-level constant, from a Haskell value.
Example:
> pconstant @PInteger 42
-}
pconstant :: forall p s. PLift p => PLifted p -> Term s p
pconstant x = punsafeConstantInternal $ PLC.someValue @(PConstantRepr (PLifted p)) @PLC.DefaultUni $ pconstantToRepr x

{- | Error during script evaluation.

 @since 1.2.1
-}
data LiftError
  = LiftError_EvalError EvalError
  | LiftError_KnownTypeError KnownTypeError
  | LiftError_FromRepr
  | LiftError_CompilationError Text
  deriving stock (Eq)

instance P.Pretty LiftError where
  pretty (LiftError_EvalError err) = "plift failed: erring term: " <> P.pretty (show err)
  pretty (LiftError_KnownTypeError err) =
    let unliftErrorMaybe = err ^? _UnliftingErrorE
     in "plift failed: incorrect type: "
          <> maybe "absurd evaluation failure" (P.pretty . show) unliftErrorMaybe
  pretty LiftError_FromRepr = "plift failed: pconstantFromRepr returned 'Nothing'"
  pretty (LiftError_CompilationError err) = "plift failed: compilation failed: " <> P.pretty (show err)

pliftCompiled ::
  forall (p :: S -> Type).
  (PLift p) =>
  CompiledTerm p ->
  Either LiftError (PLifted p)
pliftCompiled (CompiledTerm term) =
  case readKnownConstant term of
    Right r -> case pconstantFromRepr r of
      Just h -> Right h
      Nothing -> Left LiftError_FromRepr
    Left e -> Left $ LiftError_KnownTypeError e

pliftTrace ::
  forall (p :: S -> Type).
  (PLift p) =>
  ClosedTerm p ->
  (Either LiftError (PLifted p), ExBudget, [Text])
pliftTrace term =
  case compileTerm' (Config {tracingMode = DoTracing}) term of
    Right cterm ->
      let (cterm', budget, trace) = evalCompiled cterm
       in case cterm' of
            Right cterm'' -> (pliftCompiled cterm'', budget, trace)
            Left err -> (Left $ LiftError_EvalError err, budget, trace)
    Left err -> (Left $ LiftError_CompilationError err, mempty, mempty)

{-# DEPRECATED plift' "use pliftTrace" #-}

plift' ::
  forall (p :: S -> Type).
  (PLift p) =>
  ClosedTerm p ->
  Either LiftError (PLifted p)
plift' term =
    let (lifted, _, _) = pliftTrace term
    in lifted

-- | Like `pliftTrace` but throws on failure and does not return traces.
plift ::
  forall (p :: S -> Type).
  (HasCallStack, PLift p) =>
  ClosedTerm p ->
  PLifted p
plift term =
    let (lifted, _, _) = pliftTrace term
    in either (error . show . P.pretty) id lifted

{- | Newtype wrapper for deriving @PConstant@ when the wrapped type is directly
represented by a builtin UPLC type that is /not/ @Data@.

  Ex: @PInteger@ is directly represented as a builtin integer.
-}
newtype DerivePConstantDirect (h :: Type) (p :: PType) = DerivePConstantDirect h

instance
  (PLift p, PLC.DefaultUni `PLC.Includes` h) =>
  PConstantDecl (DerivePConstantDirect h p)
  where
  type PConstantRepr (DerivePConstantDirect h p) = h
  type PConstanted (DerivePConstantDirect h p) = p
  pconstantToRepr = coerce
  pconstantFromRepr = Just . coerce

{- | Newtype wrapper for deriving @PConstant@ when the wrapped type is represented
indirectly by a builtin UPLC type that is /not/ @Data@.

  Ex: @PPubKeyHash@ is a newtype to a @PByteString@ and @PByteString@ is directly
  represented as a builtin bytestring.

Polymorphic types can be derived as follows:

>newtype Foo a = Foo a
>
>newtype PFoo a s = PFoo (Term s a)
>
>instance forall a. PLift a => PUnsafeLiftDecl (PFoo a) where
>  type PLifted (PFoo a) = Foo (PLifted a)
>
>deriving via
>  ( DerivePConstantViaNewtype
>      (Foo a)
>      (PFoo (PConstanted a))
>      (PConstanted a)
>  )
>  instance
>    PConstant a =>
>    PConstantDecl (Foo a)
-}
newtype
  DerivePConstantViaNewtype
    (h :: Type)
    (p :: PType) -- PType to associate with the newtype
    (p' :: PType) -- Underlying UPLC representation type
  = -- | The Haskell newtype we are deriving a @PConstant@ instance for
    DerivePConstantViaNewtype h

{- | Type synonym to simplify deriving of @PConstant@ via @DerivePConstantViaNewtype@.

A newtype @Foo a@ is considered "Constantable" if:

- The wrapped type @a@ has a @PConstant@ instance.
- The lifted type of @a@ has a @PUnsafeLiftDecl@ instance.
- There is type equality between @a@ and @PLifted (PConstanted a)@.

These constraints are sufficient to derive a @PConstant@ instance for the newtype.

For deriving @PConstant@ for a wrapped type represented in UPLC as @Data@, see
@DerivePConstantViaData@.
-}
type PConstant :: Type -> Constraint
type PConstant a = (a ~ PLifted (PConstanted a), PConstantDecl a)

instance (PLift p, PLift p', Coercible h (PLifted p')) => PConstantDecl (DerivePConstantViaNewtype h p p') where
  type PConstantRepr (DerivePConstantViaNewtype h p p') = PConstantRepr (PLifted p')
  type PConstanted (DerivePConstantViaNewtype h p p') = p
  pconstantToRepr x = pconstantToRepr @(PLifted p') $ coerce x
  pconstantFromRepr x = coerce $ pconstantFromRepr @(PLifted p') x

class ToBuiltin' a arep | a -> arep where
  toBuiltin' :: a -> arep

class FromBuiltin' arep a | arep -> a where
  fromBuiltin' :: arep -> a

-- FIXME this overlappable instance is nonsense and disregards the fundep
instance {-# OVERLAPPABLE #-} ToBuiltin a arep => ToBuiltin' a arep where
  toBuiltin' = toBuiltin

instance {-# OVERLAPPABLE #-} FromBuiltin arep a => FromBuiltin' arep a where
  fromBuiltin' = fromBuiltin

instance ToBuiltin' Data BuiltinData where
  toBuiltin' = dataToBuiltinData

instance FromBuiltin' BuiltinData Data where
  fromBuiltin' = builtinDataToData

newtype DerivePConstantViaBuiltin (h :: Type) (p :: PType) (p' :: PType) = DerivePConstantViaBuiltin h

instance
  ( PLift p
  , PLift p'
  , Coercible h h'
  , ToBuiltin' (PLifted p') h'
  , FromBuiltin' h' (PLifted p')
  ) =>
  PConstantDecl (DerivePConstantViaBuiltin h p p')
  where
  type PConstantRepr (DerivePConstantViaBuiltin h p p') = PConstantRepr (PLifted p')
  type PConstanted (DerivePConstantViaBuiltin h p p') = p
  pconstantToRepr x = pconstantToRepr @(PLifted p') $ fromBuiltin' (coerce x :: h')
  pconstantFromRepr x = coerce (toBuiltin' <$> pconstantFromRepr @(PLifted p') x :: Maybe h')
