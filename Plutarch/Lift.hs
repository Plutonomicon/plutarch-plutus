{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Plutarch.Lift (PConstant (..), PUnsafeLiftDecl (..), PLift, pconstant, plift, plift', LiftError, DerivePConstantViaCoercible (..), DerivePConstantViaNewtype (..)) where

import Data.Coerce
import Data.Kind (Type)
import GHC.Stack (HasCallStack)
import Plutarch.Evaluate (evaluateScript)
import Plutarch.Internal (ClosedTerm, PType, Term, compile, punsafeConstantInternal)
import qualified Plutus.V1.Ledger.Scripts as Scripts
import qualified PlutusCore as PLC
import PlutusCore.Constant (readKnownConstant)
import PlutusCore.Evaluation.Machine.Exception (ErrorWithCause, MachineError)
import qualified UntypedPlutusCore as UPLC

class (PConstant (PLifted p), PConstanted (PLifted p) ~ p) => PUnsafeLiftDecl (p :: PType) where
  type PLifted p :: Type

class (PUnsafeLiftDecl (PConstanted h), PLC.DefaultUni `PLC.Includes` PConstantRepr h) => PConstant (h :: Type) where
  type PConstantRepr h :: Type
  type PConstanted h :: PType
  pconstantToRepr :: h -> PConstantRepr h
  pconstantFromRepr :: PConstantRepr h -> Maybe h

type PLift = PUnsafeLiftDecl

pconstant :: forall p s. PLift p => PLifted p -> Term s p
pconstant x = punsafeConstantInternal $ PLC.someValue @(PConstantRepr (PLifted p)) @PLC.DefaultUni $ pconstantToRepr x

-- | Error during script evaluation.
data LiftError
  = LiftError_ScriptError Scripts.ScriptError
  | LiftError_EvalException (ErrorWithCause (MachineError PLC.DefaultFun) ())
  | LiftError_FromRepr
  | LiftError_WrongRepr
  deriving stock (Eq, Show)

plift' :: forall p. PUnsafeLiftDecl p => ClosedTerm p -> Either LiftError (PLifted p)
plift' prog = case evaluateScript (compile prog) of
  Right (_, _, Scripts.unScript -> UPLC.Program _ _ term) ->
    case readKnownConstant @_ @(PConstantRepr (PLifted p)) @(MachineError PLC.DefaultFun) Nothing term of
      Right r -> case pconstantFromRepr r of
        Just h -> Right h
        Nothing -> Left LiftError_FromRepr
      Left e -> Left $ LiftError_EvalException e
  Left e -> Left $ LiftError_ScriptError e

plift :: forall p. (HasCallStack, PLift p) => ClosedTerm p -> (PLifted p)
plift prog = case plift' prog of
  Right x -> x
  Left e -> error $ "plift failed: " <> show e

newtype DerivePConstantViaCoercible (h :: Type) (p :: PType) (r :: Type) = DerivePConstantViaCoercible h

instance (PLift p, Coercible h r, PLC.DefaultUni `PLC.Includes` r) => PConstant (DerivePConstantViaCoercible h p r) where
  type PConstantRepr (DerivePConstantViaCoercible h p r) = r
  type PConstanted (DerivePConstantViaCoercible h p r) = p
  pconstantToRepr = coerce
  pconstantFromRepr = Just . coerce

newtype DerivePConstantViaNewtype (h :: Type) (p :: PType) (p' :: PType) = DerivePConstantViaNewtype h

instance (PLift p, PLift p', Coercible h (PLifted p')) => PConstant (DerivePConstantViaNewtype h p p') where
  type PConstantRepr (DerivePConstantViaNewtype h p p') = PConstantRepr (PLifted p')
  type PConstanted (DerivePConstantViaNewtype h p p') = p
  pconstantToRepr x = pconstantToRepr @(PLifted p') $ coerce x
  pconstantFromRepr x = coerce $ pconstantFromRepr @(PLifted p') x
