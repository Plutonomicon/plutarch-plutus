{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Plutarch.Lift (PConstant (..), PUnsafeLiftDecl (..), PLift, pconstant, plift, plift', LiftError, DerivePLiftViaCoercible (..), DerivePConstant) where

import Data.Coerce
import Data.Kind (Type)
import GHC.Stack (HasCallStack)
import Plutarch.Evaluate (evaluateScript)
import Plutarch.Internal (ClosedTerm, PType, S, Term, compile, punsafeConstantInternal)
import qualified Plutus.V1.Ledger.Scripts as Scripts
import qualified PlutusCore as PLC
import PlutusCore.Constant (readKnownConstant)
import PlutusCore.Evaluation.Machine.Exception (MachineError)
import qualified UntypedPlutusCore as UPLC

class (PConstant (PLifted p), PConstanted (PLifted p) ~ p, PLC.DefaultUni `PLC.Includes` PLiftedRepr p) => PUnsafeLiftDecl (p :: PType) where
  type PLiftedRepr p :: Type
  type PLifted p :: Type
  pliftToRepr :: PLifted p -> PLiftedRepr p
  pliftFromRepr :: PLiftedRepr p -> Maybe (PLifted p)

class (PLift (PConstanted h), PLifted (PConstanted h) ~ h) => PConstant (h :: Type) where
  type PConstanted h :: PType

type PLift = PUnsafeLiftDecl

newtype DerivePLiftViaCoercible (h :: Type) (p :: PType) (r :: Type) (s :: S) = DerivePLiftViaCoercible (p s)
newtype DerivePConstant (h :: Type) (p :: PType) = DerivePConstant h

instance (PConstant h, PConstanted h ~ DerivePLiftViaCoercible h p r, Coercible h r, PLC.DefaultUni `PLC.Includes` r) => PUnsafeLiftDecl (DerivePLiftViaCoercible h p r) where
  type PLiftedRepr (DerivePLiftViaCoercible h p r) = r
  type PLifted (DerivePLiftViaCoercible h p r) = h
  pliftToRepr = coerce
  pliftFromRepr = Just . coerce

instance (PLift p, PLifted p ~ DerivePConstant h p) => PConstant (DerivePConstant h p) where
  type PConstanted (DerivePConstant h p) = p

pconstant :: forall p s. PUnsafeLiftDecl p => PLifted p -> Term s p
pconstant x = punsafeConstantInternal $ PLC.someValue @(PLiftedRepr p) @PLC.DefaultUni $ pliftToRepr x

-- | Error during script evaluation.
data LiftError = LiftError deriving stock (Eq, Show)

plift' :: forall p. PUnsafeLiftDecl p => ClosedTerm p -> Either LiftError (PLifted p)
plift' prog = case evaluateScript (compile prog) of
  Right (_, _, Scripts.unScript -> UPLC.Program _ _ term) ->
    case readKnownConstant @_ @(PLiftedRepr p) @(MachineError PLC.DefaultFun) Nothing term of
      Right r -> case pliftFromRepr r of
        Just h -> Right h
        Nothing -> Left LiftError
      Left _ -> Left LiftError
  Left _ -> Left LiftError

plift :: forall p. (HasCallStack, PUnsafeLiftDecl p) => ClosedTerm p -> (PLifted p)
plift prog = case plift' prog of
  Right x -> x
  Left _ -> error "plift failed"

-- FIXME: improve error messages using below code
{-
  plift' prog =
    case evaluateScript (compile prog) of
      Left e -> Left $ LiftError_ScriptError e
      Right (_, _, Scripts.unScript -> UPLC.Program _ _ term) ->
        first (LiftError_EvalException . showEvalException) $
          readKnownSelf term

showEvalException :: EvaluationException CekUserError (MachineError PLC.DefaultFun) (UPLC.Term UPLC.DeBruijn PLC.DefaultUni PLC.DefaultFun ()) -> Text
showEvalException = T.pack . show
-}
