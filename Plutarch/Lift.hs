{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Plutarch.Lift (PUnsafeLiftDecl (..), PLift, pconstant, plift, plift', LiftError, DerivePLiftViaCoercible (..)) where

import Data.Coerce
import Data.Kind (Type)
import GHC.Stack (HasCallStack)
import Plutarch.Evaluate (evaluateScript)
import Plutarch.Internal (ClosedTerm, Term, compile, punsafeConstantInternal)
import qualified Plutus.V1.Ledger.Scripts as Scripts
import qualified PlutusCore as PLC
import PlutusCore.Constant (readKnownConstant)
import PlutusCore.Evaluation.Machine.Exception (MachineError)
import qualified UntypedPlutusCore as UPLC

-- FIXME: `h -> p`
class (PLifted p ~ h, PLC.DefaultUni `PLC.Includes` PLiftedRepr p) => PUnsafeLiftDecl (h :: Type) (p :: k -> Type) | p -> h where
  type PLiftedRepr p :: Type
  type PLifted p :: Type
  pliftToRepr :: h -> PLiftedRepr p
  pliftFromRepr :: PLiftedRepr p -> Maybe h

class PUnsafeLiftDecl (PLifted p) p => PLift (p :: k -> Type)
instance PUnsafeLiftDecl (PLifted p) p => PLift (p :: k -> Type)

newtype DerivePLiftViaCoercible (h :: Type) (p :: k -> Type) (r :: Type) (s :: k) = DerivePLiftViaCoercible (p s)

instance (Coercible h r, PLC.DefaultUni `PLC.Includes` r) => PUnsafeLiftDecl h (DerivePLiftViaCoercible h p r) where
  type PLiftedRepr (DerivePLiftViaCoercible h p r) = r
  type PLifted (DerivePLiftViaCoercible h p r) = h
  pliftToRepr = coerce
  pliftFromRepr = Just . coerce

pconstant :: forall p h s. PUnsafeLiftDecl h p => h -> Term s p
pconstant x = punsafeConstantInternal $ PLC.someValue @(PLiftedRepr p) @PLC.DefaultUni $ pliftToRepr @_ @h @p x

-- | Error during script evaluation.
data LiftError = LiftError deriving stock (Eq, Show)

plift' :: forall p h. PUnsafeLiftDecl h p => ClosedTerm p -> Either LiftError h
plift' prog = case evaluateScript (compile prog) of
  Right (_, _, Scripts.unScript -> UPLC.Program _ _ term) ->
    case readKnownConstant @_ @(PLiftedRepr p) @(MachineError PLC.DefaultFun) Nothing term of
      Right r -> case pliftFromRepr @_ @h @p r of
        Just h -> Right h
        Nothing -> Left LiftError
      Left _ -> Left LiftError
  Left _ -> Left LiftError

plift :: forall p h. (HasCallStack, PUnsafeLiftDecl h p) => ClosedTerm p -> h
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
