{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Lift (
  -- * Converstion between Plutarch terms and Haskell types
  pconstant,
  plift,
  plift',
  LiftError (..),

  -- * Define your own conversion
  PLift,

  -- * Internal use
  PDefaultUniType,
) where

import Data.Bifunctor (first)
import Data.Data (Proxy (Proxy))
import Data.Kind (Type)
import Data.String
import Data.Text
import qualified Data.Text as T
import GHC.Stack (HasCallStack)
import Plutarch.Evaluate (evaluateScript)
import Plutarch.Internal (
  ClosedTerm,
  Term,
  compile,
  punsafeConstantInternal,
 )
import qualified Plutus.V1.Ledger.Scripts as Scripts
import qualified PlutusCore as PLC
import PlutusCore.Constant (readKnownSelf)
import qualified PlutusCore.Constant as PLC
import PlutusCore.Evaluation.Machine.Exception (
  EvaluationException,
  MachineError,
 )
import qualified UntypedPlutusCore as UPLC
import UntypedPlutusCore.Evaluation.Machine.Cek (CekUserError)

data LiftError
  = LiftError_ScriptError Scripts.ScriptError
  | LiftError_EvalException T.Text -- Using Text, because there is no Eq possible with DeBruijn naming.
  | LiftError_Custom T.Text
  deriving stock (Eq, Show)

instance IsString LiftError where
  fromString = LiftError_Custom . T.pack

class PLift p (h :: Type) | p -> h where
  -- {-
  -- Create a Plutarch-level constant, from a Haskell value.
  -- Example:
  -- > pconstant @PInteger 42
  -- -}
  pconstant :: h -> Term s p

  -- {-
  -- Convert a Plutarch term to the associated Haskell value. Fail otherwise.
  -- This will fully evaluate the arbitrary closed expression, and convert the
  -- resulting value.
  -- -}
  plift' :: ClosedTerm p -> Either LiftError h

-- | Like `plift'` but fails on error.
plift :: (PLift p h, HasCallStack) => ClosedTerm p -> h
plift prog = either (error . show) id $ plift' prog

instance
  {-# OVERLAPPABLE #-}
  ( PLC.KnownTypeIn PLC.DefaultUni (UPLC.Term PLC.DeBruijn PLC.DefaultUni PLC.DefaultFun ()) h
  , PLC.DefaultUni `PLC.Contains` h
  , PDefaultUniType p ~ h
  ) =>
  PLift p h
  where
  pconstant =
    punsafeConstantInternal . PLC.Some . PLC.ValueOf (PLC.knownUniOf (Proxy @h))
  plift' prog =
    case evaluateScript (compile prog) of
      Left e -> Left $ LiftError_ScriptError e
      Right (_, _, Scripts.unScript -> UPLC.Program _ _ term) ->
        first (LiftError_EvalException . showEvalException) $
          readKnownSelf term

showEvalException :: EvaluationException CekUserError (MachineError PLC.DefaultFun) (UPLC.Term UPLC.DeBruijn PLC.DefaultUni PLC.DefaultFun ()) -> Text
showEvalException = T.pack . show

{- | Family of eDSL Types that map to Plutus builtin in its `DefaultUni`

 We use this in: PLC.knownUniOf $ Proxy @(PDefaultUniType a)

 TODO: can we obviate this by using something from Plutus?
-}
type family PDefaultUniType (a :: k -> Type) :: Type
