{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Lift (
  -- * Converstion between Plutarch terms and Haskell types
  pconstant,
  plift,
  LiftError (..),

  -- * Define your own conversion
  PLift (..),

  -- * Internal use
  PBuiltinType (..),
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

-- | Error during script evaluation.
data LiftError
  = LiftError_ScriptError Scripts.ScriptError
  | LiftError_EvalException T.Text -- Using Text, because there is no Eq possible with DeBruijn naming.
  | LiftError_Custom T.Text
  deriving stock (Eq, Show)

instance IsString LiftError where
  fromString = LiftError_Custom . T.pack

{- | Class of Plutarch types `p` that can be converted to/from a Haskell type.

The Haskell type is determined by `PHaskellType p`.
-}
class PLift (p :: k -> Type) where
  -- | The associated Haskell type for `p`
  type PHaskellType p :: Type

  -- {-
  -- Create a Plutarch-level constant, from a Haskell value.
  -- Example:
  -- > pconstant @PInteger 42
  -- -}
  pconstant' :: PHaskellType p -> Term s p

  -- {-
  -- Convert a Plutarch term to the associated Haskell value. Fail otherwise.
  -- This will fully evaluate the arbitrary closed expression, and convert the
  -- resulting value.
  -- -}
  plift' :: ClosedTerm p -> Either LiftError (PHaskellType p)

-- | Like `pconstant'` but TypeApplication-friendly
pconstant :: forall p s. PLift p => PHaskellType p -> Term s p
pconstant = pconstant'

-- | Like `plift'` but fails on error.
plift :: (PLift p, HasCallStack) => ClosedTerm p -> PHaskellType p
plift prog = either (error . show) id $ plift' prog

{- | DerivingVia representation to auto-derive `PLift` for Plutarch types
 representing builtin Plutus types in the `DefaultUni`.

 The `h` parameter is the Haskell type associated with `p`. Example use:

 > deriving PLift via (PBuiltinType PInteger Integer)

 See instance below.
-}
newtype PBuiltinType (p :: k -> Type) (h :: Type) s = PBuiltinType (p s)

instance
  ( PLC.KnownTypeIn PLC.DefaultUni (UPLC.Term PLC.DeBruijn PLC.DefaultUni PLC.DefaultFun ()) h
  , PLC.DefaultUni `PLC.Contains` h
  ) =>
  PLift (PBuiltinType p h)
  where
  type PHaskellType (PBuiltinType p h) = h
  pconstant' =
    punsafeConstantInternal . PLC.Some . PLC.ValueOf (PLC.knownUniOf (Proxy @h))
  plift' prog =
    case evaluateScript (compile prog) of
      Left e -> Left $ LiftError_ScriptError e
      Right (_, _, Scripts.unScript -> UPLC.Program _ _ term) ->
        first (LiftError_EvalException . showEvalException) $
          readKnownSelf term

showEvalException :: EvaluationException CekUserError (MachineError PLC.DefaultFun) (UPLC.Term UPLC.DeBruijn PLC.DefaultUni PLC.DefaultFun ()) -> Text
showEvalException = T.pack . show
