{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Lift (
  Lift (..),
  Unlift (..),
) where

import Data.Data (Proxy (Proxy))
import GHC.Stack (HasCallStack)
import Plutarch
import Plutarch.Evaluate (evaluateScript)
import Plutarch.Prelude
import qualified Plutus.V1.Ledger.Scripts as Scripts
import qualified PlutusCore as PLC
import PlutusCore.Constant (readKnownSelf)
import qualified PlutusCore.Constant as PLC
import PlutusCore.Evaluation.Machine.Exception (
  EvaluationException,
  MachineError,
 )
import PlutusCore.Pretty (Pretty, PrettyConst)
import qualified UntypedPlutusCore as UPLC
import UntypedPlutusCore.Evaluation.Machine.Cek (CekUserError)

class Lift (h :: Type) p where
  -- {-
  -- Create a Plutarch-level constant, from a Haskell value.
  -- -}
  pconstant :: h -> Term s p

class Unlift (h :: Type) p where
  -- {-
  -- Convert a Plutarch term to the associated Haskell value. Fail otherwise.
  -- This will fully evaluate the arbitrary closed expression, and convert the
  -- resulting value.
  -- -}
  plift :: HasCallStack => ClosedTerm p -> h

instance UPLC.DefaultUni `PLC.Contains` h => Lift h p where
  pconstant =
    punsafeConstant . PLC.Some . PLC.ValueOf (PLC.knownUniOf (Proxy @h))

instance PLC.KnownTypeIn PLC.DefaultUni (UPLC.Term PLC.DeBruijn PLC.DefaultUni PLC.DefaultFun ()) h => Unlift h p where
  plift prog =
    case evaluateScript (compile prog) of
      Left e -> error $ show e
      Right (_, _, Scripts.unScript -> UPLC.Program _ _ term) ->
        either (error . showEvalException) id $
          readKnownSelf term

showEvalException ::
  (PLC.Everywhere uni PrettyConst, PLC.GShow uni, PLC.Closed uni, Pretty fun) =>
  EvaluationException CekUserError (MachineError fun) (UPLC.Term UPLC.DeBruijn uni fun ()) ->
  String
showEvalException = show
