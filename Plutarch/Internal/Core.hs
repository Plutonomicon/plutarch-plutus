module Plutarch.Internal.Core (PInteger, PByteString, PString, PBool, PList, (:-->), PDelayed, POpaque, PData, Term, pLam, pApp, pDelay, pForce, pHoist, pError, pUnsafeCoerce, pBuiltin, pConstant, compile) where

import qualified UntypedPlutusCore as UPLC
import qualified PlutusCore as PLC
import PlutusCore.DeBruijn (DeBruijn(DeBruijn), Index(Index))
import Plutus.V1.Ledger.Scripts (Script(Script))
import Numeric.Natural (Natural)

data PInteger
data PByteString
data PString
data PBool
data PList a
data (:-->) x y
infixr 0 :-->
data PDelayed a
data POpaque
data PData

-- Source: Unembedding Domain-Specific Languages by Robert Atkey, Sam Lindley, Jeremy Yallop
-- Thanks!
newtype Term (a :: *) = Term { asRawTerm :: Natural -> UPLC.Term DeBruijn UPLC.DefaultUni UPLC.DefaultFun () }

pLam :: (Term a -> Term b) -> Term (a :--> b)
pLam f = Term $ \i ->
  let v = Term $ \j -> UPLC.Var () (DeBruijn . Index $ j - (i + 1)) in
  UPLC.LamAbs () (DeBruijn . Index $ 0) $ asRawTerm (f v) (i + 1)

pApp :: Term (a :--> b) -> Term a -> Term b
pApp x y = Term $ \i -> UPLC.Apply () (asRawTerm x i) (asRawTerm y i)

pDelay :: Term a -> Term (PDelayed a)
pDelay x = Term $ \i -> UPLC.Delay () (asRawTerm x i)

pForce x = Term $ \i -> UPLC.Force () (asRawTerm x i)
pForce :: Term (PDelayed a) -> Term a

pError :: Term a
pError = Term $ \_ -> UPLC.Error ()

pUnsafeCoerce :: Term a -> Term b
pUnsafeCoerce (Term x) = Term x

pBuiltin :: UPLC.DefaultFun -> Term a
pBuiltin f = Term $ \_ -> UPLC.Builtin () f

pConstant :: PLC.Some (PLC.ValueOf PLC.DefaultUni) -> Term a
pConstant c = Term $ \_ -> UPLC.Constant () c

-- FIXME: hoist
pHoist :: Term a -> Term a
pHoist = id

compile :: Term a -> Script
compile t = Script $ UPLC.Program () (PLC.defaultVersion ()) (asRawTerm t 0)
