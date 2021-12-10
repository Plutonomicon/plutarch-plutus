module Plutarch.Internal.Core (Constant(..), (:-->), PDelayed, Term, pLam, pApp, pDelay, pForce, pHoist, pError, pUnsafeCoerce, pUnsafeBuiltin, pUnsafeConstant, compile) where

import qualified UntypedPlutusCore as UPLC
import qualified PlutusCore as PLC
import PlutusCore (ValueOf(ValueOf), Some(Some))
import PlutusCore.DeBruijn (DeBruijn(DeBruijn), Index(Index))
import Plutus.V1.Ledger.Scripts (Script(Script))
import Numeric.Natural (Natural)
import Data.Hashable (Hashable, hashWithSalt, hash)
import Data.Kind (Type)
import GHC.Generics (Generic)
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import PlutusCore.Data (Data)

data (:-->) x y
infixr 0 :-->
data PDelayed a

data Constant a where
  CInteger    :: Constant (PLC.Esc Integer)
  CByteString :: Constant (PLC.Esc BS.ByteString)
  CString     :: Constant (PLC.Esc Text.Text)
  CUnit       :: Constant (PLC.Esc ())
  CBool       :: Constant (PLC.Esc Bool)
  CProtoList  :: Constant (PLC.Esc [])
  CProtoPair  :: Constant (PLC.Esc (,))
  CApplyC     :: (Hashable a, Hashable (f a)) => !(Constant (PLC.Esc f)) -> !(Constant (PLC.Esc a)) -> Constant (PLC.Esc (f a))
  CData       :: Constant (PLC.Esc Data)

instance {-# OVERLAPPING #-} Hashable (Some (ValueOf Constant)) where
  hashWithSalt s (Some (ValueOf CInteger x)) = s `hashWithSalt` x
  hashWithSalt s (Some (ValueOf CByteString x)) = s `hashWithSalt` x
  hashWithSalt s (Some (ValueOf CString x)) = s `hashWithSalt` x
  hashWithSalt s (Some (ValueOf CUnit x)) = s `hashWithSalt` x
  hashWithSalt s (Some (ValueOf CBool x)) = s `hashWithSalt` x
  hashWithSalt _ (Some (ValueOf CData _)) = error "FIXME: Implement `Hashable Data`" -- s `hashWithSalt` x
  hashWithSalt s (Some (ValueOf (CApplyC _ _) x)) = s `hashWithSalt` x

convertUni :: Constant a -> UPLC.DefaultUni a
convertUni CInteger = PLC.DefaultUniInteger
convertUni CByteString = PLC.DefaultUniByteString
convertUni CString = PLC.DefaultUniString
convertUni CUnit = PLC.DefaultUniUnit
convertUni CBool = PLC.DefaultUniBool
convertUni CProtoList = PLC.DefaultUniProtoList
convertUni CProtoPair = PLC.DefaultUniProtoPair
convertUni (CApplyC f x) = PLC.DefaultUniApply (convertUni f) (convertUni x)
convertUni CData = PLC.DefaultUniData

convertConstant :: Some (ValueOf Constant) -> Some (ValueOf UPLC.DefaultUni)
convertConstant (Some (ValueOf t v)) = Some (ValueOf (convertUni t) v)

data RawTerm
  = RVar Natural
  | RLamAbs RawTerm
  | RApply RawTerm RawTerm
  | RForce RawTerm
  | RDelay RawTerm
  | RConstant (Some (ValueOf Constant))
  | RBuiltin PLC.DefaultFun
  | RError
  deriving stock Generic
  deriving anyclass Hashable

rawTermToUPLC :: RawTerm -> UPLC.Term DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()
rawTermToUPLC (RVar i) = UPLC.Var () (DeBruijn . Index $ i)
rawTermToUPLC (RLamAbs t) = UPLC.LamAbs () (DeBruijn . Index $ 0) (rawTermToUPLC t)
rawTermToUPLC (RApply x y) = UPLC.Apply () (rawTermToUPLC x) (rawTermToUPLC y)
rawTermToUPLC (RDelay t) = UPLC.Delay () (rawTermToUPLC t)
rawTermToUPLC (RForce t) = UPLC.Force () (rawTermToUPLC t)
rawTermToUPLC (RBuiltin f) = UPLC.Builtin () f
rawTermToUPLC (RConstant c) = UPLC.Constant () (convertConstant c)
rawTermToUPLC RError = UPLC.Error ()

-- Source: Unembedding Domain-Specific Languages by Robert Atkey, Sam Lindley, Jeremy Yallop
-- Thanks!
newtype Term (a :: Type) = Term { asRawTerm :: Natural -> RawTerm }

pLam :: (Term a -> Term b) -> Term (a :--> b)
pLam f = Term $ \i ->
  let v = Term $ \j -> RVar (j - (i + 1)) in
  RLamAbs $ asRawTerm (f v) (i + 1)

pApp :: Term (a :--> b) -> Term a -> Term b
pApp x y = Term $ \i -> RApply (asRawTerm x i) (asRawTerm y i)

pDelay :: Term a -> Term (PDelayed a)
pDelay x = Term $ \i -> RDelay (asRawTerm x i)

pForce :: Term (PDelayed a) -> Term a
pForce x = Term $ \i -> RForce (asRawTerm x i)

pError :: Term a
pError = Term $ \_ -> RError

pUnsafeCoerce :: Term a -> Term b
pUnsafeCoerce (Term x) = Term x

pUnsafeBuiltin :: UPLC.DefaultFun -> Term a
pUnsafeBuiltin f = Term $ \_ -> RBuiltin f

pUnsafeConstant :: Some (ValueOf Constant) -> Term a
pUnsafeConstant c = Term $ \_ -> RConstant c

-- FIXME: hoist
pHoist :: Term a -> Term a
pHoist = id

compile :: Term a -> Script
compile t = Script $ UPLC.Program () (PLC.defaultVersion ()) (rawTermToUPLC $ asRawTerm t 0)
