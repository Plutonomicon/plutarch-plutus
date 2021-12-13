module Plutarch.Internal (Constant(..), (:-->), PDelayed, Term, pLam, pApp, pDelay, pForce, pHoistAcyclic, pError, pUnsafeCoerce, pUnsafeBuiltin, pUnsafeConstant, compile, ClosedTerm) where

import qualified UntypedPlutusCore as UPLC
import qualified PlutusCore as PLC
import PlutusCore (ValueOf(ValueOf), Some(Some))
import PlutusCore.DeBruijn (DeBruijn(DeBruijn), Index(Index))
import Plutus.V1.Ledger.Scripts (Script(Script))
import Numeric.Natural (Natural)
-- FIXME: Replace! Very bad. We need a good hashing algorithm, not this ad-hoc approach. Use blake2b.
import Data.Hashable (Hashable, hashWithSalt, hash)
import Data.Kind (Type)
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import PlutusCore.Data (Data)
import Data.List (foldl')
import qualified Data.Map.Lazy as M
import Data.Maybe (fromJust)

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
  hashWithSalt s (Some (ValueOf CInteger x)) = s `hashWithSalt` (0 :: Int) `hashWithSalt` x
  hashWithSalt s (Some (ValueOf CByteString x)) = s `hashWithSalt` (1 :: Int) `hashWithSalt` x
  hashWithSalt s (Some (ValueOf CString x)) = s `hashWithSalt` (2 :: Int) `hashWithSalt` x
  hashWithSalt s (Some (ValueOf CUnit x)) = s `hashWithSalt` (3 :: Int) `hashWithSalt` x
  hashWithSalt s (Some (ValueOf CBool x)) = s `hashWithSalt` (4 :: Int) `hashWithSalt` x
  hashWithSalt _ (Some (ValueOf CData _)) = error "FIXME: Implement `Hashable Data`" -- s `hashWithSalt` (5 :: Int) `hashWithSalt` x
  hashWithSalt s (Some (ValueOf (CApplyC _ _) x)) = s `hashWithSalt` (6 :: Int) `hashWithSalt` x

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

-- Explanation for hoisted terms:
-- Hoisting is a convenient way of importing terms without duplicating them
-- across your tree. Currently, hoisting is only supported on terms that do
-- not refer to any free variables.
--
-- An RHoisted contains a term and its hash. A RawTerm will have a DAG
-- of hoisted terms, where an edge represents a dependency.
-- We topologically sort these hoisted terms, such that each has an index.
--
-- We wrap our RawTerm in RLamAbs and RApply in an order corresponding to the
-- indices. Each level can refer to levels above it by the nature of De Bruijn naming,
-- though the name is relative to the current level.

data HoistedTerm = HoistedTerm Int RawTerm

data RawTerm
  = RVar Natural
  | RLamAbs RawTerm
  | RApply RawTerm RawTerm
  | RForce RawTerm
  | RDelay RawTerm
  | RConstant (Some (ValueOf Constant))
  | RBuiltin PLC.DefaultFun
  | RError
  | RHoisted HoistedTerm

instance Hashable RawTerm where
  hashWithSalt s (RVar x) = s `hashWithSalt` (0 :: Int) `hashWithSalt` x
  hashWithSalt s (RLamAbs x) = s `hashWithSalt` (1 :: Int) `hashWithSalt` x
  hashWithSalt s (RApply x y) = s `hashWithSalt` (2 :: Int) `hashWithSalt` x `hashWithSalt` y
  hashWithSalt s (RForce x) = s `hashWithSalt` (3 :: Int) `hashWithSalt` x
  hashWithSalt s (RDelay x) = s `hashWithSalt` (4 :: Int) `hashWithSalt` x
  hashWithSalt s (RConstant x) = s `hashWithSalt` (5 :: Int) `hashWithSalt` x
  hashWithSalt s (RBuiltin x) = s `hashWithSalt` (6 :: Int) `hashWithSalt` x
  hashWithSalt s RError = s `hashWithSalt` (7 :: Int)
  hashWithSalt s (RHoisted (HoistedTerm x _)) = s `hashWithSalt` (8 :: Int) `hashWithSalt` x

-- Source: Unembedding Domain-Specific Languages by Robert Atkey, Sam Lindley, Jeremy Yallop
-- Thanks!
-- NB: Hoisted terms must be sorted such that the dependents are first and dependencies last.
--
-- s: This parameter isn't ever instantiated with something concrete. It is merely here
-- to ensure that `compile` and `pHoistAcyclic` only accept terms without any free variables.
newtype Term (s :: k) (a :: k -> Type) = Term { asRawTerm :: Natural -> (RawTerm, [HoistedTerm]) }

type ClosedTerm (a :: k -> Type) = forall (s :: k). Term s a

data (:-->) (a :: k -> Type) (b :: k -> Type) (s :: k)
infixr 0 :-->
data PDelayed (a :: k -> Type) (s :: k)

pLam :: (Term s a -> Term s b) -> Term s (a :--> b)
pLam f = Term $ \i ->
  let
    v = Term $ \j -> (RVar (j - (i + 1)), [])
    (t, deps) = asRawTerm (f v) (i + 1)
  in
  (RLamAbs $ t, deps)

pApp :: Term s (a :--> b) -> Term s a -> Term s b
pApp x y = Term $ \i ->
  let (x', deps) = asRawTerm x i in
  let (y', deps') = asRawTerm y i in
  (RApply x' y', deps ++ deps')

pDelay :: Term s a -> Term s (PDelayed a)
pDelay x = Term $ \i ->
  let (x', deps) = asRawTerm x i in
  (RDelay x', deps)

pForce :: Term s (PDelayed a) -> Term s a
pForce x = Term $ \i ->
  let (x', deps) = asRawTerm x i in
  (RForce x', deps)

pError :: Term s a
pError = Term $ \_ -> (RError, [])

pUnsafeCoerce :: Term s a -> Term s b
pUnsafeCoerce (Term x) = Term x

pUnsafeBuiltin :: UPLC.DefaultFun -> Term s a
pUnsafeBuiltin f = Term $ \_ -> (RBuiltin f, [])

pUnsafeConstant :: Some (ValueOf Constant) -> Term s a
pUnsafeConstant c = Term $ \_ -> (RConstant c, [])

-- FIXME: Give proper error message when mutually recursive.
pHoistAcyclic :: ClosedTerm a -> Term s a
pHoistAcyclic t = Term $ \_ ->
  let (t', deps) = asRawTerm t 0 in
  let t'' = HoistedTerm (hash t') t' in
  (RHoisted t'', t'' : deps)

rawTermToUPLC :: (HoistedTerm -> Natural) -> Natural -> RawTerm -> UPLC.Term DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()
rawTermToUPLC _ _ (RVar i) = UPLC.Var () (DeBruijn . Index $ i)
rawTermToUPLC m l (RLamAbs t) = UPLC.LamAbs () (DeBruijn . Index $ 0) (rawTermToUPLC m (l + 1) t)
rawTermToUPLC m l (RApply x y) = UPLC.Apply () (rawTermToUPLC m l x) (rawTermToUPLC m l y)
rawTermToUPLC m l (RDelay t) = UPLC.Delay () (rawTermToUPLC m l t)
rawTermToUPLC m l (RForce t) = UPLC.Force () (rawTermToUPLC m l t)
rawTermToUPLC _ _ (RBuiltin f) = UPLC.Builtin () f
rawTermToUPLC _ _ (RConstant c) = UPLC.Constant () (convertConstant c)
rawTermToUPLC _ _ RError = UPLC.Error ()
rawTermToUPLC m l (RHoisted hoisted) = UPLC.Var () . DeBruijn . Index $ l - m hoisted - 1

compile' :: ClosedTerm a -> UPLC.Term DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()
compile' t =
  let
    (t', deps) = asRawTerm t 0

    f :: Natural -> Maybe Natural -> (Bool, Maybe Natural)
    f n Nothing = (True, Just n)
    f _ (Just n) = (False, Just n)

    g :: HoistedTerm -> (M.Map Int Natural, [(Natural, RawTerm)], Natural) -> (M.Map Int Natural, [(Natural, RawTerm)], Natural)
    g (HoistedTerm hash term) (map, defs, n) = case M.alterF (f n) hash map of
      (True, map) -> (map, (n, term):defs, n+1)
      (False, map) -> (map, defs, n)

  -- map: term -> de Bruijn level
  -- defs: the terms, level 0 is last
  -- n: # of terms
    (map, defs, n) = foldr g (M.empty, [], 0) deps

    map' = fromJust . flip M.lookup map . (\(HoistedTerm hash _) -> hash)

    body = rawTermToUPLC map' n t'

    wrapped = foldl' (\b (lvl, def) -> UPLC.Apply () (UPLC.LamAbs () (DeBruijn . Index $ 0) b) (rawTermToUPLC map' lvl def)) body defs
  in
  wrapped

compile :: ClosedTerm a -> Script
compile t = Script $ UPLC.Program () (PLC.defaultVersion ()) (compile' t)
