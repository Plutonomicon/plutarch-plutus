{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeData #-}

module Plutarch.Backend.Term (
  RawTerm (..),
  TermEnv (..),
  Term (..),
  TermError (..),
  PDelayed,
  S,
  (:-->) (..),
  plam',
  plet,
  pthrow,
  papp,
  pdelay,
  pforce,
  perror,
  pplaceholder,
  punsafeCoerce,
  punsafeBuiltin,
  punsafeConstantInternal,
  pfix,
) where

import Control.Applicative ((<|>))
import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.RWS.CPS (
  MonadState,
  RWS,
  get,
  modify,
 )
import Data.Kind (Type)
import Data.Text (Text)
import Data.Vector.NonEmpty qualified as NEVector
import Data.Word (Word64)
import GHC.Stack (CallStack, HasCallStack, callStack)
import Plutarch.Backend.PosTree (PosTree (PApply, PHere, PLet, POne))
import Plutarch.Backend.RawTerm (
  RawTerm (
    RApply,
    RBuiltin,
    RConstant,
    RDelay,
    RError,
    RFix,
    RForce,
    RLamAbs,
    RLet,
    RPlaceholder,
    RVar
  ),
  VarTag (Argument, LetBinding, Self),
  getRawTermAnn,
 )
import Plutarch.Backend.VarMap (
  VarMap,
  vmDelete,
  vmEmpty,
  vmMap,
  vmMerge,
  vmSingleton,
 )
import PlutusCore (Some, ValueOf)
import PlutusCore qualified as PLC

type data S

data TermEnv = TermEnv

data TermError
  = UnusedSelfArgument
  | UserSpecified CallStack Text

newtype Term (s :: S) (a :: S -> Type)
  = Term {asRawTerm :: ExceptT TermError (RWS TermEnv () Word64) (RawTerm VarMap)}

type role Term nominal nominal

newtype (:-->) (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PLam (Term s a -> Term s b)

infixr 0 :-->

data PDelayed (a :: S -> Type) (s :: S)

plam' ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  (Term s a -> Term s b) -> Term s (a :--> b)
plam' f = Term $ do
  fresh <- freshAndIncrement
  let varTerm = Term . pure . RVar (vmSingleton fresh PHere) $ Argument
  t <- asRawTerm (f varTerm)
  let vm = getRawTermAnn t
  let (mpt, vm') = vmDelete fresh vm
  let vmExtended = vmMap POne vm'
  pure $ case t of
    RLamAbs _ paramTrees t' -> RLamAbs vmExtended (NEVector.cons mpt paramTrees) t'
    _ -> RLamAbs vmExtended (NEVector.singleton mpt) t

plet ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s a -> (Term s a -> Term s b) -> Term s b
plet v f = Term $ do
  fresh <- freshAndIncrement
  let varTerm = Term . pure . RVar (vmSingleton fresh PHere) $ LetBinding
  ft <- asRawTerm (f varTerm)
  let fvm = getRawTermAnn ft
  vt <- asRawTerm v
  let vvm = getRawTermAnn vt
  let (fpt, fvm') = vmDelete fresh fvm
  let fvmExtended = vmMap (PLet Nothing . Just) fvm'
  let vvmExtended = vmMap (\pt -> PLet (Just pt) Nothing) vvm
  let vm = vmMerge mergeLet fvmExtended vvmExtended
  pure . RLet vm fpt vt $ ft
  where
    mergeLet :: PosTree -> PosTree -> PosTree
    mergeLet (PLet v1 f1) (PLet v2 f2) = PLet (v1 <|> v2) (f1 <|> f2)
    mergeLet x _ = x

pfix ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  (Term s (a :--> b) -> Term s (a :--> b)) ->
  Term s (a :--> b)
pfix f = Term $ do
  fresh <- freshAndIncrement
  let varTerm = Term . pure . RVar (vmSingleton fresh PHere) $ Self
  t <- asRawTerm (f varTerm)
  let vm = getRawTermAnn t
  let (mpt, vm') = vmDelete fresh vm
  case mpt of
    Nothing -> throwError UnusedSelfArgument
    Just pt -> do
      let vmExtended = vmMap POne vm'
      pure . RFix vmExtended pt $ t

pthrow ::
  forall (a :: S -> Type) (s :: S).
  HasCallStack => Text -> Term s a
pthrow = Term . throwError . UserSpecified callStack

papp ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s (a :--> b) -> Term s a -> Term s b
papp f x = Term $ do
  ft <- asRawTerm f
  let fvm = getRawTermAnn ft
  xt <- asRawTerm x
  let xvm = getRawTermAnn xt
  let fvmExtended = vmMap (\pt -> PApply (Just pt) (NEVector.singleton Nothing)) fvm
  let xvmExtended = vmMap (PApply Nothing . NEVector.singleton . Just) xvm
  pure $ case ft of
    RApply _ func args ->
      let merged = vmMerge mergeExtend fvmExtended xvmExtended
       in RApply merged func . NEVector.snoc args $ xt
    _ ->
      let merged = vmMerge mergeApply fvmExtended xvmExtended
       in RApply merged ft . NEVector.singleton $ xt
  where
    mergeApply :: PosTree -> PosTree -> PosTree
    mergeApply (PApply f1 xs1) (PApply f2 xs2) = PApply (f1 <|> f2) (NEVector.zipWith (<|>) xs1 xs2)
    mergeApply x _ = x
    mergeExtend :: PosTree -> PosTree -> PosTree
    mergeExtend (PApply f1 xs1) (PApply f2 xs2) = PApply (f1 <|> f2) (xs1 <> xs2)
    mergeExtend x _ = x

pdelay ::
  forall (a :: S -> Type) (s :: S).
  Term s a -> Term s (PDelayed a)
pdelay t = Term $ do
  t' <- asRawTerm t
  let vm = getRawTermAnn t'
  pure . RDelay (vmMap POne vm) $ t'

pforce ::
  forall (a :: S -> Type) (s :: S).
  Term s (PDelayed a) -> Term s a
pforce t = Term $ do
  t' <- asRawTerm t
  let vm = getRawTermAnn t'
  pure . RForce (vmMap POne vm) $ t'

perror :: forall (a :: S -> Type) (s :: S). Term s a
perror = Term . pure . RError $ vmEmpty

pplaceholder ::
  forall (a :: S -> Type) (s :: S).
  Integer -> Term s a
pplaceholder = Term . pure . RPlaceholder vmEmpty

punsafeCoerce ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s a -> Term s b
punsafeCoerce = Term . asRawTerm

punsafeBuiltin ::
  forall (a :: S -> Type) (s :: S).
  PLC.DefaultFun -> Term s a
punsafeBuiltin = Term . pure . RBuiltin vmEmpty

punsafeConstantInternal ::
  forall (a :: S -> Type) (s :: S).
  Some (ValueOf PLC.DefaultUni) -> Term s a
punsafeConstantInternal = Term . pure . RConstant vmEmpty

-- Helpers

freshAndIncrement ::
  forall (m :: Type -> Type) (a :: Type).
  (MonadState a m, Num a) =>
  m a
freshAndIncrement = do
  fresh <- get
  modify (+ 1)
  pure fresh
