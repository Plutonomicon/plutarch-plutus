{-# LANGUAGE ImpredicativeTypes #-}
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
  punsafeConstr,
  punsafeCase,
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
import Data.These (These (That, These, This))
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEVector
import Data.Word (Word64)
import GHC.Stack (CallStack, HasCallStack, callStack)
import Plutarch.Backend.PosTree (
  PosTree (
    PApplyCase,
    PHere,
    PMany,
    POne,
    PTwo
  ),
 )
import Plutarch.Backend.RawTerm (
  RawTerm (
    RApply,
    RBuiltin,
    RCase,
    RConstant,
    RConstr,
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
  = Term {asRawTerm :: ExceptT TermError (RWS TermEnv () Word64) (VarMap, RawTerm ())}

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
  let varTerm = Term . pure $ (vmSingleton fresh PHere, RVar () Argument)
  (vm, t) <- asRawTerm (f varTerm)
  let (mpt, vm') = vmDelete fresh vm
  pure (vmMap POne vm', RLamAbs () mpt t)

plet ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s a -> (Term s a -> Term s b) -> Term s b
plet v f = Term $ do
  fresh <- freshAndIncrement
  let varTerm = Term . pure $ (vmSingleton fresh PHere, RVar () LetBinding)
  (fvm, ft) <- asRawTerm (f varTerm)
  (vvm, vt) <- asRawTerm v
  let (fpt, fvm') = vmDelete fresh fvm
  let fvmExtended = vmMap (PTwo . This) fvm'
  let vvmExtended = vmMap (PTwo . That) vvm
  let vm = vmMerge mergeTwo fvmExtended vvmExtended
  pure (vm, RLet () fpt vt ft)

pfix ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  (Term s (a :--> b) -> Term s (a :--> b)) ->
  Term s (a :--> b)
pfix f = Term $ do
  fresh <- freshAndIncrement
  let varTerm = Term . pure $ (vmSingleton fresh PHere, RVar () Self)
  (vm, t) <- asRawTerm (f varTerm)
  let (mpt, vm') = vmDelete fresh vm
  case mpt of
    Nothing -> throwError UnusedSelfArgument
    Just pt -> pure (vmMap POne vm', RFix () pt t)

pthrow ::
  forall (a :: S -> Type) (s :: S).
  HasCallStack => Text -> Term s a
pthrow = Term . throwError . UserSpecified callStack

papp ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s (a :--> b) -> Term s a -> Term s b
papp f x = Term $ do
  (fvm, ft) <- asRawTerm f
  (xvm, xt) <- asRawTerm x
  case ft of
    RApply () func args -> do
      let merged = vmMerge mergeExtendApply fvm xvm
      pure (merged, RApply () func . NEVector.snoc args $ xt)
    _ -> do
      let fvmExtended = vmMap (\pt -> PApplyCase (Just pt) (NEVector.singleton Nothing)) fvm
      let xvmExtended = vmMap (PApplyCase Nothing . NEVector.singleton . Just) xvm
      let merged = vmMerge mergeApplyCase fvmExtended xvmExtended
      pure (merged, RApply () ft . NEVector.singleton $ xt)

pdelay ::
  forall (a :: S -> Type) (s :: S).
  Term s a -> Term s (PDelayed a)
pdelay t = Term $ do
  (vm, t') <- asRawTerm t
  pure (vmMap POne vm, RDelay () t')

pforce ::
  forall (a :: S -> Type) (s :: S).
  Term s (PDelayed a) -> Term s a
pforce t = Term $ do
  (vm, t') <- asRawTerm t
  pure (vmMap POne vm, RForce () t')

perror :: forall (a :: S -> Type) (s :: S). Term s a
perror = Term . pure $ (vmEmpty, RError ())

pplaceholder ::
  forall (a :: S -> Type) (s :: S).
  Integer -> Term s a
pplaceholder i = Term . pure $ (vmEmpty, RPlaceholder () i)

punsafeCoerce ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s a -> Term s b
punsafeCoerce = Term . asRawTerm

punsafeBuiltin ::
  forall (a :: S -> Type) (s :: S).
  PLC.DefaultFun -> Term s a
punsafeBuiltin f = Term . pure $ (vmEmpty, RBuiltin () f)

punsafeConstantInternal ::
  forall (a :: S -> Type) (s :: S).
  Some (ValueOf PLC.DefaultUni) -> Term s a
punsafeConstantInternal c = Term . pure $ (vmEmpty, RConstant () c)

punsafeConstr ::
  forall (a :: S -> Type) (s :: S).
  Word64 ->
  Vector (forall (b :: S -> Type). Term s b) ->
  Term s a
punsafeConstr ix fields = Term $ do
  -- Note (Koz, 28/05/2026): We need to use the constructor explicitly here, as
  -- `asRawTerm` can't solve for the existential for some reason.
  fields' <- traverse (\(Term t) -> t) fields
  let len = Vector.length fields
  let vm = Vector.ifoldl' (go len) vmEmpty . fmap fst $ fields'
  pure (vm, RConstr () ix . fmap snd $ fields')
  where
    go :: Int -> VarMap -> Int -> VarMap -> VarMap
    go len acc ix = vmMerge mergeConstr acc . vmMap (toConstr len ix)

punsafeCase ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s a ->
  NonEmptyVector (forall (c :: S -> Type). Term s c) ->
  Term s b
punsafeCase scrut handlers = Term $ do
  (vmScrut, tscrut) <- asRawTerm scrut
  -- Note (Koz, 28/05/2026): We need to use the constructor explicitly here, as
  -- `asRawTerm` can't solve for the existential for some reason.
  handlers' <- traverse (\(Term t) -> t) handlers
  let len = NEVector.length handlers
  let vmScrutExtended = vmMap (\pt -> PApplyCase (Just pt) . NEVector.replicate1 len $ Nothing) vmScrut
  let vm = NEVector.ifoldl' (go len) vmScrutExtended . fmap fst $ handlers'
  pure (vm, RCase () tscrut . fmap snd $ handlers')
  where
    go :: Int -> VarMap -> Int -> VarMap -> VarMap
    go len acc ix = vmMerge mergeApplyCase acc . vmMap (toCase len ix)

-- Helpers

toCase :: Int -> Int -> PosTree -> PosTree
toCase len ix pt = PApplyCase Nothing . NEVector.generate1 len $ \ix' -> if ix == ix' then Just pt else Nothing

toConstr :: Int -> Int -> PosTree -> PosTree
toConstr len ix pt = PMany . Vector.generate len $ \ix' -> if ix == ix' then Just pt else Nothing

mergeConstr :: PosTree -> PosTree -> PosTree
mergeConstr (PMany xs) (PMany ys) = PMany . Vector.zipWith (<|>) xs $ ys
mergeConstr x _ = x

mergeTwo :: PosTree -> PosTree -> PosTree
mergeTwo (PTwo (This t1)) (PTwo (That t2)) = PTwo . These t1 $ t2
mergeTwo x _ = x

mergeApplyCase :: PosTree -> PosTree -> PosTree
mergeApplyCase (PApplyCase func1 args1) (PApplyCase func2 args2) =
  PApplyCase (func1 <|> func2) (NEVector.zipWith (<|>) args1 args2)
mergeApplyCase x _ = x

mergeExtendApply :: PosTree -> PosTree -> PosTree
mergeExtendApply (PApplyCase func args) x = PApplyCase func . NEVector.snoc args . Just $ x
mergeExtendApply x _ = x

freshAndIncrement ::
  forall (m :: Type -> Type) (a :: Type).
  (MonadState a m, Num a) =>
  m a
freshAndIncrement = do
  fresh <- get
  modify (+ 1)
  pure fresh
