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

import Control.Monad.Except (
  ExceptT,
  MonadError,
  throwError,
 )
import Control.Monad.RWS.CPS (
  MonadState,
  RWS,
  get,
  modify,
 )
import Data.Can (Can (Eno, Non, One, Two))
import Data.Kind (Type)
import Data.Map.Merge.Strict (WhenMatched, zipWithAMatched)
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
  vmMergeM,
  vmSingleton,
 )
import PlutusCore (Some, ValueOf)
import PlutusCore qualified as PLC

type data S

data TermEnv = TermEnv

data TermError
  = UnusedSelfArgument
  | UserSpecified CallStack Text
  | BadMergeLet Word64 PosTree PosTree
  | BadMergeApplyExtend Word64 PosTree PosTree
  | BadMergeApplyCase Word64 PosTree PosTree
  | BadMergeConstr Word64 PosTree PosTree

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
  case t of
    RLamAbs () paramTrees body -> do
      let vmPeeled = vmMap (\case POne t -> t; x -> x) vm
      let (mpt, vm') = vmDelete fresh vmPeeled
      pure (vmMap POne vm', RLamAbs () (NEVector.cons mpt paramTrees) body)
    _ -> do
      let (mpt, vm') = vmDelete fresh vm
      pure (vmMap POne vm', RLamAbs () (NEVector.singleton mpt) t)

plet ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s a -> (Term s a -> Term s b) -> Term s b
plet v f = Term $ do
  fresh <- freshAndIncrement
  let varTerm = Term . pure $ (vmSingleton fresh PHere, RVar () LetBinding)
  (fvm, ft) <- asRawTerm (f varTerm)
  (vvm, vt) <- asRawTerm v
  let (fpt, fvm') = vmDelete fresh fvm
  let vvmExtended = vmMap (PTwo . This) vvm
  let fvmExtended = vmMap (PTwo . That) fvm'
  vm <- vmMergeM mergeTwo vvmExtended fvmExtended
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
      merged <- vmMergeM mergeExtendApply fvm xvm
      pure (merged, RApply () func . NEVector.snoc args $ xt)
    _ -> do
      let fvmExtended = vmMap (\pt -> PApplyCase (Just pt) (NEVector.singleton Nothing)) fvm
      let xvmExtended = vmMap (PApplyCase Nothing . NEVector.singleton . Just) xvm
      merged <- vmMergeM mergeApplyCase fvmExtended xvmExtended
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
  vm <- Vector.ifoldM (go len) vmEmpty . fmap fst $ fields'
  pure (vm, RConstr () ix . fmap snd $ fields')
  where
    go ::
      forall (m :: Type -> Type).
      MonadError TermError m =>
      Int -> VarMap -> Int -> VarMap -> m VarMap
    go len acc ix = vmMergeM mergeConstr acc . vmMap (toConstr len ix)

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
  vm <- NEVector.ifoldM (go len) vmScrutExtended . fmap fst $ handlers'
  pure (vm, RCase () tscrut . fmap snd $ handlers')
  where
    go ::
      forall (m :: Type -> Type).
      MonadError TermError m => Int -> VarMap -> Int -> VarMap -> m VarMap
    go len acc ix = vmMergeM mergeApplyCase acc . vmMap (toCase len ix)

-- Helpers

toCase :: Int -> Int -> PosTree -> PosTree
toCase len ix pt = PApplyCase Nothing . NEVector.generate1 len $ \ix' -> if ix == ix' then Just pt else Nothing

toConstr :: Int -> Int -> PosTree -> PosTree
toConstr len ix pt = PMany . Vector.generate len $ \ix' -> if ix == ix' then Just pt else Nothing

mergeConstr ::
  forall (m :: Type -> Type).
  MonadError TermError m =>
  WhenMatched m Word64 PosTree PosTree PosTree
mergeConstr = zipWithAMatched $ \k v1 v2 -> case v1 of
  PMany xs -> case v2 of
    PMany ys -> do
      let combined = Vector.zipWith maybeToCan xs ys
      PMany <$> traverse (mergeCanM (BadMergeConstr k v1 v2)) combined
    _ -> throwError . BadMergeConstr k v1 $ v2
  _ -> throwError . BadMergeConstr k v1 $ v2

mergeTwo ::
  forall (m :: Type -> Type).
  MonadError TermError m =>
  WhenMatched m Word64 PosTree PosTree PosTree
mergeTwo = zipWithAMatched $ \k v1 v2 -> case v1 of
  PTwo (This t1) -> case v2 of
    PTwo (That t2) -> pure . PTwo . These t1 $ t2
    _ -> throwError . BadMergeLet k v1 $ v2
  _ -> throwError . BadMergeLet k v1 $ v2

mergeApplyCase ::
  forall (m :: Type -> Type).
  MonadError TermError m =>
  WhenMatched m Word64 PosTree PosTree PosTree
mergeApplyCase = zipWithAMatched $ \k v1 v2 -> case v1 of
  PApplyCase func1 args1 -> case v2 of
    PApplyCase func2 args2 -> do
      let funcs = maybeToCan func1 func2
      let args = NEVector.zipWith maybeToCan args1 args2
      func <- mergeCanM (BadMergeApplyCase k v1 v2) funcs
      args' <- traverse (mergeCanM (BadMergeApplyCase k v1 v2)) args
      pure . PApplyCase func $ args'
    _ -> throwError . BadMergeApplyCase k v1 $ v2
  _ -> throwError . BadMergeApplyCase k v1 $ v2

mergeExtendApply ::
  forall (m :: Type -> Type).
  MonadError TermError m =>
  WhenMatched m Word64 PosTree PosTree PosTree
mergeExtendApply = zipWithAMatched $ \k v1 v2 -> case v1 of
  PApplyCase func args -> pure . PApplyCase func . NEVector.snoc args . Just $ v2
  _ -> throwError . BadMergeApplyExtend k v1 $ v2

freshAndIncrement ::
  forall (m :: Type -> Type) (a :: Type).
  (MonadState a m, Num a) =>
  m a
freshAndIncrement = do
  fresh <- get
  modify (+ 1)
  pure fresh

maybeToCan ::
  forall (a :: Type) (b :: Type).
  Maybe a -> Maybe b -> Can a b
maybeToCan x y = case x of
  Nothing -> maybe Non Eno y
  Just x' -> case y of
    Nothing -> One x'
    Just y' -> Two x' y'

mergeCanM ::
  forall (e :: Type) (m :: Type -> Type) (a :: Type).
  MonadError e m =>
  e ->
  Can a a ->
  m (Maybe a)
mergeCanM err = \case
  Non -> pure Nothing
  One x -> pure . Just $ x
  Eno x -> pure . Just $ x
  Two _ _ -> throwError err
