{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeData #-}

module Plutarch.Backend.Term (
  RawTerm (..),
  TermEnv (..),
  Term (..),
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
) where

import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.RWS.CPS (
  MonadState,
  RWS,
  get,
  modify,
 )
import Data.Kind (Type)
import Data.String (fromString)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEVector
import Data.Word (Word64)
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import Plutarch.Backend.Arity (Arity (Arity, NoArity), getBuiltinArity)
import Plutarch.Backend.UPLC (UPLCTerm)
import PlutusCore (Some, ValueOf)
import PlutusCore qualified as PLC

data VarTag
  = Argument
  | LetBinding
  deriving stock (Show, Eq)

data RawTerm
  = RVar VarTag Word64
  | RLamAbs (NonEmptyVector Word64) RawTerm
  | RApply RawTerm (NonEmptyVector RawTerm)
  | RForce RawTerm
  | RDelay RawTerm
  | RConstant (Some (ValueOf PLC.DefaultUni))
  | RBuiltin PLC.DefaultFun
  | RCompiled UPLCTerm
  | RError
  | RPlaceholder Integer
  | RConstr Word64 (Vector RawTerm)
  | RCase RawTerm (Vector RawTerm)
  | RLet RawTerm RawTerm
  deriving stock (Show, Eq)

type data S

data TermEnv = TermEnv

newtype Term (s :: S) (a :: S -> Type)
  = Term {asRawTerm :: ExceptT Text (RWS TermEnv () Word64) RawTerm}

type role Term nominal nominal

newtype (:-->) (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PLam (Term s a -> Term s b)

infixr 0 :-->

data PDelayed (a :: S -> Type) (s :: S)

plam' ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  (Term s a -> Term s b) -> Term s (a :--> b)
plam' = plamWith Argument

plet ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s a -> (Term s a -> Term s b) -> Term s b
plet v f = Term $ do
  vt <- asRawTerm v
  let doInline = asRawTerm (f v)
  case vt of
    -- Inline single variables
    RVar _ _ -> doInline
    -- Inline builtins
    RBuiltin _ -> doInline
    -- If we have an error in the body, propagate it.
    RError -> pure RError
    -- Otherwise, perform the lambda transform.
    _ -> do
      ft <- asRawTerm (plamWith LetBinding f)
      case ft of
        RError -> pure RError
        RLamAbs (NVUncons ix VNil) (RVar _ ix') ->
          if ix == ix'
            then asRawTerm v
            else pure . RLet vt $ ft
        RApply f' xs -> pure . RApply f' . NEVector.cons vt $ xs
        _ -> pure . RLet vt $ ft

pthrow ::
  forall (a :: S -> Type) (s :: S).
  HasCallStack => Text -> Term s a
pthrow msg = Term . throwError $ fromString (prettyCallStack callStack) <> "\n\n" <> msg

papp ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  Term s (a :--> b) -> Term s a -> Term s b
papp f x = Term $ do
  ft <- asRawTerm f
  xt <- asRawTerm x
  case (ft, xt) of
    -- Applying anything to an error is an error.
    (RError, _) -> pure RError
    -- Applying an error to anything is an error.
    (_, RError) -> pure RError
    -- Applying anything to `id` does nothing.
    (RLamAbs (NVUncons ix VNil) (RVar _ ix'), _) ->
      if ix == ix'
        then asRawTerm x
        else pure (RApply ft . NEVector.singleton $ xt)
    -- If we have an existing application, extend it by one arg.
    (RApply func args, _) -> pure . RApply func . NEVector.cons xt $ args
    -- Otherwise, build a new application.
    _ -> pure (RApply ft . NEVector.singleton $ xt)

pdelay ::
  forall (a :: S -> Type) (s :: S).
  Term s a -> Term s (PDelayed a)
pdelay = Term . fmap RDelay . asRawTerm

pforce ::
  forall (a :: S -> Type) (s :: S).
  Term s (PDelayed a) -> Term s a
pforce = Term . fmap go . asRawTerm
  where
    go :: RawTerm -> RawTerm
    go = \case
      RDelay t -> t
      t -> RForce t

perror :: forall (a :: S -> Type) (s :: S). Term s a
perror = Term . pure $ RError

pplaceholder ::
  forall (a :: S -> Type) (s :: S).
  Integer -> Term s a
pplaceholder = Term . pure . RPlaceholder

punsafeCoerce ::
  forall (b :: S -> Type) (a :: S -> Type) (s :: S).
  Term s a -> Term s b
punsafeCoerce = Term . asRawTerm

punsafeBuiltin ::
  forall (a :: S -> Type) (s :: S).
  PLC.DefaultFun -> Term s a
punsafeBuiltin = Term . pure . RBuiltin

punsafeConstantInternal ::
  forall (a :: S -> Type) (s :: S).
  Some (ValueOf PLC.DefaultUni) -> Term s a
punsafeConstantInternal = Term . pure . RConstant

-- Helpers

plamWith ::
  forall (a :: S -> Type) (b :: S -> Type) (s :: S).
  VarTag -> (Term s a -> Term s b) -> Term s (a :--> b)
plamWith tag f = Term $ do
  fresh <- freshAndIncrement
  let varTerm = Term . pure . RVar tag $ fresh
  t <- asRawTerm (f varTerm)
  let soleParam = NEVector.singleton fresh
  let defaultCase = RLamAbs soleParam t
  -- To complete any lambda, we have to perform two further tasks:
  --
  -- \* We may be a wrapper for an 'extension lambda' simulating a higher arity.
  -- \* For efficiency, we can eta-reduce in some specific cases, as this
  --   produces a smaller AST.
  --
  -- We perform both analyses simultaneously below.
  pure $ case t of
    -- Our body just applies our own argument to another function.
    -- We can assume that if `t'` has a known arity at all, that arity must
    -- be 1 exactly.
    --
    -- First, check arity.
    RApply t' (NVUncons (RVar _ ix) VNil) -> case getArity t' of
      -- Nothing to do here.
      NoArity -> defaultCase
      -- Check if our own variable is being used.
      Arity _ ->
        if fresh == ix
          -- We have \x -> f x, so we can reduce to just `f`.
          then t'
          -- Nothing to do.
          else defaultCase
    -- Our body is an 'extension lambda' representing a higher-arity function,
    -- which also immediately calls something else. This is similar to the case
    -- above, but we have to be slightly more careful to make sure that our
    -- expansions are legal.
    --
    -- First, check the arity of whatever we're applying to.
    RLamAbs params (RApply t' args) -> case getArity t' of
      -- Nothing to do.
      NoArity -> defaultCase
      -- If everything being passed to `t'` is either own own argument, or one
      -- of `params`, in the correct order, we can eta-reduce. More precisely,
      -- we must have something of the form for this to 'fire':
      --
      -- \x -> \y1 y2 z -> f x y1 y2 z
      --
      -- which would rewrite to just `f` in this case.
      --
      -- First, check that everything in `args` is a variable.
      Arity _ -> case traverse (\case RVar _ ix -> Just ix; _ -> Nothing) args of
        -- Something being applied to `t'` isn't a variable, so nothing to do.
        Nothing -> defaultCase
        -- Check that
        Just argList ->
          if NEVector.cons fresh params == argList
            -- Eta-reduce.
            then t'
            else defaultCase
    -- Our body is an 'extension lambda' simulating higher arity. We can assume
    -- that this already has the right number of arguments and their names, so
    -- we don't have to recurse any 'lower'.
    RLamAbs params t' -> RLamAbs (NEVector.cons fresh params) t'
    -- In all other situations, nothing to do.
    _ -> defaultCase

freshAndIncrement ::
  forall (m :: Type -> Type) (a :: Type).
  (MonadState a m, Num a) =>
  m a
freshAndIncrement = do
  fresh <- get
  modify (+ 1)
  pure fresh

pattern VNil :: forall (a :: Type). Vector a
pattern VNil <- (Vector.null -> True)

pattern NVUncons :: forall (a :: Type). a -> Vector a -> NonEmptyVector a
pattern NVUncons x xs <- (NEVector.uncons -> (x, xs))

{-# COMPLETE NVUncons #-}

getArity :: RawTerm -> Arity
getArity = \case
  RBuiltin t -> getBuiltinArity t
  RForce (RBuiltin t) -> getBuiltinArity t
  _ -> NoArity
