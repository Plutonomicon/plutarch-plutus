{-# LANGUAGE NoPartialTypeSignatures #-}

{- | Helpers for building up UPLC @Term@s.

@since wip
-}
module Plutarch.Backend.UPLC (
  UPLCTerm (..),
  uplcApply,
  uplcApply1,
  uplcLam,
  uplcLam1,
  uplcLet,
  uplcForce,
  uplcDelay,
  uplcVar,
  uplcConstant,
  uplcBuiltin,
  uplcError,
  uplcConstr,
  uplcCase,
  uplcMCombinator,
  rewriteUniques,
) where

import Control.Monad.State.Strict (
  MonadState,
  gets,
  modify,
  runState,
 )
import Data.Bifunctor (bimap)
import Data.Coerce (coerce)
import Data.Hashable (Hashable (hash))
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEVector
import Data.Word (Word64)
import PlutusCore (Some, ValueOf)
import PlutusCore.Pretty (prettyPlcReadable)
import Prettyprinter (Pretty (pretty))
import UntypedPlutusCore qualified as UPLC

{- | A wrapper around the polymorphism soup that is the UPLC @Term@. This is
both simpler and more concise to work with, and also allows us to provide our
own helpers.

@since wip
-}
newtype UPLCTerm = UPLCTerm (UPLC.Term UPLC.Name UPLC.DefaultUni UPLC.DefaultFun ())
  deriving
    ( -- | @since wip
      Eq
    , -- | @since wip
      Show
    , -- | @since wip
      Hashable
    )
    via (UPLC.Term UPLC.Name UPLC.DefaultUni UPLC.DefaultFun ())

-- | @since wip
instance Pretty UPLCTerm where
  pretty (UPLCTerm t) = prettyPlcReadable t

{- | The @M@ combinator, or @\x -> x@. The argument provides the name of its
variable (in this case, @x@).

@since wip
-}
uplcMCombinator :: UPLC.Name -> UPLCTerm
uplcMCombinator arg = uplcLam1 arg . uplcApply1 (uplcVar arg) . uplcVar $ arg

{- | @'uplcLet' name v f@ produces the equivalent of @(\name -> f) v@.

@since wip
-}
uplcLet :: UPLC.Name -> UPLCTerm -> UPLCTerm -> UPLCTerm
uplcLet varName v f = uplcApply1 (uplcLam1 varName f) v

{- | Arity-1 application.

@since wip
-}
uplcApply1 :: UPLCTerm -> UPLCTerm -> UPLCTerm
uplcApply1 (UPLCTerm f) (UPLCTerm x) = UPLCTerm . UPLC.Apply () f $ x

{- | Arbitrary-arity application.

@since wip
-}
uplcApply :: UPLCTerm -> NonEmptyVector UPLCTerm -> UPLCTerm
uplcApply = NEVector.foldl' uplcApply1

{- | The @force@ from UPLC.

@since wip
-}
uplcForce :: UPLCTerm -> UPLCTerm
uplcForce (UPLCTerm x) = UPLCTerm . UPLC.Force () $ x

{- | The @delay@ from UPLC.

@since wip
-}
uplcDelay :: UPLCTerm -> UPLCTerm
uplcDelay (UPLCTerm x) = UPLCTerm . UPLC.Delay () $ x

{- | A variable with the given name.

@since wip
-}
uplcVar :: UPLC.Name -> UPLCTerm
uplcVar = UPLCTerm . UPLC.Var ()

{- | The given constant.

@since wip
-}
uplcConstant :: Some (ValueOf UPLC.DefaultUni) -> UPLCTerm
uplcConstant = UPLCTerm . UPLC.Constant ()

{- | The given builtin.

@since wip
-}
uplcBuiltin :: UPLC.DefaultFun -> UPLCTerm
uplcBuiltin = UPLCTerm . UPLC.Builtin ()

{- | The canonical error term.

@since wip
-}
uplcError :: UPLCTerm
uplcError = UPLCTerm . UPLC.Error $ ()

{- | A UPLC @constr@.

@since wip
-}
uplcConstr :: Word64 -> Vector UPLCTerm -> UPLCTerm
uplcConstr tag = UPLCTerm . UPLC.Constr () tag . Vector.toList . fmap coerce

{- | A UPLC @case@.

@since wip
-}
uplcCase :: UPLCTerm -> NonEmptyVector UPLCTerm -> UPLCTerm
uplcCase (UPLCTerm scrut) = UPLCTerm . UPLC.Case () scrut . NEVector.toVector . fmap coerce

{- | A lambda of arbitrary arity.

@since wip
-}
uplcLam :: NonEmptyVector UPLC.Name -> UPLCTerm -> UPLCTerm
uplcLam argNames body = NEVector.foldr uplcLam1 body argNames

{- | A lambda of specifically arity 1.

@since wip
-}
uplcLam1 :: UPLC.Name -> UPLCTerm -> UPLCTerm
uplcLam1 varName (UPLCTerm body) = UPLCTerm . UPLC.LamAbs () varName $ body

-- | @since wip
rewriteUniques :: UPLCTerm -> Set Int -> (UPLCTerm, Set Int)
rewriteUniques (UPLCTerm code) used = case runState (go code) (used, Map.empty) of
  (code', (used', _)) -> (UPLCTerm code', used')
  where
    go ::
      forall (m :: Type -> Type).
      MonadState (Set Int, Map Int Int) m =>
      UPLC.Term UPLC.Name UPLC.DefaultUni UPLC.DefaultFun () ->
      m (UPLC.Term UPLC.Name UPLC.DefaultUni UPLC.DefaultFun ())
    go = \case
      t@(UPLC.Var () (UPLC.Name name (UPLC.Unique uniq))) -> do
        -- Should we rewrite this?
        mRewrite <- gets (Map.lookup uniq . snd)
        case mRewrite of
          Just uniq' -> pure . UPLC.Var () . UPLC.Name name . UPLC.Unique $ uniq'
          -- Does this clash?
          Nothing -> do
            clashes <- gets (Set.member uniq . fst)
            if clashes
              then do
                -- Rehash the current 'unique' until it no longer clashes
                uniq' <- doUntilM uniq (\x -> pure . hash $ (1 :: Int, x)) (\hash' -> gets (Set.notMember hash' . fst))
                -- Store the rewrite, and note that the new unique is used
                modify (bimap (Set.insert uniq') (Map.insert uniq uniq'))
                -- Apply the rewrite
                pure . UPLC.Var () . UPLC.Name name . UPLC.Unique $ uniq'
              else pure t
      UPLC.LamAbs () allName@(UPLC.Name name (UPLC.Unique uniq)) body -> do
        body' <- go body
        -- Should we rewrite our bound var?
        mRewrite <- gets (Map.lookup uniq . snd)
        pure $ case mRewrite of
          Just uniq' -> UPLC.LamAbs () (UPLC.Name name . UPLC.Unique $ uniq') body'
          Nothing -> UPLC.LamAbs () allName body'
      UPLC.Apply () f x -> UPLC.Apply () <$> go f <*> go x
      UPLC.Force () body -> UPLC.Force () <$> go body
      UPLC.Delay () body -> UPLC.Delay () <$> go body
      UPLC.Constant () c -> pure . UPLC.Constant () $ c
      UPLC.Builtin () f -> pure . UPLC.Builtin () $ f
      UPLC.Error () -> pure . UPLC.Error $ ()
      UPLC.Constr () tag fields -> UPLC.Constr () tag <$> traverse go fields
      UPLC.Case () scrut handlers -> UPLC.Case () <$> go scrut <*> traverse go handlers

doUntilM ::
  forall (a :: Type) (m :: Type -> Type).
  Monad m =>
  a ->
  (a -> m a) ->
  (a -> m Bool) ->
  m a
doUntilM x act cond = do
  res <- act x
  stop <- cond res
  if stop then pure res else doUntilM res act cond
