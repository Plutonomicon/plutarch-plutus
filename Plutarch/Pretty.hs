{-# LANGUAGE PatternSynonyms #-}

module Plutarch.Pretty (prettyTerm, prettyScript) where

import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.ST (runST)
import Control.Monad.State (MonadState (get, put), StateT (runStateT), modify, modify')
import Data.Foldable (fold)
import Data.Functor (($>), (<&>))
import Data.Traversable (for)

import System.Random.Stateful (mkStdGen, newSTGenM)

import Prettyprinter ((<+>))
import qualified Prettyprinter as PP

import Plutarch.Internal (ClosedTerm, compile)
import Plutus.V1.Ledger.Scripts (Script (unScript))
import qualified PlutusCore as PLC
import UntypedPlutusCore (
  DeBruijn (DeBruijn),
  DefaultFun,
  DefaultUni,
  Program (_progTerm),
  Term (Apply, Builtin, Constant, Delay, Error, Force, LamAbs, Var),
 )

import Plutarch.Pretty.Internal.BuiltinConstant (prettyConstant)
import Plutarch.Pretty.Internal.Config (indentWidth)
import Plutarch.Pretty.Internal.Name (freshVarName, smartName)
import Plutarch.Pretty.Internal.TermUtils (
  unwrapApply,
  unwrapBindings,
  unwrapLamAbs,
  pattern IfThenElseLikeAST,
 )
import Plutarch.Pretty.Internal.Types (
  PrettyCursor (AppliedOver, Applying, Normal, UnaryArg),
  PrettyMonad,
  PrettyState (PrettyState, ps'cursor, ps'nameMap),
  appliedCursor,
  applyingCursor,
  builtinFunAtRef,
  forkState,
  insertBindings,
  insertName,
  nameOfRef,
  normalizeCursor,
  unaryCursor,
 )

prettyScript :: Script -> PP.Doc ()
prettyScript = prettyUPLC . _progTerm . unScript

prettyTerm :: ClosedTerm a -> PP.Doc ()
prettyTerm x = prettyScript $ compile x

{- This isn't suitable for pretty printing UPLC from any source. It's primarily suited for Plutarch output.
Practically speaking though, it should work with any _idiomatic_ UPLC.
-}
prettyUPLC :: Term DeBruijn DefaultUni DefaultFun () -> PP.Doc ()
prettyUPLC uplc = runST $ do
  stGen <- newSTGenM $ mkStdGen 42
  (doc, _) <- runReaderT (go uplc) stGen `runStateT` PrettyState mempty mempty Normal
  pure doc
  where
    prettyUnaryParens unaryOp t = modify normalizeCursor *> go t <&> (\x -> unaryOp <> PP.parens x)

    go :: Term DeBruijn DefaultUni DefaultFun () -> PrettyMonad s (PP.Doc ())
    go (Constant _ c) = pure $ prettyConstant c
    go (Builtin _ b) = pure $ PP.pretty b
    go (Error _) = pure "ERROR"
    go (Var _ (DeBruijn x)) = do
      PrettyState {ps'nameMap} <- get
      pure $ case nameOfRef x ps'nameMap of
        Just nm -> PP.pretty nm
        Nothing -> error "impossible: free variable"
    go (IfThenElseLikeAST (Force () (Builtin () PLC.IfThenElse)) cond trueBranch falseBranch) = do
      prettyIfThenElse (forkState . go) cond trueBranch falseBranch
    go ast@(IfThenElseLikeAST scrutinee cond trueBranch falseBranch) = do
      PrettyState {ps'nameMap} <- get
      case scrutinee of
        Var () (DeBruijn (builtinFunAtRef ps'nameMap -> Just PLC.IfThenElse)) ->
          prettyIfThenElse (forkState . go) cond trueBranch falseBranch
        _ -> case ast of
          Force _ t@Apply {} -> prettyUnaryParens "!" t
          _ -> error "impossible: IfThenElseLikeAST"
    go (Force _ t@Apply {}) = prettyUnaryParens "!" t
    go (Force _ t@LamAbs {}) = prettyUnaryParens "!" t
    go (Force _ t) = modify unaryCursor *> go t <&> ("!" <>)
    go (Delay _ t@Apply {}) = prettyUnaryParens "~" t
    go (Delay _ t@LamAbs {}) = prettyUnaryParens "~" t
    go (Delay _ t) = modify unaryCursor *> go t <&> ("~" <>)
    go (LamAbs _ _ t') = do
      currState <- get
      let (depth, bodyTerm) = unwrapLamAbs 0 t'
      names <- traverse (const freshVarName) [0 .. depth]
      -- Add all the new names to the nameMap, starting with 0 index.
      put $ insertBindings names currState
      modify' normalizeCursor
      funcBody <- forkState $ go bodyTerm
      pure . PP.parens . PP.hang indentWidth $
        PP.sep
          [ "\\" <> PP.hsep (reverse $ map PP.pretty names) <+> "->"
          , funcBody
          ]
    go (Apply _ (LamAbs _ _ t) firstArg) = do
      let (restArgs, coreF) = unwrapBindings [] t
          helper (name, expr) = do
            modify' normalizeCursor
            valueDoc <- forkState $ go expr
            pure . PP.hang indentWidth $
              PP.sep
                [ PP.pretty name <+> "="
                , valueDoc
                ]
      firstName <- smartName firstArg
      firstBindingDoc <- helper (firstName, firstArg)
      modify' $ insertName firstName
      restBindingDoc <- fmap fold . for (reverse restArgs) $ \argExpr -> do
        newName <- smartName argExpr
        bindingDoc <- helper (newName, argExpr)
        modify' (insertName newName) $> PP.flatAlt PP.hardline "; " <> bindingDoc
      modify' normalizeCursor
      coreExprDoc <- go coreF
      pure $
        PP.align $
          PP.vsep
            [ "let" <+> PP.align (firstBindingDoc <> restBindingDoc)
            , "in" <+> coreExprDoc
            ]
    go (Apply _ t arg) = do
      PrettyState {ps'cursor} <- get
      let (l, f) = unwrapApply [] t
          args = l <> [arg]
      functionDoc <- forkState $ modify' applyingCursor *> go f
      argsDoc <- modify' appliedCursor *> traverse (forkState . go) args
      (if ps'cursor == AppliedOver then pure . PP.parens else pure) $
        PP.hang indentWidth $ PP.sep $ functionDoc : argsDoc

prettyIfThenElse ::
  (t -> PrettyMonad s (PP.Doc ann)) ->
  t ->
  t ->
  t ->
  PrettyMonad s (PP.Doc ann)
prettyIfThenElse cont cond trueBranch falseBranch = do
  PrettyState {ps'cursor} <- get
  modify' normalizeCursor
  condAst <- cont cond
  trueAst <- cont trueBranch
  falseAst <- cont falseBranch
  (if ps'cursor `elem` [Applying, AppliedOver, UnaryArg] then pure . PP.parens else pure) $
    PP.hang indentWidth $ PP.vsep ["if" <+> condAst, "then" <+> trueAst, "else" <+> falseAst]
