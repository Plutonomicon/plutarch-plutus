{-# LANGUAGE PatternSynonyms #-}

module Plutarch.Pretty.Internal.Name (smartName, freshVarName) where

import Control.Monad.Reader (ask)
import Control.Monad.State (
  get,
  lift,
  modify',
 )
import Data.Functor (($>))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Txt
import Data.Traversable (for)

import System.Random.Stateful (randomRM, uniformRM)

import PlutusCore qualified as PLC
import UntypedPlutusCore (
  DeBruijn (DeBruijn),
  DefaultFun,
  Term (Builtin, Force, Var),
 )

import Plutarch.Pretty.Internal.Config (forcedPrefix, keywords)
import Plutarch.Pretty.Internal.TermUtils (pattern ComposeAST, pattern PFixAst)
import Plutarch.Pretty.Internal.Types (
  PrettyMonad,
  PrettyState (PrettyState, ps'nameMap, ps'names),
  builtinFunAtRef,
  memorizeName,
 )

smartName :: Term DeBruijn uni DefaultFun () -> PrettyMonad s Text
smartName uplc = do
  PrettyState {ps'nameMap} <- get
  case uplc of
    Force _ (Force _ (Builtin _ b)) -> pure $ forcedPrefix <> Txt.pack (show b)
    Force _ (Builtin _ b) -> pure $ forcedPrefix <> Txt.pack (show b)
    PFixAst -> pure "fix"
    ComposeAST
      (Builtin () PLC.SndPair)
      (Builtin () PLC.UnConstrData) -> pure "unDataSum"
    ComposeAST
      (Var () (DeBruijn (builtinFunAtRef ps'nameMap -> Just PLC.SndPair)))
      (Builtin () PLC.UnConstrData) -> pure "unDataSum"
    _ -> freshVarName

freshVarName :: PrettyMonad s Text
freshVarName = do
  stGen <- ask
  PrettyState {ps'names} <- get
  let existingNames = Set.union ps'names keywords
  nameTailLen <- lift . lift $ randomRM (0 :: Int, 7) stGen
  beginChar <- chooseChar starterChars
  newName <- fmap (Txt.pack . (beginChar :)) . for [0 .. nameTailLen] . const $ chooseChar chars
  if Set.member newName existingNames
    then freshVarName
    else modify' (memorizeName newName) $> newName
  where
    chooseChar x = do
      stGen <- ask
      chosenIx <- lift . lift $ uniformRM (0, Txt.length x - 1) stGen
      pure $ Txt.index x chosenIx
    starterChars = Txt.pack ['a' .. 'z']
    chars = Txt.append starterChars . Txt.pack $ (['A' .. 'Z'] <> (['0' .. '9'] <> ['_', '\'']))
