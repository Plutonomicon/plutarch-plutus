{-# LANGUAGE PatternSynonyms #-}

module Plutarch.Pretty.Internal.Name (smartName, freshVarName) where

import Control.Monad.Reader (ask)
import Control.Monad.State (
  get,
  lift,
  modify',
 )
import Data.Functor (($>))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Txt
import Data.Traversable (for)

import System.Random.Stateful (randomRM, uniformRM)

import qualified PlutusCore as PLC
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
  nameLen <- lift . lift $ randomRM (1 :: Int, 7) stGen
  newName <- fmap Txt.pack . for [1 .. nameLen] $ \_ -> do
    chosenIx <- lift . lift $ uniformRM (1, Txt.length chars - 1) stGen
    pure $ Txt.index chars chosenIx
  if Set.member newName existingNames
    then freshVarName
    else modify' (memorizeName newName) $> newName
  where
    chars :: Text
    chars = "abcdefghijkmnprstuvwxyz"
