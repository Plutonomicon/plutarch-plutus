{-# LANGUAGE NoOverloadedLists #-}

module Plutarch.Backend.Compile (
  toUPLCTerm,
) where

import Control.Monad (foldM, guard)
import Control.Monad.RWS.CPS (
  MonadState (get),
  RWS,
  asks,
  evalRWS,
  modify,
  runRWS,
 )
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.ST (ST, runST)
import Data.Foldable (foldl', for_, traverse_)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Vector (MVector)
import Data.Vector qualified as Vector
import Data.Vector.Mutable qualified as MVector
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEVector
import Data.Word (Word64)
import Plutarch.Backend.ANF (
  ANF (ANF),
  ANFBind (
    ANFApply,
    ANFCase,
    ANFConstr,
    ANFDelay,
    ANFFix,
    ANFForce,
    ANFLam,
    ANFLeaf
  ),
  Id (Id),
  Leaf (
    LBuiltin,
    LCompiled,
    LConstant,
    LError
  ),
  Ref (AVar, AnId),
 )
import Plutarch.Backend.AST (
  Hash (Hash),
  Multiplicity (MultiplicityMany, MultiplicityOne),
 )
import Plutarch.Backend.UPLC (
  UPLCTerm,
  uplcApply,
  uplcApply1,
  uplcBuiltin,
  uplcCase,
  uplcConstant,
  uplcConstr,
  uplcDelay,
  uplcError,
  uplcForce,
  uplcLam,
  uplcLam1,
  uplcLet,
  uplcMCombinator,
  uplcVar,
 )
import PlutusCore qualified as PLC

{- | Given an ANF, compile it into UPLC. This compilation also applies automatic
@let@-bindings of any unique (up to alpha-equivalence) computation that is
used more than once.

@since wip
-}
toUPLCTerm :: forall (ann :: Type). ANF ann -> UPLCTerm
toUPLCTerm (ANF _ binds) =
  -- We use the hashes of any variable as its `Unique`. To ensure we don't
  -- accidentally ever alias them, we collect all the ones we use.
  let allVarUniques = NEVector.foldl' collectVarName Set.empty binds
      -- Check how many fixpoints we have and where they are.
      fixpoints = doFixpointAnalysis binds
      -- To compile a fixpoint, we take its functional (of the form `F = \self ->
      -- body`) and transform it into `M (\r -> F (r r))`. As M is small, it's
      -- cheaper to inline than bind it. Thus, each unique fixpoint (up to
      -- alpha-equivalent functionals) requires two unique names:
      --
      -- - The argument to its copy of M; and
      -- - The variable name `r` for the transformed functional.
      (fixpointNameMap, lastFresh, _) = runRWS (foldM mkFixpointNames Map.empty . Set.toList $ fixpoints) allVarUniques 0
      -- Make a unique name for any unused arguments. As lambdas in UPLC are all
      -- arity 1, and we will never use an unused argument, we can generate just
      -- a single name. It's cheaper to do this speculatively.
      (unusedParamName, lastFresh', _) = runRWS mkUnusedName allVarUniques lastFresh
      -- Name every bind, avoiding any names of existing variables.
      len = NEVector.length binds
      bindNames = fst . evalRWS (NEVector.replicate1M len mkBindName) allVarUniques $ lastFresh'
      -- Demand analysis
      demandResult = doDemandAnalysis binds
      -- Set up our compilation environment with everything we just put together
      compileEnv = CompileEnv binds bindNames demandResult fixpointNameMap unusedParamName
   in -- Use our demand analysis to compile everything.
      runST $ runReaderT compile compileEnv
  where
    collectVarName :: Set Int -> ANFBind ann -> Set Int
    collectVarName acc = \case
      ANFLeaf _ -> acc
      ANFForce _ r -> addVar acc r
      ANFDelay _ r -> addVar acc r
      ANFLam _ _ _ r -> addVar acc r
      ANFFix _ _ _ r -> addVar acc r
      ANFApply _ fR xsRs -> addVar (NEVector.foldl' addVar acc xsRs) fR
      ANFConstr _ _ fieldsRs -> Vector.foldl' addVar acc fieldsRs
      ANFCase _ scrutR handlersRs -> addVar (NEVector.foldl' addVar acc handlersRs) scrutR
    addVar :: Set Int -> Ref -> Set Int
    addVar ess = \case
      AVar (Hash h) -> Set.insert h ess
      _ -> ess
    mkUnusedName :: RWS (Set Int) () Int PLC.Name
    mkUnusedName = do
      fresh <- untilM getFresh (asks . Set.notMember)
      pure . mkName "unused" $ fresh
    mkBindName :: RWS (Set Int) () Int PLC.Name
    mkBindName = do
      fresh <- untilM getFresh (asks . Set.notMember)
      pure . mkName "bind" $ fresh

-- Helpers

compile ::
  forall (ann :: Type) (s :: Type).
  ReaderT (CompileEnv ann) (ST s) UPLCTerm
compile = do
  binds <- asks ceBinds
  let len = NEVector.length binds
  mv <- MVector.new len
  NEVector.imapM_ (go mv) binds
  (\(_, _, x) -> x) <$> MVector.read mv (len - 1)
  where
    go ::
      MVector s (Int, Maybe PLC.Name, UPLCTerm) ->
      Int ->
      ANFBind ann ->
      ReaderT (CompileEnv ann) (ST s) ()
    go compiled i bind = do
      (useCount, firstDemanded) <- asks ((NEVector.! i) . ceUseByBind)
      (toBind, code) <- case bind of
        ANFLeaf ell -> pure ([], compileLeaf ell)
        ANFForce _ body -> do
          (firstDemandedBody, mNameBody, codeBody) <- checkCache compiled body
          case mNameBody of
            Nothing -> pure ([], uplcForce codeBody)
            Just nameBody -> do
              let forceBody = uplcForce . uplcVar $ nameBody
              let forceBinds = [(nameBody, codeBody) | firstDemandedBody == i]
              pure (forceBinds, forceBody)
        ANFDelay _ body -> do
          (firstDemandedBody, mNameBody, codeBody) <- checkCache compiled body
          case mNameBody of
            Nothing -> pure ([], uplcDelay codeBody)
            Just nameBody -> do
              let delayBody = uplcDelay . uplcVar $ nameBody
              let delayBinds = [(nameBody, codeBody) | firstDemandedBody == i]
              pure (delayBinds, delayBody)
        ANFFix _ _ _ body -> do
          (firstDemandedBody, mNameBody, codeBody) <- checkCache compiled body
          -- For fixed points, given the body `F`, we want to produce `M
          -- (\r -> F (r r))`. To enable this, we've set aside two
          -- suitable names: one for the argument of `M`, the other for
          -- `r`. Since `M` is small, it's cheaper to inline than
          -- `let`-bind it. We also don't bother `let`-binding `(\r -> F
          -- (r r))`: if `F` is unique (up to alpha renaming), so is
          -- `(\r -> F (r r))`.
          (mArgName, functionalArgName) <- asks (fromJust . Map.lookup i . ceFPNameMap)
          -- `M = \x -> x x` using the reserved name
          let m = uplcMCombinator mArgName
          -- `r` using the reserved name
          let funcArg = uplcVar functionalArgName
          -- `r r`
          let functionalSelfApply = uplcApply1 funcArg funcArg
          case mNameBody of
            -- `M (\r -> F (r r))`
            Nothing -> pure ([], uplcApply1 m . uplcLam1 functionalArgName . uplcApply1 codeBody $ functionalSelfApply)
            -- `let f = F in M (\r -> f (r r))`
            Just nameBody -> do
              let fixBody = uplcApply1 m . uplcLam1 functionalArgName . uplcApply1 (uplcVar nameBody) $ functionalSelfApply
              let fixBinds = [(nameBody, codeBody) | firstDemandedBody == i]
              pure (fixBinds, fixBody)
        ANFConstr _ tag fields -> do
          fields' <- traverse (checkCache compiled) fields
          let constrBinds = Vector.toList . Vector.mapMaybe (toBind i) $ fields'
          let constrArgs = Vector.map (\(_, mName, code) -> maybe code uplcVar mName) fields'
          pure (constrBinds, uplcConstr tag constrArgs)
        ANFCase _ scrut handlers -> do
          (firstDemandedScrut, mNameScrut, codeScrut) <- checkCache compiled scrut
          handlers' <- traverse (checkCache compiled) handlers
          let (mScrutBind, scrutArg) = case mNameScrut of
                Nothing -> (Nothing, codeScrut)
                Just scrutName ->
                  if firstDemandedScrut == i
                    then (Just (scrutName, codeScrut), uplcVar scrutName)
                    else (Nothing, uplcVar scrutName)
          let caseBinds = NEVector.foldl' (extendBinds i) (maybeToList mScrutBind) handlers'
          let handlerArgs = NEVector.map (\(_, mName, code) -> maybe code uplcVar mName) handlers'
          pure (caseBinds, uplcCase scrutArg handlerArgs)
        ANFApply _ f xs -> do
          (firstDemandedF, mNameF, codeF) <- checkCache compiled f
          xs' <- traverse (checkCache compiled) xs
          let (mFBind, fArg) = case mNameF of
                Nothing -> (Nothing, codeF)
                Just fName ->
                  if firstDemandedF == i
                    then (Just (fName, codeF), uplcVar fName)
                    else (Nothing, uplcVar fName)
          let applyBinds = NEVector.foldl' (extendBinds i) (maybeToList mFBind) xs'
          let xsArgs = NEVector.map (\(_, mName, code) -> maybe code uplcVar mName) xs'
          if NEVector.length xs' <= 2
            -- Compile a regular chain of `apply`
            then pure (applyBinds, uplcApply fArg xsArgs)
            -- 'Pack' everything into a `constr`, then `case` it
            -- immediately using `f` as the sole handler
            else do
              let constrCall = uplcConstr 0 . NEVector.toVector $ xsArgs
              let soleHandler = NEVector.singleton fArg
              pure (applyBinds, uplcCase constrCall soleHandler)
        ANFLam _ params _ body -> do
          (firstDemandedBody, mNameBody, codeBody) <- checkCache compiled body
          asParamNames <- NEVector.mapM multToName params
          case mNameBody of
            Nothing -> pure ([], uplcLam asParamNames codeBody)
            Just nameBody -> do
              let lamBody = uplcLam asParamNames . uplcVar $ nameBody
              let lamBinds = [(nameBody, codeBody) | firstDemandedBody == i]
              pure (lamBinds, lamBody)
      let codeWithBinds = foldl' doLetBinds code toBind
      mName <-
        if useCount <= 1
          then pure Nothing
          else Just <$> asks ((NEVector.! i) . ceBindNames)
      MVector.write compiled i (firstDemanded, mName, codeWithBinds)
    compileLeaf :: Leaf ann -> UPLCTerm
    compileLeaf = \case
      LConstant _ c -> uplcConstant c
      LBuiltin _ f -> uplcBuiltin f
      LCompiled _ code -> code
      LError _ -> uplcError
    checkCache ::
      MVector s (Int, Maybe PLC.Name, UPLCTerm) ->
      Ref ->
      ReaderT (CompileEnv ann) (ST s) (Int, Maybe PLC.Name, UPLCTerm)
    checkCache compiled = \case
      AVar (Hash h) -> pure (-1, Nothing, uplcVar . mkName "arg" $ h)
      AnId (Id j) -> MVector.read compiled j
    toBind :: Int -> (Int, Maybe PLC.Name, UPLCTerm) -> Maybe (PLC.Name, UPLCTerm)
    toBind i (firstDemanded, mName, code) = do
      name <- mName
      guard (firstDemanded == i)
      pure (name, code)
    extendBinds :: Int -> [(PLC.Name, UPLCTerm)] -> (Int, Maybe PLC.Name, UPLCTerm) -> [(PLC.Name, UPLCTerm)]
    extendBinds i acc (firstDemanded, mName, code) = case mName of
      Nothing -> acc
      Just name ->
        if firstDemanded == i
          then (name, code) : acc
          else acc
    multToName :: Maybe Multiplicity -> ReaderT (CompileEnv ann) (ST s) PLC.Name
    multToName = \case
      Nothing -> asks unusedParamName
      Just (MultiplicityOne (Hash h)) -> pure . mkName "arg" $ h
      Just (MultiplicityMany (Hash h)) -> pure . mkName "arg" $ h
    doLetBinds :: UPLCTerm -> (PLC.Name, UPLCTerm) -> UPLCTerm
    doLetBinds f (name, v) = uplcLet name v f

-- Check every bind to see if it's a fixpoint, and if it is, record its
-- position.
--
-- We need this for two reasons:
--
-- 1. To know if we have any fixed points at all; and
-- 2. How many we have, so that we can apply the fixpoint transform safely.
doFixpointAnalysis ::
  forall (ann :: Type).
  NonEmptyVector (ANFBind ann) -> Set Int
doFixpointAnalysis = NEVector.ifoldl' go Set.empty
  where
    go :: Set Int -> Int -> ANFBind ann -> Set Int
    go acc pos = \case
      ANFFix {} -> Set.insert pos acc
      _ -> acc

-- For each bind, check:
--
-- - How many times it is required as a direct dependency and
-- - Which bind that requires it is highest-up the dependency chain (that is,
--   closest to the toplevel computation)
doDemandAnalysis ::
  forall (ann :: Type).
  NonEmptyVector (ANFBind ann) -> NonEmptyVector (Word64, Int)
doDemandAnalysis binds = runST $ do
  let len = NEVector.length binds
  -- Note (Koz, 05/06/2026): We're working with a possibly-empty mutable vector
  -- here as currently, there is no way to 'freeze' a mutable non-empty vector.
  mv <- MVector.replicate len (0, -1)
  for_ [0, 1 .. len - 1] $ \i -> case binds NEVector.! i of
    ANFLeaf _ -> pure ()
    ANFForce _ r -> updateWithRef mv i r
    ANFDelay _ r -> updateWithRef mv i r
    ANFLam _ _ _ r -> updateWithRef mv i r
    ANFFix _ _ _ r -> updateWithRef mv i r
    ANFApply _ f xs -> do
      updateWithRef mv i f
      traverse_ (updateWithRef mv i) xs
    ANFConstr _ _ fields -> traverse_ (updateWithRef mv i) fields
    ANFCase _ scrut handlers -> do
      updateWithRef mv i scrut
      traverse_ (updateWithRef mv i) handlers
  v <- Vector.unsafeFreeze mv
  pure . NEVector.unsafeFromVector $ v
  where
    updateWithRef :: MVector s (Word64, Int) -> Int -> Ref -> ST s ()
    updateWithRef mv i = \case
      AnId (Id j) ->
        MVector.modify mv (\(usedTimes, demandedPos) -> (usedTimes + 1, max demandedPos i)) j
      _ -> pure ()

mkFixpointNames ::
  Map Int (PLC.Name, PLC.Name) ->
  Int ->
  RWS (Set Int) () Int (Map Int (PLC.Name, PLC.Name))
mkFixpointNames acc i = do
  freshForM <- untilM getFresh (asks . Set.notMember)
  freshForFunctional <- untilM getFresh (asks . Set.notMember)
  let names = (mkName "mArg" freshForM, mkName "functionalArg" freshForFunctional)
  pure . Map.insert i names $ acc

-- A read-only environment for compilation. Contains:
--
-- - All ANF binds
-- - All unique names reserved for these binds, in the same order
-- - Use counts for each bind
-- - Unique name pairs for each fixpoint we have to compile
-- - A unique name for unused function parameters
data CompileEnv (ann :: Type) = CompileEnv
  { ceBinds :: NonEmptyVector (ANFBind ann)
  , ceBindNames :: NonEmptyVector PLC.Name
  , ceUseByBind :: NonEmptyVector (Word64, Int)
  , ceFPNameMap :: Map Int (PLC.Name, PLC.Name)
  , unusedParamName :: PLC.Name
  }

untilM ::
  forall (a :: Type) (m :: Type -> Type).
  Monad m =>
  m a ->
  (a -> m Bool) ->
  m a
untilM act cond = do
  res <- act
  stop <- cond res
  if stop then pure res else untilM act cond

getFresh :: RWS (Set Int) () Int Int
getFresh = do
  fresh <- get
  modify (+ 1)
  pure fresh

mkName :: Text -> Int -> PLC.Name
mkName t = PLC.Name t . PLC.Unique
