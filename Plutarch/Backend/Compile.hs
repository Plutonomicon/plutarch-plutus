{-# LANGUAGE NoOverloadedLists #-}

{- | Generates 'UPLCTerm' from an 'ANF'.

@since wip
-}
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
import Control.Monad.Reader (MonadReader, runReaderT)
import Control.Monad.ST (runST)
import Control.Monad.State.Strict (put, runStateT)
import Data.Foldable (foldl', for_)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Vector (MVector)
import Data.Vector qualified as Vector
import Data.Vector.Mutable (PrimMonad, PrimState)
import Data.Vector.Mutable qualified as MVector
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEVector
import Plutarch.Backend.ANF (
  ANF (ANF),
  ANFBind (
    ANFApply,
    ANFCase,
    ANFCompose,
    ANFConstr,
    ANFDelay,
    ANFFix,
    ANFForce,
    ANFLam,
    ANFLeaf
  ),
  Demand (Demanded, NeverDemanded, Trivial),
  Id (Id),
  Leaf (
    LBuiltin,
    LCompiled,
    LConstant,
    LError
  ),
  Ref (AVar, AnId),
  getANFBindAnn,
 )
import Plutarch.Backend.AST (
  Hash (Hash),
  Multiplicity (MultiplicityMany, MultiplicityOne),
 )
import Plutarch.Backend.UPLC (
  UPLCTerm,
  rewriteUniques,
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
toUPLCTerm :: ANF Demand -> UPLCTerm
toUPLCTerm (ANF _ binds) =
  -- We use the hashes of any variable as its `Unique`. To ensure we don't
  -- accidentally ever alias them, we collect all the ones we use. As we know
  -- they can't collide, we don't perform any rehashing.
  let allVarUniques = NEVector.foldl' collectVarName Set.empty binds
      -- If we have any chunks of precompiled code, some of their variable
      -- `Unique`s might clash with ours. We can use rehashing to fix this.
      (rewrittenBinds, usedNames) = fixPrecompiled binds allVarUniques
      -- Look for the identity function if it exists
      mIdentity = findIdentity rewrittenBinds
      -- Check how many fixpoints we have and where they are.
      fixpoints = doFixpointAnalysis rewrittenBinds
      -- To compile a fixpoint, we take its functional (of the form `F = \self ->
      -- body`) and transform it into `M (\r -> F (r r))`. As M is small, it's
      -- cheaper to inline than bind it. Thus, each unique fixpoint (up to
      -- alpha-equivalent functionals) requires two unique names:
      --
      -- - The argument to its copy of M; and
      -- - The variable name `r` for the transformed functional.
      (fixpointNameMap, lastFresh, _) = runRWS (foldM mkFixpointNames Map.empty . Set.toList $ fixpoints) usedNames 0
      -- Check how many compositions we have and where they are.
      compositions = doCompositionAnalysis rewrittenBinds
      -- To compile a composition of the form `[f_1, f_2, ... , f_k]`, we want
      -- to produce `\z -> f_1 (f_2 (... (f_k z) ...)`. For this, we need a
      -- unique name for `z`.
      (compositionNameMap, lastFresh', _) = runRWS (foldM mkCompositionName Map.empty . Set.toList $ compositions) usedNames lastFresh
      -- Make a unique name for any unused arguments. As lambdas in UPLC are all
      -- arity 1, and we will never use an unused argument, we can generate just
      -- a single name. It's cheaper to do this speculatively.
      (unusedParamName, lastFresh'', _) = runRWS mkUnusedName usedNames lastFresh'
      -- Name every bind, avoiding any names of existing variables.
      namedBinds = fst . evalRWS (NEVector.mapM nameBind rewrittenBinds) usedNames $ lastFresh''
      -- Set up our compilation environment with everything we just put together
      compileEnv = CompileEnv namedBinds fixpointNameMap compositionNameMap unusedParamName mIdentity
   in -- Use our demand analysis to compile everything.
      runST $ runReaderT compile compileEnv
  where
    collectVarName :: Set Int -> ANFBind ann -> Set Int
    collectVarName acc = \case
      ANFLeaf _ -> acc
      ANFForce _ r -> addVar acc r
      ANFDelay _ r -> addVar acc r
      ANFLam _ _ r -> addVar acc r
      ANFFix _ _ r -> addVar acc r
      ANFApply _ fR xsRs -> addVar (NEVector.foldl' addVar acc xsRs) fR
      ANFConstr _ _ fieldsRs -> Vector.foldl' addVar acc fieldsRs
      ANFCase _ scrutR handlersRs -> addVar (NEVector.foldl' addVar acc handlersRs) scrutR
      ANFCompose _ componentRs -> NEVector.foldl' addVar acc componentRs
    addVar :: Set Int -> Ref -> Set Int
    addVar ess = \case
      AVar (Hash h) -> Set.insert h ess
      _ -> ess
    mkUnusedName :: RWS (Set Int) () Int PLC.Name
    mkUnusedName = do
      fresh <- untilM getFresh (asks . Set.notMember)
      pure . mkName "unused" $ fresh
    nameBind :: ANFBind Demand -> RWS (Set Int) () Int (PLC.Name, ANFBind Demand)
    nameBind bind = do
      fresh <- untilM getFresh (asks . Set.notMember)
      pure (mkName "bind" fresh, bind)

-- Helpers

fixPrecompiled ::
  NonEmptyVector (ANFBind Demand) ->
  Set Int ->
  (NonEmptyVector (ANFBind Demand), Set Int)
fixPrecompiled binds usedNames = runST $ runStateT go usedNames
  where
    go ::
      forall (m :: Type -> Type).
      (PrimMonad m, MonadState (Set Int) m) =>
      m (NonEmptyVector (ANFBind Demand))
    go = do
      let len = NEVector.length binds
      mv <- MVector.new len
      for_ [0, 1 .. len - 1] $ \i -> do
        case binds NEVector.! i of
          x@(ANFLeaf ell) -> case ell of
            -- If we see any compiled code, we rewrite all of its variable
            -- uniques if any clash with ones we've chosen.
            LCompiled ann code -> do
              used <- get
              let (code', used') = rewriteUniques code used
              put used'
              MVector.write mv i . ANFLeaf . LCompiled ann $ code'
            -- Any other leaf can be left as-is.
            _ -> MVector.write mv i x
          -- Anything else doesn't need any changes.
          x -> MVector.write mv i x
      v <- Vector.unsafeFreeze mv
      pure . NEVector.unsafeFromVector $ v

compile ::
  forall (m :: Type -> Type).
  (MonadReader CompileEnv m, PrimMonad m) =>
  m UPLCTerm
compile = do
  binds <- asks ceBinds
  let len = NEVector.length binds
  -- Mutable compile cache. This stores (and incrementally updates), for each
  -- bind:
  --
  -- \* When it is first demanded (as an index, -1 means 'never demanded')
  -- \* Its name (if it should be referred to via variable)
  -- \* Its code
  --
  -- Because we compile from dependencies to dependents, we can do this in one
  -- pass: we can never be forced to compile a dependency before all its
  -- dependents are already cached.
  mv <- MVector.new len
  NEVector.imapM_ (compileWithCache mv) binds
  (\(_, _, x) -> x) <$> MVector.read mv (len - 1)

compileWithCache ::
  forall (m :: Type -> Type).
  (PrimMonad m, MonadReader CompileEnv m) =>
  MVector (PrimState m) (Int, Maybe PLC.Name, UPLCTerm) ->
  Int ->
  (PLC.Name, ANFBind Demand) ->
  m ()
compileWithCache cache i (bindName, bind) = do
  -- We only store the name of a compiled bind if we need to `let`-bind it
  -- later.
  let (firstDemanded, mName) = case getANFBindAnn bind of
        -- Top-level node, nothing to do.
        NeverDemanded -> (-1, Nothing)
        -- Something we should always inline.
        Trivial -> (-1, Nothing)
        -- Check use count: if it's greater than 1, we have to let-bind;
        -- otherwise, we inline.
        Demanded (Id j) useCount ->
          if useCount <= 1
            then (j, Nothing)
            else (j, Just bindName)
  (toBind, code) <- compileBind cache i bind
  -- In some cases (most notably compositions), we can end up with duplicate
  -- `let`-bind requests. Thus, we first isolate uniquely named binds before
  -- doing them.
  let uniqueBinds = Map.fromList toBind
  let codeWithBinds = Map.foldlWithKey' doLetBind code uniqueBinds
  -- We only store the name of a compiled bind if we'd need to `let`-bind it
  -- later.
  MVector.write cache i (firstDemanded, mName, codeWithBinds)

compileBind ::
  forall (m :: Type -> Type).
  (PrimMonad m, MonadReader CompileEnv m) =>
  MVector (PrimState m) (Int, Maybe PLC.Name, UPLCTerm) ->
  Int ->
  ANFBind Demand ->
  m ([(PLC.Name, UPLCTerm)], UPLCTerm)
compileBind cache i = \case
  ANFLeaf ell -> pure ([], compileLeaf ell)
  ANFForce _ body -> do
    (firstDemandedBody, mNameBody, codeBody) <- checkCache cache body
    case mNameBody of
      Nothing -> pure ([], uplcForce codeBody)
      Just nameBody -> do
        let forceBinds = [(nameBody, codeBody) | firstDemandedBody == i]
        pure (forceBinds, uplcForce . uplcVar $ nameBody)
  ANFDelay _ body -> do
    (firstDemandedBody, mNameBody, codeBody) <- checkCache cache body
    case mNameBody of
      Nothing -> pure ([], uplcForce codeBody)
      Just nameBody -> do
        let delayBinds = [(nameBody, codeBody) | firstDemandedBody == i]
        pure (delayBinds, uplcDelay . uplcVar $ nameBody)
  ANFFix _ _ body -> do
    (firstDemandedBody, mNameBody, codeBody) <- checkCache cache body
    -- For fixed points, given the body `F`, we want to generate `M (\r -> F (r
    -- r))`. We have two names set aside for this: one for the argument of `M`,
    -- the other for `r`. As `M` is small, it's cheaper to inline than
    -- `let`-bind it. We also don't bother `let`-binding `(\r -> F (r r))`: it
    -- is a small computation, and if `F` is unique (up to alpha renaming), so
    -- is `(\r -> F (r r))`.
    (mArgName, functionalArgName) <- asks (fromJust . Map.lookup i . ceFPNameMap)
    -- `M = \x -> x x`, using the reserved name.
    let m = uplcMCombinator mArgName
    -- `r`, using the reserved name.
    let funcArg = uplcVar functionalArgName
    -- `r r`
    let funcSelfApp = uplcApply1 funcArg funcArg
    -- Helper for code generation for fixpoint
    let mkFixBody code = uplcApply1 m . uplcLam1 functionalArgName . uplcApply1 code $ funcSelfApp
    case mNameBody of
      -- Generate `M (\r -> F (r r))`
      Nothing -> pure ([], mkFixBody codeBody)
      Just nameBody -> do
        let fixBinds = [(nameBody, codeBody) | firstDemandedBody == i]
        pure (fixBinds, mkFixBody (uplcVar nameBody))
  ANFConstr _ tag fields -> do
    fields' <- traverse (checkCache cache) fields
    let constrBinds = Vector.toList . Vector.mapMaybe (toBind i) $ fields'
    let constrArgs = Vector.map (\(_, mName, code) -> maybe code uplcVar mName) fields'
    pure (constrBinds, uplcConstr tag constrArgs)
  ANFCase _ scrut handlers -> do
    (firstDemandedScrut, mNameScrut, codeScrut) <- checkCache cache scrut
    handlers' <- traverse (checkCache cache) handlers
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
    applyingToId <- isTheIdentity f
    if applyingToId
      -- Since applying anything to the identity is just itself, and we already
      -- know for certain that there's exactly one argument, we can just compile
      -- it instead.
      then do
        (firstDemandedX, mNameX, codeX) <- checkCache cache . NEVector.head $ xs
        let (mXBind, xArg) = case mNameX of
              Nothing -> (Nothing, codeX)
              Just xName ->
                if firstDemandedX == i
                  then (Just (xName, codeX), uplcVar xName)
                  else (Nothing, uplcVar xName)
        pure (maybeToList mXBind, xArg)
      else do
        (firstDemandedF, mNameF, codeF) <- checkCache cache f
        xs' <- traverse (checkCache cache) xs
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
          -- 'Pack' everything into a `constr`, then `case` it immediately using `f`
          -- as the sole handler.
          else do
            let constrCall = uplcConstr 0 . NEVector.toVector $ xsArgs
            let soleHandler = NEVector.singleton fArg
            pure (applyBinds, uplcCase constrCall soleHandler)
  ANFLam _ params body -> do
    (firstDemandedBody, mNameBody, codeBody) <- checkCache cache body
    asParamNames <- NEVector.mapM multToName params
    case mNameBody of
      Nothing -> pure ([], uplcLam asParamNames codeBody)
      Just nameBody -> do
        let lamBinds = [(nameBody, codeBody) | firstDemandedBody == i]
        pure (lamBinds, uplcLam asParamNames . uplcVar $ nameBody)
  ANFCompose _ components -> do
    components' <- traverse (checkCache cache) components
    let componentBinds = Vector.toList . NEVector.mapMaybe (toBind i) $ components'
    let componentArgs = NEVector.map (\(_, mName, code) -> maybe code uplcVar mName) components'
    -- For compositions, given components `[f_1, f_2, .. , f_k]`, we want to
    -- generate `\z -> f_1 (f_2 ( ... (f_k z) ... )`. We have a name set aside
    -- for `z` to ensure it's unique.
    compArgName <- asks (fromJust . Map.lookup i . ceCompNameMap)
    let len = NEVector.length components'
    let body = foldl' (\acc i -> uplcApply1 (componentArgs NEVector.! i) acc) (uplcVar compArgName) [len - 1, len - 2 .. 0]
    pure (componentBinds, uplcLam1 compArgName body)
multToName ::
  forall (m :: Type -> Type).
  MonadReader CompileEnv m =>
  Maybe Multiplicity -> m PLC.Name
multToName = \case
  Nothing -> asks ceUnusedParamName
  Just (MultiplicityOne (Hash h)) -> pure . mkName "arg" $ h
  Just (MultiplicityMany (Hash h)) -> pure . mkName "arg" $ h

doLetBind :: UPLCTerm -> PLC.Name -> UPLCTerm -> UPLCTerm
doLetBind f name v = uplcLet name v f

compileLeaf :: Leaf ann -> UPLCTerm
compileLeaf = \case
  LConstant _ c -> uplcConstant c
  LBuiltin _ f -> uplcBuiltin f
  LCompiled _ code -> code
  LError _ -> uplcError

checkCache ::
  forall (m :: Type -> Type).
  PrimMonad m =>
  MVector (PrimState m) (Int, Maybe PLC.Name, UPLCTerm) ->
  Ref ->
  m (Int, Maybe PLC.Name, UPLCTerm)
checkCache compiled = \case
  -- Variables are always just inlined, so we don't need to even check the
  -- cache for them
  AVar (Hash h) -> pure (-1, Nothing, uplcVar . mkName "arg" $ h)
  AnId (Id j) -> MVector.read compiled j

toBind :: Int -> (Int, Maybe PLC.Name, UPLCTerm) -> Maybe (PLC.Name, UPLCTerm)
toBind i (firstDemanded, mName, code) = do
  name <- mName
  guard (firstDemanded == i)
  pure (name, code)

extendBinds ::
  Int ->
  [(PLC.Name, UPLCTerm)] ->
  (Int, Maybe PLC.Name, UPLCTerm) ->
  [(PLC.Name, UPLCTerm)]
extendBinds i acc (firstDemanded, mName, code) = case mName of
  Nothing -> acc
  Just name ->
    if firstDemanded == i
      then (name, code) : acc
      else acc

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

-- Check every bind to see if it's a composition, and if it is, record its
-- position.
--
-- We need this for two reasons:
--
-- 1. To know if there are any compositions at all; and
-- 2. How many we have, so we can apply the composition transform safely.
doCompositionAnalysis ::
  forall (ann :: Type).
  NonEmptyVector (ANFBind ann) -> Set Int
doCompositionAnalysis = NEVector.ifoldl' go Set.empty
  where
    go :: Set Int -> Int -> ANFBind ann -> Set Int
    go acc pos = \case
      ANFCompose {} -> Set.insert pos acc
      _ -> acc

mkFixpointNames ::
  Map Int (PLC.Name, PLC.Name) ->
  Int ->
  RWS (Set Int) () Int (Map Int (PLC.Name, PLC.Name))
mkFixpointNames acc i = do
  freshForM <- untilM getFresh (asks . Set.notMember)
  freshForFunctional <- untilM getFresh (asks . Set.notMember)
  let names = (mkName "mArg" freshForM, mkName "functionalArg" freshForFunctional)
  pure . Map.insert i names $ acc

mkCompositionName ::
  Map Int PLC.Name ->
  Int ->
  RWS (Set Int) () Int (Map Int PLC.Name)
mkCompositionName acc i = do
  freshForZ <- untilM getFresh (asks . Set.notMember)
  let name = mkName "compArg" freshForZ
  pure . Map.insert i name $ acc

-- A read-only environment for compilation. Contains:
--

-- * All ANF binds (with demand analysis)

-- * All unique names reserved for these binds, in the same order

-- * Unique name pairs for each fixpoint we have to compile

-- * A unique name for each composition we have to compile

-- - A unique name for unused function parameters
data CompileEnv = CompileEnv
  { ceBinds :: NonEmptyVector (PLC.Name, ANFBind Demand)
  , ceFPNameMap :: Map Int (PLC.Name, PLC.Name)
  , ceCompNameMap :: Map Int PLC.Name
  , ceUnusedParamName :: PLC.Name
  , ceTheIdentity :: Maybe Id
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

findIdentity :: NonEmptyVector (ANFBind Demand) -> Maybe Id
findIdentity binds = Id <$> NEVector.findIndex go binds
  where
    go :: ANFBind Demand -> Bool
    go = \case
      ANFLam _ mults r -> case NEVector.uncons mults of
        (arg, rest) -> case fmap (\case MultiplicityOne h -> h; MultiplicityMany h -> h) arg of
          Nothing -> False
          Just h -> case r of
            AVar h' -> Vector.null rest && h == h'
            _ -> False
      _ -> False

isTheIdentity ::
  forall (m :: Type -> Type).
  MonadReader CompileEnv m =>
  Ref -> m Bool
isTheIdentity = \case
  AnId i -> asks ((== Just i) . ceTheIdentity)
  _ -> pure False
