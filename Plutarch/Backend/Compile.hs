{-# LANGUAGE NoOverloadedLists #-}

{- | Generates 'UPLCTerm' from an 'ANF'.

@since wip
-}
module Plutarch.Backend.Compile (
  toUPLCTerm,
) where

import Control.Monad (foldM, when)
import Control.Monad.RWS.CPS (
  MonadState (get),
  RWS,
  asks,
  evalRWS,
  modify,
  runRWS,
 )
import Control.Monad.Reader (MonadReader)
import Control.Monad.ST (runST)
import Control.Monad.State.Strict (gets, put, runStateT)
import Data.Foldable (foldl', for_)
import Data.Kind (Type)
import Data.List.NonEmpty qualified as NEList
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NESet
import Data.Text (Text)
import Data.Vector qualified as Vector
import Data.Vector.Mutable (PrimMonad)
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
  BoundVar (BoundVar),
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
import Plutarch.Helpers.Backend (getFresh)
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
      compileState = CompileState Map.empty Map.empty
   in -- Use our demand analysis to compile everything.
      runCompileM compile compileEnv compileState
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

findIdentity :: NonEmptyVector (ANFBind Demand) -> Maybe Id
findIdentity binds = Id <$> NEVector.findIndex go binds
  where
    go :: ANFBind Demand -> Bool
    go = \case
      ANFLam _ bvs (AVar h') -> case NEVector.uncons bvs of
        (Just (BoundVar h _), rest) -> Vector.null rest && h == h'
        _ -> False
      _ -> False

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

mkFixpointNames ::
  Map Int (PLC.Name, PLC.Name) ->
  Int ->
  RWS (Set Int) () Int (Map Int (PLC.Name, PLC.Name))
mkFixpointNames acc i = do
  freshForM <- untilM getFresh (asks . Set.notMember)
  freshForFunctional <- untilM getFresh (asks . Set.notMember)
  let names = (mkName "mArg" freshForM, mkName "functionalArg" freshForFunctional)
  pure . Map.insert i names $ acc

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

mkCompositionName ::
  Map Int PLC.Name ->
  Int ->
  RWS (Set Int) () Int (Map Int PLC.Name)
mkCompositionName acc i = do
  freshForZ <- untilM getFresh (asks . Set.notMember)
  let name = mkName "compArg" freshForZ
  pure . Map.insert i name $ acc

-- A read-only environment for compilation.
data CompileEnv = CompileEnv
  { -- All ANF binds, with demand analysis, together with their unique names
    ceBinds :: NonEmptyVector (PLC.Name, ANFBind Demand)
  , -- Unique name pairs for each fixpoint we have to compile
    ceFPNameMap :: Map Int (PLC.Name, PLC.Name)
  , -- A unique name for each composition we have to compile
    ceCompNameMap :: Map Int PLC.Name
  , -- A unique name for unused function parameters
    ceUnusedParamName :: PLC.Name
  , -- Whether the identify functions occurs, and if so, where
    ceTheIdentity :: Maybe Id
  }

data CompileState = CompileState
  { csCache :: Map Id UPLCTerm
  , csBindRequirements :: Map Id (NESet Id)
  }

newtype CompileM (a :: Type) = CompileM (RWS CompileEnv () CompileState a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader CompileEnv
    , MonadState CompileState
    )
    via (RWS CompileEnv () CompileState)

runCompileM ::
  forall (a :: Type).
  CompileM a ->
  CompileEnv ->
  CompileState ->
  a
runCompileM (CompileM comp) env = fst . evalRWS comp env

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

mkName :: Text -> Int -> PLC.Name
mkName t = PLC.Name t . PLC.Unique

compile ::
  forall (m :: Type -> Type).
  (MonadState CompileState m, MonadReader CompileEnv m) =>
  m UPLCTerm
compile = do
  binds <- asks (NEVector.map snd . ceBinds)
  NEVector.imapM_ compileWithCache binds
  let topNodeIx = NEVector.length binds - 1
  -- This cannot 'miss', as we have compiled every node, and the top node is
  -- always the last bind.
  fromJust <$> gets (Map.lookup (Id topNodeIx) . csCache)
  where
    compileWithCache :: Int -> ANFBind Demand -> m ()
    compileWithCache ix bind = do
      -- First, determine if this will be bound at some future stage. If so,
      -- record it now as we're seeing this bind for the first time.
      case getANFBindAnn bind of
        -- This is the top-level node.
        NeverDemanded -> pure ()
        -- This node is always inlined.
        Trivial -> pure ()
        -- If we need this more than once, record this fact.
        Demanded letBindLoc useCount -> when (useCount > 1) (modify (recordLetBind letBindLoc ix))
      -- Check if we have to `let`-bind anything here.
      letBindsRequired <- do
        mBinds <- gets (Map.lookup (Id ix) . csBindRequirements)
        case mBinds of
          Nothing -> pure []
          Just ess -> traverse lookupBindName . NEList.toList . NESet.toList $ ess
      -- Compile the bind, doing any `let`-binds we need, and cache.
      compileAndCache ix letBindsRequired bind

recordLetBind :: Id -> Int -> CompileState -> CompileState
recordLetBind letBindLoc whatToBind = \case
  CompileState cache reqs -> CompileState cache . Map.alter go letBindLoc $ reqs
  where
    go :: Maybe (NESet Id) -> Maybe (NESet Id)
    go =
      Just . \case
        Nothing -> NESet.singleton (Id whatToBind)
        Just ess -> NESet.insert (Id whatToBind) ess

lookupBindName ::
  forall (m :: Type -> Type).
  (MonadReader CompileEnv m, MonadState CompileState m) =>
  Id -> m (PLC.Name, UPLCTerm)
lookupBindName i@(Id asIx) = do
  name <- asks (\env -> fst $ ceBinds env NEVector.! asIx)
  -- Due to the order we compile in, we can never require something to be
  -- `let`-bound before it's been compiled. Thus, this cannot 'miss'.
  code <- gets (fromJust . Map.lookup i . csCache)
  pure (name, code)

compileAndCache ::
  forall (m :: Type -> Type).
  (MonadState CompileState m, MonadReader CompileEnv m) =>
  Int -> [(PLC.Name, UPLCTerm)] -> ANFBind Demand -> m ()
compileAndCache ix requiredLetBinds = \case
  -- Leaves can never require any `let`-bindings, so we just compile and cache
  -- the bind and move on. Furthermore, leaves can never reference anything, so
  -- we don't have to namecheck.
  ANFLeaf ell -> modify (writeToCache (Id ix) . compileLeaf $ ell)
  -- For `force` and `delay`, we have to namecheck their bodies. We also have to
  -- potentially resolve some `let`-binds.
  ANFForce _ body -> do
    bodyCode <- checkCache body
    modify (writeToCache (Id ix) . doLetBinds requiredLetBinds . uplcForce $ bodyCode)
  ANFDelay _ body -> do
    bodyCode <- checkCache body
    modify (writeToCache (Id ix) . doLetBinds requiredLetBinds . uplcDelay $ bodyCode)
  -- Fixpoint nodes require considerable additional work. Given the body `F`, we
  -- want to generate `M (\r -> F (r r))`. We have two names set aside for this:
  -- one for the argument of `M`, the other for `r`. As `M` is small, it's
  -- cheaper to inline than `let`-bind it. We also don't bother `let`-binding
  -- `(\r -> F (r r))`: it is a small computation, and if `F` is unique (up to
  -- alpha renaming), so is `(\r -> F (r r))`.
  --
  -- Furthermore, `let`-bind requirements have to be done carefully. Like with
  -- lambdas, our analysis detects _binding_ sites, which means that we have to
  -- resolve `let`-bindings required here 'around' `F`, not the result!
  ANFFix _ bv body -> do
    -- Might as well resolve `let`-binds _now_.
    bodyCode <- doLetBinds requiredLetBinds <$> checkCache body
    -- We have to translate F to a lambda before doing the rest of the
    -- transform.
    let bodyLam = uplcLam1 (selfToName bv) bodyCode
    -- This cannot 'miss', as we checked before for all fixpoint sites and made
    -- a name pair for each.
    (mArgName, functionalArgName) <- asks (fromJust . Map.lookup ix . ceFPNameMap)
    -- `M = \x -> x x`, using the reserved name.
    let m = uplcMCombinator mArgName
    -- `r`, using the reserved name.
    let funcArg = uplcVar functionalArgName
    -- `r r`
    let funcSelfApp = uplcApply1 funcArg funcArg
    -- Assemble everything.
    let finalCode = uplcApply1 m . uplcLam1 functionalArgName . uplcApply1 bodyLam $ funcSelfApp
    modify (writeToCache (Id ix) finalCode)
  ANFConstr _ tag fields -> do
    fieldCodes <- traverse checkCache fields
    modify (writeToCache (Id ix) . doLetBinds requiredLetBinds . uplcConstr tag $ fieldCodes)
  ANFCase _ scrut handlers -> do
    scrutCode <- checkCache scrut
    handlerCodes <- traverse checkCache handlers
    modify (writeToCache (Id ix) . doLetBinds requiredLetBinds . uplcCase scrutCode $ handlerCodes)
  ANFApply _ f xs -> do
    -- Check if `f` is the identity
    applyingToId <- isTheIdentity f
    xsCode <- traverse checkCache xs
    -- If `f` is the identity, then we already for certain know there's only one
    -- argument, so we can just compile that instead.
    if applyingToId
      then modify (writeToCache (Id ix) . doLetBinds requiredLetBinds . NEVector.head $ xsCode)
      else do
        fCode <- checkCache f
        let xsLen = NEVector.length xsCode
        -- If we have two or fewer applications, we can compile an `app` chain.
        -- However, for three or more, it's more efficient to 'pack' the `xs`
        -- into a `constr`, then `case` on it immediately using `f` as the sole
        -- handler.
        if xsLen <= 2
          then modify (writeToCache (Id ix) . doLetBinds requiredLetBinds . uplcApply fCode $ xsCode)
          else do
            let constrCall = uplcConstr 0 . NEVector.toVector $ xsCode
            let soleHandler = NEVector.singleton fCode
            modify (writeToCache (Id ix) . doLetBinds requiredLetBinds . uplcCase constrCall $ soleHandler)
  -- As with fixpoints, our analysis detects _binding_ sites, we have to
  -- resolve `let`-binds 'around' the _body_ of the lambda, not the lambda
  -- itself!
  ANFLam _ bvs body -> do
    bodyCode <- doLetBinds requiredLetBinds <$> checkCache body
    asBvNames <- NEVector.mapM bvToName bvs
    modify (writeToCache (Id ix) . uplcLam asBvNames $ bodyCode)
  -- For compositions, given components `[f_1, f_2, ... , f_k]`, we want to
  -- generate `\z -> f_1 (f_2 ( ... (f_k z) ... )`. We have a name set aside for
  -- `z` to ensure it's unique.
  ANFCompose _ components -> do
    componentCodes <- traverse checkCache components
    -- This cannot 'miss', as we have checked every site of a composition and
    -- set aside an argument for it specifically.
    compArgName <- asks (fromJust . Map.lookup ix . ceCompNameMap)
    -- We have to fold 'backwards' here, as application order would apply the
    -- _last_ item in the composition first. We could reverse the vector, but
    -- this would require copying it, so we don't.
    let len = NEVector.length componentCodes
    let reverseIxes = [len - 1, len - 2 .. 0]
    let asCompArg = uplcVar compArgName
    let finalCode = foldl' (\acc i -> uplcApply1 (componentCodes NEVector.! i) acc) asCompArg reverseIxes
    modify (writeToCache (Id ix) . uplcLam1 compArgName . doLetBinds requiredLetBinds $ finalCode)

bvToName ::
  forall (m :: Type -> Type).
  MonadReader CompileEnv m =>
  Maybe BoundVar -> m PLC.Name
bvToName = \case
  -- Unused argument, use reserved name
  Nothing -> asks ceUnusedParamName
  Just (BoundVar (Hash h) _) -> pure . mkName "arg" $ h

selfToName :: BoundVar -> PLC.Name
selfToName (BoundVar (Hash h) _) = mkName "arg" h

writeToCache :: Id -> UPLCTerm -> CompileState -> CompileState
writeToCache i code = \case
  CompileState cache reqs -> CompileState (Map.insert i code cache) reqs

compileLeaf :: forall (ann :: Type). Leaf ann -> UPLCTerm
compileLeaf = \case
  LConstant _ c -> uplcConstant c
  LBuiltin _ f -> uplcBuiltin f
  LCompiled _ code -> code
  LError _ -> uplcError

checkCache ::
  forall (m :: Type -> Type).
  (MonadReader CompileEnv m, MonadState CompileState m) =>
  Ref -> m UPLCTerm
checkCache = \case
  -- Variables always inline, so we don't even need to check the cache for them.
  AVar (Hash h) -> pure . uplcVar . mkName "arg" $ h
  -- Depending on the demand analysis, we might need to refer to this code by
  -- name instead of by its literal compilation. We check this first.
  AnId asId@(Id i) -> do
    (name, bind) <- asks (\env -> ceBinds env NEVector.! i)
    -- Due to compilation order (from dependencies to dependents), we can never
    -- require code of a subcomputation that we haven't already compiled. Thus,
    -- this cannot 'miss'.
    code <- gets (fromJust . Map.lookup asId . csCache)
    pure $ case getANFBindAnn bind of
      Demanded _ useCount ->
        if useCount > 1
          -- Use name
          then uplcVar name
          -- Use code
          else code
      -- Use code unconditionally
      _ -> code

doLetBinds :: [(PLC.Name, UPLCTerm)] -> UPLCTerm -> UPLCTerm
doLetBinds requiredLetBinds t =
  foldl' (\acc (name, bind) -> uplcLet name bind acc) t requiredLetBinds

isTheIdentity ::
  forall (m :: Type -> Type).
  MonadReader CompileEnv m =>
  Ref -> m Bool
isTheIdentity = \case
  AnId i -> asks ((== Just i) . ceTheIdentity)
  _ -> pure False
