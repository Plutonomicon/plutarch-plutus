{-# LANGUAGE NoOverloadedLists #-}

module Plutarch.Backend.Compile (
  toUPLCTerm,
) where

import Control.Monad (foldM)
import Control.Monad.RWS.CPS (
  MonadReader,
  MonadState (get),
  RWS,
  asks,
  evalRWS,
  gets,
  modify,
  runRWS,
 )
import Control.Monad.ST (ST, runST)
import Data.Foldable (for_, traverse_)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
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
      -- Figure out how many times everything is being used.
      useByBind = doPackratAnalysis binds
      -- Other bits we need
      topBind = NEVector.last binds
      compileEnv = CompileEnv binds bindNames useByBind fixpointNameMap unusedParamName
      compileState = CompileState Map.empty
   in fromRight' . runCompileM (compile (len - 1) topBind) compileEnv $ compileState
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

-- Check how many times any given bind is used. The name refers to packrat
-- parsing: our goal is to figure out what is worth 'stashing' (namely
-- `let`-binding) and what we should inline.
doPackratAnalysis ::
  forall (ann :: Type).
  NonEmptyVector (ANFBind ann) -> NonEmptyVector Word64
doPackratAnalysis binds = runST $ do
  let len = NEVector.length binds
  -- Note (Koz, 05/06/2026): We're working with a possibly-empty mutable vector
  -- here as currently, there is no way to 'freeze' a mutable non-empty vector.
  v <- MVector.replicate len 0
  for_ [0, 1 .. len - 1] $ \i -> case binds NEVector.! i of
    ANFLeaf _ -> pure ()
    ANFForce _ r -> updateWithRef v r
    ANFDelay _ r -> updateWithRef v r
    ANFLam _ _ _ r -> updateWithRef v r
    ANFFix _ _ _ r -> updateWithRef v r
    ANFApply _ f xs -> do
      updateWithRef v f
      traverse_ (updateWithRef v) xs
    ANFConstr _ _ fields -> traverse_ (updateWithRef v) fields
    ANFCase _ scrut handlers -> do
      updateWithRef v scrut
      traverse_ (updateWithRef v) handlers
  v' <- Vector.unsafeFreeze v
  pure . NEVector.unsafeFromVector $ v'
  where
    updateWithRef :: forall (s :: Type). MVector s Word64 -> Ref -> ST s ()
    updateWithRef v = \case
      AnId (Id j) -> MVector.modify v (+ 1) j
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
  , ceUseByBind :: NonEmptyVector Word64
  , ceFPNameMap :: Map Int (PLC.Name, PLC.Name)
  , unusedParamName :: PLC.Name
  }

-- A \'compilation cache\', storing either the code corresponding to any
-- compiled ANF bind, or, if that bind has been @let@-bound, its name
newtype CompileState = CompileState
  { csCache :: Map Id (Either PLC.Name UPLCTerm)
  }

-- An environment for compilation to UPLC, containing a 'CompileEnv' and a
-- 'CompileState'.
newtype CompileM (ann :: Type) (a :: Type)
  = CompileM (RWS (CompileEnv ann) () CompileState a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (CompileEnv ann)
    , MonadState CompileState
    )
    via (RWS (CompileEnv ann) () CompileState)

runCompileM ::
  forall (a :: Type) (ann :: Type).
  CompileM ann a -> CompileEnv ann -> CompileState -> a
runCompileM (CompileM comp) env = fst . evalRWS comp env

-- Given an identifier corresponding to a fixpoint, get the unique names assigned
-- for its `M` and functional.
--
-- We know that we have an entry for every fixpoint in our map, hence the
-- `fromJust` is safe, as `Map.lookup` cannot 'miss'.
getFixNames ::
  forall (ann :: Type).
  Id ->
  CompileM ann (PLC.Name, PLC.Name)
getFixNames (Id k) = asks (fromJust . Map.lookup k . ceFPNameMap)

-- Check if we've already compiled the bind at the given `Id`. If so, produce
-- the cache entry. Otherwise, perform the given action and cache the resulting
-- code.
withCache ::
  forall (ann :: Type).
  Id ->
  CompileM ann UPLCTerm ->
  CompileM ann (Either PLC.Name UPLCTerm)
withCache i act = do
  compiled <- gets (Map.lookup i . csCache)
  case compiled of
    Nothing -> do
      code <- act
      modify (\(CompileState cache) -> CompileState . Map.insert i (Right code) $ cache)
      pure . Right $ code
    Just entry -> pure entry

fromRight' ::
  forall (a :: Type) (b :: Type).
  Either a b -> b
fromRight' = \case
  Left _ -> error "fromRight' : saw Left"
  Right x -> x

compile ::
  forall (ann :: Type).
  Int -> ANFBind ann -> CompileM ann (Either PLC.Name UPLCTerm)
compile pos = \case
  ANFLeaf ell -> withCache (Id pos) $ pure . compileLeaf $ ell
  ANFForce _ body -> withCache (Id pos) $ do
    (mBodyBind, bodyCode) <- compileRef body
    pure $ case mBodyBind of
      Nothing -> uplcForce bodyCode
      Just bodyName -> uplcLet bodyName bodyCode . uplcForce . uplcVar $ bodyName
  ANFDelay _ body -> withCache (Id pos) $ do
    (mBodyBind, bodyCode) <- compileRef body
    pure $ case mBodyBind of
      Nothing -> uplcDelay bodyCode
      Just bodyName -> uplcLet bodyName bodyCode . uplcDelay . uplcVar $ bodyName
  ANFFix _ _ _ body -> withCache (Id pos) $ do
    (mBodyBind, bodyCode) <- compileRef body
    -- For fixed points, given the functional `F`, we want to produce `M (\r
    -- -> F (r r))`. To enable this, we've set aside two suitable names: one for the
    -- argument of `M`, the other for `r`. Since `M` is small, it's cheaper to
    -- inline than bind it. We also don't bother binding `(\r -> F (r r))`: if
    -- `F` is a unique functional, then `(\r -> F (r r))` will be unique by the
    -- same logic.
    (mArgName, functionalArgName) <- getFixNames (Id pos)
    -- `M = \x -> x x`, using the reserved name.
    let m = uplcMCombinator mArgName
    -- `r` using the reserved name.
    let funcArg = uplcVar functionalArgName
    -- `r r`
    let functionalSelfApply = uplcApply1 funcArg funcArg
    pure $ case mBodyBind of
      -- `M (\r -> F (r r))`
      Nothing -> uplcApply1 m . uplcLam1 functionalArgName . uplcApply1 bodyCode $ functionalSelfApply
      -- `let f = F in M (\r -> f (r r))`
      Just bodyName ->
        uplcLet bodyName bodyCode
          . uplcApply1 m
          . uplcLam1 functionalArgName
          . uplcApply1 (uplcVar bodyName)
          $ functionalSelfApply
  ANFConstr _ tag fields -> withCache (Id pos) $ do
    compiledFields <- traverse compileRef fields
    let fieldsAsCode = fmap mkFieldCode compiledFields
    let constrCode = uplcConstr tag fieldsAsCode
    pure . Vector.foldl' mkFieldLets constrCode $ compiledFields
  ANFCase _ scrut handlers -> withCache (Id pos) $ do
    (mScrutBind, scrutCode) <- compileRef scrut
    compiledHandlers <- traverse compileRef handlers
    let handlersAsCode = fmap mkFieldCode compiledHandlers
    -- If we need to let-bind the scrutinee, we do it 'on the inside'.
    let caseCode = case mScrutBind of
          Nothing -> uplcCase scrutCode handlersAsCode
          Just scrutName -> uplcLet scrutName scrutCode . uplcCase (uplcVar scrutName) $ handlersAsCode
    pure . NEVector.foldl' mkFieldLets caseCode $ compiledHandlers
  ANFApply _ f xs -> withCache (Id pos) $ do
    (mFBind, fCode) <- compileRef f
    compiledXs <- traverse compileRef xs
    let xsAsCode = fmap mkFieldCode compiledXs
    let appCode =
          if NEVector.length compiledXs <= 2
            -- Compile a chain of applications
            then case mFBind of
              Nothing -> uplcApply fCode xsAsCode
              Just fName -> uplcLet fName fCode . uplcApply (uplcVar fName) $ xsAsCode
            -- Cheaper to compile a `constr` which is immediately
            -- `case`d on.
            else
              let tempConstr = uplcConstr 0 . NEVector.toVector $ xsAsCode
               in case mFBind of
                    Nothing ->
                      let soleHandler = NEVector.singleton fCode
                       in uplcCase tempConstr soleHandler
                    Just fName ->
                      let soleHandler = NEVector.singleton . uplcVar $ fName
                       in uplcLet fName fCode . uplcCase tempConstr $ soleHandler
    pure . NEVector.foldl' mkFieldLets appCode $ compiledXs
  ANFLam _ params _ body -> withCache (Id pos) $ do
    (bodyBind, bodyCode) <- compileRef body
    asParamNames <- NEVector.mapM multToName params
    pure $ case bodyBind of
      Nothing -> uplcLam asParamNames bodyCode
      Just bodyName -> uplcLet bodyName bodyCode . uplcLam asParamNames . uplcVar $ bodyName
  where
    compileLeaf :: Leaf ann -> UPLCTerm
    compileLeaf = \case
      LConstant _ c -> uplcConstant c
      LBuiltin _ f -> uplcBuiltin f
      LCompiled _ code -> code
      LError _ -> uplcError
    mkFieldCode :: (Maybe PLC.Name, UPLCTerm) -> UPLCTerm
    mkFieldCode (mName, code) = maybe code uplcVar mName
    mkFieldLets :: UPLCTerm -> (Maybe PLC.Name, UPLCTerm) -> UPLCTerm
    mkFieldLets acc (mName, code) = case mName of
      -- We've inlined the field already.
      Nothing -> acc
      -- We need to let-bind this field, as the `constr` uses a variable for it.
      Just name -> uplcLet name code acc
    multToName :: Maybe Multiplicity -> CompileM ann PLC.Name
    multToName = \case
      Nothing -> asks unusedParamName
      Just (MultiplicityOne (Hash h)) -> pure . mkName "arg" $ h
      Just (MultiplicityMany (Hash h)) -> pure . mkName "arg" $ h

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

-- Compiles a subcomputation, while also checking if it should be `let`-bound.
-- If it should be `let`-bound, also produces the name it should be bound under.
compileRef ::
  forall (ann :: Type).
  Ref -> CompileM ann (Maybe PLC.Name, UPLCTerm)
compileRef = \case
  AVar (Hash h) -> pure (Nothing, uplcVar . mkName "arg" $ h)
  AnId (Id subId) -> do
    binds <- asks ceBinds
    compiledSubExpr <- compile subId (binds NEVector.! subId)
    case compiledSubExpr of
      -- This has been let-bound 'above' us, so we use a variable to refer to the
      -- body. This does not need further let-binding.
      Left subExprName -> pure (Nothing, uplcVar subExprName)
      -- This hasn't been let-bound (yet). Depending on how many times this might
      -- be used, it might be worth let-binding.
      Right subExprCode -> do
        used <- getUseCount (Id subId)
        case used of
          -- Since this bind is only used once, we may as well inline it.
          1 -> pure (Nothing, subExprCode)
          -- This is going to be used multiple times. Since we're declaring it
          -- should be let-bound, we also rewrite its compilation cache entry to
          -- its previously-reserved name, and produce a variable by that name.
          _ -> do
            subExprName <- getBindName (Id subId)
            rewriteToName subExprName (Id subId)
            pure (Just subExprName, subExprCode)

getUseCount :: forall (ann :: Type). Id -> CompileM ann Word64
getUseCount (Id i) = asks ((NEVector.! i) . ceUseByBind)

getBindName :: forall (ann :: Type). Id -> CompileM ann PLC.Name
getBindName (Id i) = asks ((NEVector.! i) . ceBindNames)

-- Used when we have identified that a compiled bind needed as a dependency is
-- going to be needed elsewhere as well. This rewrites its cached entry to be a
-- name instead of code.
rewriteToName :: forall (ann :: Type). PLC.Name -> Id -> CompileM ann ()
rewriteToName name i =
  modify (\(CompileState cs) -> CompileState . Map.insert i (Left name) $ cs)
