{-# LANGUAGE NoOverloadedLists #-}
{-# LANGUAGE NoPartialTypeSignatures #-}

{- | Administrative normal form representation, with alpha-equivalent binds
coalesced.

= Links

- [Description of administrative normal
form](https://en.wikipedia.org/wiki/A-normal_form)

@since wip
-}
module Plutarch.Backend.ANF (
  Leaf (..),
  Ref (..),
  ANFBind (..),
  Id (..),
  ANF (..),
  fromHashedAST,
  toUPLCTerm,
) where

import Control.Monad (foldM)
import Control.Monad.RWS.CPS (RWS, evalRWS, runRWS)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.ST (ST, runST)
import Control.Monad.State.Strict (
  MonadState (get),
  State,
  gets,
  modify,
  runState,
 )
import Data.Bifunctor (bimap)
import Data.Bimap (Bimap)
import Data.Bimap qualified as Bimap
import Data.Foldable (for_, traverse_)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Vector (MVector, Vector)
import Data.Vector qualified as Vector
import Data.Vector.Mutable qualified as MVector
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEVector
import Data.Word (Word64)
import Plutarch.Backend.AST (
  AST (
    ASTApply,
    ASTCase,
    ASTConstr,
    ASTDelay,
    ASTFix,
    ASTForce,
    ASTLam,
    ASTLeaf
  ),
  Hash (Hash),
  Liftability,
  Multiplicity (MultiplicityMany, MultiplicityOne),
 )
import Plutarch.Backend.AST qualified as AST
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
import PlutusCore (Some, ValueOf)
import PlutusCore qualified as PLC

{- | A leaf bind in the ANF (that is, one that cannot have dependencies).

@since wip
-}
data Leaf (ann :: Type)
  = LConstant ann (Some (ValueOf PLC.DefaultUni))
  | LBuiltin ann PLC.DefaultFun
  | LCompiled ann UPLCTerm
  | LError ann
  deriving stock
    ( -- | @since wip
      Functor
    , -- | @since wip
      Show
    )

{- | As ANF \'inlines\' variables, subcomputations are either variables
(identified by their hashes) or other binds (identified by their
identifiers).

@since wip
-}
data Ref
  = AVar Hash
  | AnId Id
  deriving stock
    ( -- | @since wip
      Show
    )

{- | An identifier for an ANF bind.

@since wip
-}
newtype Id = Id Int
  deriving
    ( -- | @since wip
      Eq
    , -- | @since wip
      Ord
    )
    via Int
  deriving stock
    ( -- | @since wip
      Show
    )

{- | An ANF bind. Instead of a recursive ANF-like structure, this uses 'Id' to
determine subcomputations.

@since wip
-}
data ANFBind (ann :: Type)
  = ANFLeaf (Leaf ann)
  | ANFForce ann Ref
  | ANFDelay ann Ref
  | ANFLam ann (NonEmptyVector (Maybe Multiplicity)) Liftability Ref
  | ANFFix ann Multiplicity Liftability Ref
  | ANFApply ann Ref (NonEmptyVector Ref)
  | ANFConstr ann Word64 (Vector Ref)
  | ANFCase ann Ref (NonEmptyVector Ref)
  deriving stock
    ( -- | @since wip
      Show
    )

{- | A combination of a (nonempty) vector of binds, together with a unique
mapping between identifiers and hashes of unique subcomputations.

@since wip
-}
data ANF (ann :: Type) = ANF (Bimap Id Hash) (NonEmptyVector (ANFBind ann))

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

{- | Given an 'AST' annotated with hashes for unique (up to alpha-equivalence)
subcomputations, construct an ANF. The 'Hash' annotations are used to
identify alpha-equivalent subcomputations, so that they correspond to a
single unique bind.

@since wip
-}
fromHashedAST :: AST Hash -> ANF ()
fromHashedAST ast = case runState (go ast) (Bimap.empty, IntMap.empty) of
  -- Note (Koz, 05/06/2026): Due to how we construct the IntMap here, there is
  -- no chance of 'missing' or non-contiguous keys. Thus, the `lookup` cannot
  -- fail.
  (_, (bm, im)) -> ANF bm . NEVector.generate1 (IntMap.size im) $ \i -> fromJust . IntMap.lookup i $ im
  where
    -- We maintain the partially-constructed bind list as an `IntMap` mostly for
    -- efficiency, as incrementally expanding a `Vector` is quite tedious and
    -- inefficient.
    go :: AST Hash -> State (Bimap Id Hash, IntMap (ANFBind ())) Ref
    go = \case
      ASTLeaf ell -> doLeaf ell
      ASTForce h body -> withLookup h $ do
        bodyRef <- go body
        newBind h (ANFForce () bodyRef)
      ASTDelay h body -> withLookup h $ do
        bodyRef <- go body
        newBind h (ANFDelay () bodyRef)
      ASTLam h mults liftability body -> withLookup h $ do
        bodyRef <- go body
        newBind h (ANFLam () mults liftability bodyRef)
      ASTFix h mult liftability body -> withLookup h $ do
        bodyRef <- go body
        newBind h (ANFFix () mult liftability bodyRef)
      ASTApply h f xs -> withLookup h $ do
        fRef <- go f
        xsRefs <- traverse go xs
        newBind h (ANFApply () fRef xsRefs)
      ASTConstr h tag fields -> withLookup h $ do
        fieldsRefs <- traverse go fields
        newBind h (ANFConstr () tag fieldsRefs)
      ASTCase h scrut handlers -> withLookup h $ do
        scrutRef <- go scrut
        handlersRefs <- traverse go handlers
        newBind h (ANFCase () scrutRef handlersRefs)
    doLeaf :: AST.Leaf Hash -> State (Bimap Id Hash, IntMap (ANFBind ())) Ref
    doLeaf = \case
      AST.LVar _ h -> pure . AVar $ h
      AST.LConstant h c -> withLookup h $ newBind h (ANFLeaf (LConstant () c))
      AST.LBuiltin h f -> withLookup h $ newBind h (ANFLeaf (LBuiltin () f))
      AST.LCompiled h code -> withLookup h $ newBind h (ANFLeaf (LCompiled () code))
      AST.LError h -> withLookup h $ newBind h (ANFLeaf (LError ()))
    -- Checks if we've already seen an alpha-equivalent bind. If so, yield a
    -- `Ref` to it, otherwise take the supplied action to build it, and produce
    -- the `Ref` from that.
    withLookup ::
      Hash ->
      State (Bimap Id Hash, IntMap (ANFBind ())) Ref ->
      State (Bimap Id Hash, IntMap (ANFBind ())) Ref
    withLookup h act = do
      mId <- gets (Bimap.lookupR h . fst)
      maybe act (pure . AnId) mId
    newBind :: Hash -> ANFBind () -> State (Bimap Id Hash, IntMap (ANFBind ())) Ref
    newBind h bind = do
      -- Ensures we have contiguous keys starting from 0.
      firstAvailable <- gets (maybe 0 ((+ 1) . fst) . IntMap.lookupMax . snd)
      let asId = Id firstAvailable
      modify (bimap (Bimap.insert asId h) (IntMap.insert firstAvailable bind))
      pure . AnId $ asId

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

getFresh :: RWS (Set Int) () Int Int
getFresh = do
  fresh <- get
  modify (+ 1)
  pure fresh

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
