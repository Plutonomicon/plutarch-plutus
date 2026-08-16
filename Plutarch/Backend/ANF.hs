{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoPartialTypeSignatures #-}

{- | Administrative normal form representation, with alpha-equivalent binds
coalesced.

= Links

- [Description of administrative normal
form](https://en.wikipedia.org/wiki/A-normal_form)

@since wip
-}
module Plutarch.Backend.ANF (
  BoundVar (..),
  Multiplicity (MultOne),
  Leaf (..),
  Ref (..),
  ANFBind (..),
  Id (..),
  ANF (..),
  Demand (..),
  fromHashedAST,
  getANFBindAnn,
  fullPipeline,
) where

import Control.Monad (when)
import Control.Monad.State.Strict (
  MonadState,
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
import Data.Monoid (Sum (Sum))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (MVector, Vector)
import Data.Vector qualified as Vector
import Data.Vector.Mutable (PrimMonad (PrimState))
import Data.Vector.Mutable qualified as MVector
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEVector
import Data.Word (Word64)
import Plutarch.Backend.AST (
  AST (
    ASTApply,
    ASTCase,
    ASTCompose,
    ASTConstr,
    ASTDelay,
    ASTFix,
    ASTForce,
    ASTLam,
    ASTLeaf
  ),
  Hash,
 )
import Plutarch.Backend.AST qualified as AST
import Plutarch.Backend.UPLC (UPLCTerm)
import Plutarch.Helpers.Pretty (prettyValueOf, (<:=>))
import PlutusCore (Some (Some), ValueOf (ValueOf))
import PlutusCore qualified as PLC
import Prettyprinter (
  Doc,
  Pretty (pretty),
  align,
  braces,
  brackets,
  group,
  hardline,
  hsep,
  indent,
  list,
  punctuate,
  vcat,
  viaShow,
  (<+>),
 )

{- | A hash identifying a bound variable argument, together with its
multiplicity (how many times it occurs in the body where it is bound).

@since wip
-}
data BoundVar = BoundVar Hash Multiplicity
  deriving stock
    ( -- | @since wip
      Eq
    , -- | @since wip
      Show
    )

-- | @since wip
instance Pretty BoundVar where
  pretty (BoundVar h m) = pretty h <> ":" <> pretty m

{- | A positive-only number indicating the number of times a bound variable is
used in the body of whatever binds it.

@since wip
-}
newtype Multiplicity = Multiplicity Word
  deriving stock
    ( -- | @since wip
      Show
    )
  deriving
    ( -- | @since wip
      Eq
    , -- | @since wip
      Pretty
    )
    via Word
  deriving
    ( -- | @since wip
      Semigroup
    )
    via (Sum Word)

-- | @since wip
pattern MultOne :: Multiplicity
pattern MultOne = Multiplicity 1

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
    , -- | @since wip
      Eq
    )

-- | @since wip
instance Pretty (Leaf ann) where
  pretty = \case
    LConstant _ (Some (ValueOf uni x)) -> prettyValueOf uni x
    LBuiltin _ fun -> viaShow fun
    LCompiled _ uplc -> "COMPILED" <+> align (braces (align . group $ pretty uplc))
    LError _ -> "ERROR"

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
    , -- | @since wip
      Eq
    )

-- | @since wip
instance Pretty Ref where
  pretty = \case
    AVar h -> pretty h
    AnId i -> pretty i

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

-- | @since wip
instance Pretty Id where
  pretty (Id i) = "#" <> viaShow i

{- | An ANF bind. Instead of a recursive ANF-like structure, this uses 'Id' to
determine subcomputations.

@since wip
-}
data ANFBind (ann :: Type)
  = ANFLeaf (Leaf ann)
  | ANFForce ann Ref
  | ANFDelay ann Ref
  | ANFLam ann (NonEmptyVector (Maybe BoundVar)) Ref
  | ANFFix ann BoundVar Ref
  | ANFApply ann Ref (NonEmptyVector Ref)
  | ANFConstr ann Word64 (Vector Ref)
  | ANFCase ann Ref (NonEmptyVector Ref)
  | ANFCompose ann (NonEmptyVector Ref)
  deriving stock
    ( -- | @since wip
      Show
    , -- | @since wip
      Functor
    , -- | @since wip
      Eq
    )

-- | @since wip
instance Pretty (ANFBind ann) where
  pretty = \case
    ANFLeaf l -> pretty l
    ANFForce _ ref -> "Force" <+> pretty ref
    ANFDelay _ ref -> "Delay" <+> pretty ref
    ANFLam _ args body -> "\\" <> mkArgs args <+> "->" <+> pretty body
    ANFFix _ mult body -> "Fix" <> brackets (pretty mult) <+> pretty body
    ANFApply _ fnRef args -> "Apply" <+> pretty fnRef <+> list (pretty <$> NEVector.toList args)
    ANFConstr _ cix args -> "Constr" <+> viaShow cix <+> list (pretty <$> Vector.toList args)
    ANFCase _ scrut handlers -> "Case" <+> pretty scrut <+> list (pretty <$> NEVector.toList handlers)
    ANFCompose _ args -> hsep . punctuate " <<<" . fmap pretty . NEVector.toList $ args
    where
      mkArgs :: forall ann. NEVector.NonEmptyVector (Maybe BoundVar) -> Doc ann
      mkArgs (NEVector.toList -> xs) =
        hsep
          . fmap (\case Nothing -> "_"; Just m -> pretty m)
          $ xs

-- | @since wip
getANFBindAnn :: forall (ann :: Type). ANFBind ann -> ann
getANFBindAnn = \case
  ANFLeaf ell -> case ell of
    LConstant x _ -> x
    LBuiltin x _ -> x
    LCompiled x _ -> x
    LError x -> x
  ANFForce x _ -> x
  ANFDelay x _ -> x
  ANFLam x _ _ -> x
  ANFFix x _ _ -> x
  ANFApply x _ _ -> x
  ANFConstr x _ _ -> x
  ANFCase x _ _ -> x
  ANFCompose x _ -> x

{- | A combination of a (nonempty) vector of binds, together with a unique
mapping between identifiers and hashes of unique subcomputations.

@since wip
-}
data ANF (ann :: Type) = ANF (Bimap Id Hash) (NonEmptyVector (ANFBind ann))

-- | @since wip
instance Pretty (ANF ()) where
  pretty anf =
    "ANF Hashes:"
      <> hardline
      <> indent 1 (align $ prettyANFHashes anf)
      <> hardline
      <> hardline
      <> "ANF Binds:"
      <> hardline
      <> indent 1 (align $ prettyANFBinds anf)
      <> hardline

-- | @since wip
instance {-# OVERLAPS #-} Pretty ann => Pretty (ANF ann) where
  pretty anf =
    "ANF Hashes:"
      <> hardline
      <> indent 1 (align $ prettyANFHashes anf)
      <> hardline
      <> hardline
      <> "ANF Binds:"
      <> hardline
      <> indent 1 (align $ prettyANFBinds anf)
      <> hardline
      <> hardline
      <> "ANF Annotations:"
      <> hardline
      <> indent 1 (align $ prettyANFAnnotations anf)
      <> hardline

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
      ASTLam h bvs body -> withLookup h $ do
        bodyRef <- go body
        boundVars <- traverse (traverse (bvWithMultiplicity bodyRef)) bvs
        newBind h (ANFLam () boundVars bodyRef)
      ASTFix h bv body -> withLookup h $ do
        bodyRef <- go body
        boundVar <- bvWithMultiplicity bodyRef bv
        newBind h (ANFFix () boundVar bodyRef)
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
      ASTCompose h components -> withLookup h $ do
        componentsRefs <- traverse go components
        newBind h (ANFCompose () componentsRefs)
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

{- | A type to indicate how often a (sub) computation is needed as part of an
entire translation unit, as well as where it should be @let@ bound if needed.

@since wip
-}
data Demand
  = {- | This computation is not needed anywhere. This typically indicates the
    top-level computation in any translation unit.

    @since wip
    -}
    NeverDemanded
  | {- | This computation is needed at least once (exact use count indicated
    by 'Word64'), and if it needs to be @let@-bound, where this can be
    done (indicated by 'Id').

    @since wip
    -}
    Demanded Id Word64
  | {- | Should never be @let@-bound. Thus, its use count is not needed.

    @since wip
    -}
    Trivial
  deriving stock
    ( -- | @since wip
      Eq
    , -- | @since wip
      Show
    )

-- | @since wip
instance Pretty Demand where
  pretty = viaShow

{- | Performs all analysis steps needed to get the full demand analysis, then
annotates the ANF with that analysis.

@since wip
-}
fullPipeline :: forall (ann :: Type). ANF ann -> ANF Demand
fullPipeline anf@(ANF _ binds) =
  let labelled = labelTransitiveDeps binds
      counted = countUses binds
      reqVars = collectRequiredVars binds
      bindSites = determineBindSites anf
   in analyzeDemand labelled counted reqVars bindSites anf

-- Helpers

-- At this stage, we have all of the following for each bind:
--

-- * Its transitive dependencies (as `Id`s)

-- * A direct use count (or `Nothing` if trivial)

-- * All variables required by its subcomputation

--
-- Additionally, we also have, for every variable, the site where it is bound
-- (as an `Id`). We can put all this together to construct `Demand`s for each
-- bind.
analyzeDemand ::
  forall (ann :: Type).
  NonEmptyVector (Set Id) ->
  NonEmptyVector (Maybe Word) ->
  NonEmptyVector (Set Hash) ->
  Map Hash Id ->
  ANF ann ->
  ANF Demand
analyzeDemand labelled counted reqVars bindSites (ANF bm binds) =
  let len = NEVector.length binds
      newBinds = NEVector.generate1 len $ \i ->
        let oldBind = binds NEVector.! i
            uses = counted NEVector.! i
         in case uses of
              -- Trivial binds are always inlined, so no need to do any
              -- analysis.
              Nothing -> Trivial <$ oldBind
              -- Only the toplevel node will ever have a zero use count.
              Just 0 -> NeverDemanded <$ oldBind
              -- If we're only used once, we're getting inlined. Thus, what bind
              -- site we give is irrelevant, so we might as well just give
              -- itself.
              Just 1 -> Demanded (Id i) 1 <$ oldBind
              Just n ->
                let neededVars = reqVars NEVector.! i
                    -- Once we know all the required variables, we can determine
                    -- a 'bind site of last resort'. This is because we _must_
                    -- have all required variables in scope, or the bind is
                    -- meaningless.
                    --
                    -- We can establish this by finding the `Id` corresponding
                    -- to the lambda that binds a variable we need with the
                    -- _lowest_ possible identifier. As lower `Id`s are
                    -- dependencies of higher `Id`s, this guarantees we'll get
                    -- the closest enclosing lambda.
                    lastResort = Set.foldl' siteMin (len - 1) neededVars
                 in -- The site of last resort isn't always optimal, especially
                    -- for closed subcomputations. As UPLC is strict, and
                    -- `let`-bindings are compiled into applications of lambdas,
                    -- `let`-binding 'too high' can cause more evaluation than we
                    -- actually need. Consider the following example:
                    --
                    -- \a -> if a then doB else doC
                    --
                    -- Suppose that `doB` (and only `doB`) depends on a closed
                    -- subcomputation `c1` multiple times. Likewise, suppose that
                    -- `doC` (and only `doC`) depends on a closed subcomputation
                    -- `c2` multiple times. If we use the site of last resort, we
                    -- end up compiling something akin to:
                    --
                    -- let c1 = ... in let c2 = ... in \a -> if a then doB' else doC'
                    --
                    -- where `doB'` and `doC'` are `doB` and `doC` with the
                    -- relevant subcomputations replaced by `let`-bound variables.
                    -- However, this would force evaluation of both `c1` and `c2`,
                    -- even though we only ever need one of them!
                    --
                    -- A more sensible strategy would compile to something like so
                    -- instead:
                    --
                    -- \a -> if a then (let c1 = .. in doB') (let c2 = .. in doC')
                    --
                    -- This way, we end up evaluating only what we need and
                    -- nothing more.
                    --
                    -- We thus do a 'digging' analysis to try and 'push'
                    -- `let`-bindings as close to their use sites as we can.
                    Demanded (digDependency (Id i) lastResort) (fromIntegral n) <$ oldBind
   in ANF bm newBinds
  where
    siteMin :: Int -> Hash -> Int
    siteMin acc varHash = case Map.lookup varHash bindSites of
      -- Technically impossible
      Nothing -> acc
      Just (Id site) -> min acc site
    digDependency :: Id -> Int -> Id
    digDependency target currId =
      let currBind = binds NEVector.! currId
       in case currBind of
            -- We should absolutely never make it down here. Give it a default
            -- for the completeness checker.
            ANFLeaf _ -> Id currId
            -- As these binds have only a single descendant, they are never the
            -- optimal place for a `let`-bind, as we can also bind in their
            -- bodies. Thus, we 'step into' their bodies and keep searching.
            ANFForce _ r -> refToBindSite target (Id currId) r
            ANFDelay _ r -> refToBindSite target (Id currId) r
            ANFLam _ _ body -> refToBindSite target (Id currId) body
            ANFFix _ _ body -> refToBindSite target (Id currId) body
            -- Each of the following have multiple descendants. By our earlier
            -- analyses, we know that at least one of them depends, either
            -- directly or not, on `target`. Thus, there are two possibilities:
            --
            -- 1. Exactly one descendant bind depends (transitively) on
            --    `target`.
            -- 2. More than one descendant bind depends (transitively) on
            --    `target.
            --
            -- The _first time_ we encounter Case 2 represents the point at
            -- which the `let`-bind of `target` _must_ happen. If we see a Case
            -- 1, the descendant (or possibly one of its descendants) is instead
            -- a better choice.
            ANFApply _ f xs ->
              let start = refToJPR target f
               in case NEVector.foldl' (combineJPRs target) start xs of
                    -- This cannot happen: at least one of the descendants must
                    -- depend on `target` somehow.
                    NoCandidateFound -> Id currId
                    Ineligible -> Id currId
                    Descend next -> digDependency target next
            ANFConstr _ _ fields -> case Vector.uncons fields of
              -- This cannot happen: at least one of the descendants must depend
              -- on `target` somehow.
              Nothing -> Id currId
              Just (field, fields') -> case Vector.foldl' (combineJPRs target) (refToJPR target field) fields' of
                -- This cannot happen: at least one of the descendants must
                -- depend on `target` somehow.
                NoCandidateFound -> Id currId
                Ineligible -> Id currId
                Descend next -> digDependency target next
            ANFCase _ scrut handlers ->
              let start = refToJPR target scrut
               in case NEVector.foldl' (combineJPRs target) start handlers of
                    -- This cannot happen: at least one of the descendants must
                    -- depend on `target` somehow.
                    NoCandidateFound -> Id currId
                    Ineligible -> Id currId
                    Descend next -> digDependency target next
            ANFCompose _ components -> case NEVector.uncons components of
              (c, cs) -> case Vector.foldl' (combineJPRs target) (refToJPR target c) cs of
                -- This cannot happen: at least one of the descendants must
                -- depend on `target` somehow.
                NoCandidateFound -> Id currId
                Ineligible -> Id currId
                Descend next -> digDependency target next
    refToBindSite :: Id -> Id -> Ref -> Id
    refToBindSite target curr = \case
      AVar _ -> curr
      AnId (Id i) -> digDependency target i
    refToJPR :: Id -> Ref -> JoinPointResult
    refToJPR target = \case
      AVar _ -> NoCandidateFound
      AnId i@(Id asInt) ->
        if
          | -- Direct dependency, which means we _must_ `let`-bind here.
            i == target ->
              Ineligible
          | -- Indirect dependency, so we may potentially be able to bind closer to the use site.
            Set.member target (labelled NEVector.! asInt) ->
              Descend asInt
          | -- No dependency in this particular descendant.
            otherwise ->
              NoCandidateFound
    combineJPRs :: Id -> JoinPointResult -> Ref -> JoinPointResult
    combineJPRs target acc r = case acc of
      -- Once we have established that the `let`-bind must happen here, no
      -- further evidence matters.
      Ineligible -> Ineligible
      -- We haven't (yet) found which descendant(s) require `target` as a
      -- dependency, so whatever answer we get for this one becomes the result
      -- for now.
      NoCandidateFound -> refToJPR target r
      -- Once we've identified a descendant as requiring `target`, any other
      -- descendants that also require `target` essentially force the `let`-bind
      -- to happen right here. Furthermore, if any of the descendants _are_
      -- `target`, this also happens. The only way this _doesn't_ happen is if
      -- the descendant doesn't depend on `target` at all.
      Descend _ -> case refToJPR target r of
        NoCandidateFound -> acc
        _ -> Ineligible

-- A 'join point' is a place where two (or more) subcomputations connect. This
-- data type represents a (possibly partial) analysis of a join point as a
-- candidate for placing a `let`-bind.
data JoinPointResult
  = -- We haven't found any descendants (yet) which depend on the computation
    -- we're trying to `let`-bind.
    NoCandidateFound
  | -- Either this join point depends on the computation we need to `let`-bind
    -- directly, or multiple descendants depend on the computation we need to
    -- `let`-bind, directly or transitively.
    Ineligible
  | -- The descendant whose `Id` is the given `Int` is an improved candidate
    -- for placing the `let`-bind.
    Descend Int

determineBindSites :: forall (ann :: Type). ANF ann -> Map Hash Id
determineBindSites (ANF _ binds) = NEVector.ifoldl' go Map.empty binds
  where
    go :: Map Hash Id -> Int -> ANFBind ann -> Map Hash Id
    go acc i = \case
      ANFLam _ bvs _ ->
        NEVector.foldl' (\acc' bv -> maybe acc' (\(BoundVar h _) -> Map.insert h (Id i) acc') bv) acc bvs
      ANFFix _ (BoundVar h _) _ -> Map.insert h (Id i) acc
      _ -> acc

labelTransitiveDeps ::
  forall (ann :: Type).
  NonEmptyVector (ANFBind ann) ->
  NonEmptyVector (Set Id)
labelTransitiveDeps binds = NEVector.unsafeCreate $ do
  let len = NEVector.length binds
  tdMV <- MVector.replicate len Set.empty
  for_ [0, 1 .. len - 1] $ \i -> do
    let bind = binds NEVector.! i
    case bind of
      ANFLeaf _ -> pure ()
      ANFForce _ r -> updateDeps tdMV i r
      ANFDelay _ r -> updateDeps tdMV i r
      ANFLam _ _ body -> updateDeps tdMV i body
      ANFFix _ _ body -> updateDeps tdMV i body
      ANFApply _ f xs -> do
        updateDeps tdMV i f
        traverse_ (updateDeps tdMV i) xs
      ANFConstr _ _ fields -> traverse_ (updateDeps tdMV i) fields
      ANFCase _ scrut handlers -> do
        updateDeps tdMV i scrut
        traverse_ (updateDeps tdMV i) handlers
      ANFCompose _ components -> traverse_ (updateDeps tdMV i) components
  pure tdMV
  where
    updateDeps ::
      forall (m :: Type -> Type).
      PrimMonad m =>
      MVector (PrimState m) (Set Id) ->
      Int ->
      Ref ->
      m ()
    updateDeps mv i = \case
      AnId (Id j) -> do
        deps <- MVector.read mv j
        MVector.modify mv (Set.union (Set.insert (Id j) deps)) i
      AVar _ -> pure ()

-- `Nothing` means trivial
countUses :: forall (ann :: Type). NonEmptyVector (ANFBind ann) -> NonEmptyVector (Maybe Word)
countUses binds = NEVector.unsafeCreate $ do
  let len = NEVector.length binds
  countMV <- MVector.replicate len (Just 0)
  for_ [0, 1 .. len - 1] $ \i -> do
    let bind = binds NEVector.! i
    case bind of
      ANFLeaf ell -> case ell of
        LConstant _ c -> when (smallEnoughToInline c) (MVector.write countMV i Nothing)
        LBuiltin _ _ -> MVector.write countMV i Nothing
        LError _ -> MVector.write countMV i Nothing
        _ -> pure ()
      ANFForce _ r -> updateCountAt countMV r
      ANFDelay _ r -> updateCountAt countMV r
      ANFLam _ _ body -> updateCountAt countMV body
      ANFFix _ _ body -> updateCountAt countMV body
      ANFApply _ f xs -> do
        updateCountAt countMV f
        traverse_ (updateCountAt countMV) xs
      ANFConstr _ _ fields -> traverse_ (updateCountAt countMV) fields
      ANFCase _ scrut handlers -> do
        updateCountAt countMV scrut
        traverse_ (updateCountAt countMV) handlers
      ANFCompose _ components -> traverse_ (updateCountAt countMV) components
  pure countMV
  where
    smallEnoughToInline :: Some (ValueOf PLC.DefaultUni) -> Bool
    smallEnoughToInline = \case
      Some (ValueOf PLC.DefaultUniBool _) -> True
      Some (ValueOf PLC.DefaultUniUnit _) -> True
      Some (ValueOf PLC.DefaultUniInteger n) -> abs n < 256
      _ -> False
    updateCountAt ::
      forall (m :: Type -> Type).
      PrimMonad m =>
      MVector (PrimState m) (Maybe Word) -> Ref -> m ()
    updateCountAt mv = \case
      AnId (Id i) -> MVector.modify mv (fmap (+ 1)) i
      AVar _ -> pure ()

collectRequiredVars :: forall (ann :: Type). NonEmptyVector (ANFBind ann) -> NonEmptyVector (Set Hash)
collectRequiredVars binds = NEVector.unsafeCreate $ do
  let len = NEVector.length binds
  varMV <- MVector.replicate len Set.empty
  for_ [0, 1 .. len - 1] $ \i -> do
    let bind = binds NEVector.! i
    case bind of
      ANFLeaf _ -> pure ()
      ANFForce _ r -> updateWithDeps varMV i r
      ANFDelay _ r -> updateWithDeps varMV i r
      -- As lambdas are binding sites, they do not depend on the variables they
      -- themselves bind.
      ANFLam _ bvs body -> do
        let boundHashes = NEVector.foldl' (\acc -> \case Nothing -> acc; Just (BoundVar h _) -> Set.insert h acc) Set.empty bvs
        deps <- getDeps varMV body
        let actualDeps = Set.difference deps boundHashes
        MVector.write varMV i actualDeps
      -- As a fixpoint binds its `self` argument, it does not itself depend on
      -- it.
      ANFFix _ (BoundVar h _) body -> do
        let boundHashes = Set.singleton h
        deps <- getDeps varMV body
        let actualDeps = Set.difference deps boundHashes
        MVector.write varMV i actualDeps
      ANFApply _ f xs -> do
        updateWithDeps varMV i f
        traverse_ (updateWithDeps varMV i) xs
      ANFConstr _ _ fields -> traverse_ (updateWithDeps varMV i) fields
      ANFCase _ scrut handlers -> do
        updateWithDeps varMV i scrut
        traverse_ (updateWithDeps varMV i) handlers
      ANFCompose _ components -> traverse_ (updateWithDeps varMV i) components
  pure varMV
  where
    updateWithDeps ::
      forall (m :: Type -> Type).
      PrimMonad m =>
      MVector (PrimState m) (Set Hash) ->
      Int ->
      Ref ->
      m ()
    updateWithDeps mv i r = do
      deps <- getDeps mv r
      MVector.modify mv (Set.union deps) i
    getDeps ::
      forall (m :: Type -> Type).
      PrimMonad m =>
      MVector (PrimState m) (Set Hash) ->
      Ref ->
      m (Set Hash)
    getDeps mv = \case
      AnId (Id i) -> MVector.read mv i
      AVar varHash -> pure . Set.singleton $ varHash

bvWithMultiplicity ::
  forall (a :: Type) (m :: Type -> Type).
  MonadState (a, IntMap (ANFBind ())) m =>
  Ref -> Hash -> m BoundVar
bvWithMultiplicity r h = case r of
  AVar h' ->
    if h' == h
      then pure . BoundVar h . Multiplicity $ 1
      else error "Argument claimed as used, but body shows it as unused. If you see this, report a bug."
  AnId _ -> do
    existingBinds <- gets snd
    let (Sum mult) = foldMap countOccurrence existingBinds
    pure . BoundVar h . Multiplicity $ mult
    where
      countOccurrence :: ANFBind () -> Sum Word
      countOccurrence = \case
        ANFLeaf _ -> mempty
        ANFForce _ body -> hashToCount h body
        ANFDelay _ body -> hashToCount h body
        ANFLam _ _ body -> hashToCount h body
        ANFFix _ _ body -> hashToCount h body
        ANFApply _ f xs -> hashToCount h f <> foldMap (hashToCount h) xs
        ANFConstr _ _ fields -> foldMap (hashToCount h) fields
        ANFCase _ scrut handlers -> hashToCount h scrut <> foldMap (hashToCount h) handlers
        ANFCompose _ components -> foldMap (hashToCount h) components

hashToCount :: Hash -> Ref -> Sum Word
hashToCount h = \case
  AVar h' -> if h == h' then Sum 1 else mempty
  AnId _ -> mempty

prettyANFBinds :: forall ann1 ann2. ANF ann1 -> Doc ann2
prettyANFBinds (ANF _ binds) = vcat . NEVector.toList $ NEVector.imap (\(Id -> i) b -> mkBind i b) binds
  where
    mkBind :: Id -> ANFBind ann1 -> Doc ann2
    mkBind i b = align . group $ pretty i <:=> align (group $ pretty b)

prettyANFAnnotations :: forall ann1 ann2. Pretty ann1 => ANF ann1 -> Doc ann2
prettyANFAnnotations (ANF _ binds) = vcat . NEVector.toList $ NEVector.imap (\(Id -> i) b -> mkAnn i b) binds
  where
    mkAnn :: Id -> ANFBind ann1 -> Doc ann2
    mkAnn i b = align . group $ pretty i <:=> align (group . pretty $ getANFBindAnn b)

prettyANFHashes :: forall ann1 ann2. ANF ann1 -> Doc ann2
prettyANFHashes (ANF hashes _) = vcat . map (\(i, h) -> pretty i <:=> pretty h) . Bimap.toAscList $ hashes
