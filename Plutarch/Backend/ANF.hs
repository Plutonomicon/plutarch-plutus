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
  analyzeDemand,
  getANFBindAnn,
) where

import Control.Monad.ST (ST, runST)
import Control.Monad.State.Strict (
  MonadState,
  State,
  execStateT,
  gets,
  modify,
  runState,
 )
import Data.Bifunctor (bimap)
import Data.Bimap (Bimap)
import Data.Bimap qualified as Bimap
import Data.Foldable (fold, for_, traverse_)
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
import Plutarch.Utils.Pretty (prettyValueOf, (<:=>))
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

analyzeDemand :: forall (ann :: Type). ANF ann -> ANF Demand
analyzeDemand (ANF bm binds) = runST $ do
  let len = NEVector.length binds
  countMV <- MVector.replicate len (0 :: Int)
  neededVarsMV <- MVector.replicate len Set.empty
  -- Collect 'raw' statistics on use count, needed variables and binding sites
  bindingSites <- collectRawStatistics len countMV neededVarsMV
  -- At this stage, we know:
  --
  -- 1. Every (useful) use count of every bind
  -- 2. What variables must be in scope for any given bind to be meaningful
  -- 3. For every variable, where that variable was bound
  --
  -- Knowing all these, we can now construct a brand-new ANF with full demand
  -- analysis.
  newBinds <- NEVector.generate1M len $ \i -> do
    let oldBind = binds NEVector.! i
    uses <- MVector.read countMV i
    case uses of
      (-1) -> pure (Trivial <$ oldBind)
      0 -> pure (NeverDemanded <$ oldBind)
      _ -> do
        neededVars <- MVector.read neededVarsMV i
        let lowestSite = Set.foldl' (siteMin bindingSites) (len - 1) neededVars
        pure (Demanded (Id lowestSite) (fromIntegral uses) <$ oldBind)
  pure . ANF bm $ newBinds
  where
    siteMin :: Map Hash Int -> Int -> Hash -> Int
    siteMin bindingSites acc varHash = case Map.lookup varHash bindingSites of
      -- Technically impossible
      Nothing -> acc
      Just site -> min acc site
    smallEnoughToInline :: Some (ValueOf PLC.DefaultUni) -> Bool
    smallEnoughToInline = \case
      Some (ValueOf PLC.DefaultUniBool _) -> True
      Some (ValueOf PLC.DefaultUniUnit _) -> True
      Some (ValueOf PLC.DefaultUniInteger n) -> abs n < 256
      _ -> False
    updateCountAt ::
      forall (m :: Type -> Type).
      PrimMonad m =>
      MVector (PrimState m) Int -> Ref -> m ()
    updateCountAt mv = \case
      AnId (Id i) -> MVector.modify mv (\case (-1) -> (-1); old -> old + 1) i
      AVar _ -> pure ()
    getNeededFrom ::
      forall (m :: Type -> Type).
      PrimMonad m =>
      MVector (PrimState m) (Set Hash) -> Ref -> m (Set Hash)
    getNeededFrom mv = \case
      AnId (Id i) -> MVector.read mv i
      AVar v -> pure . Set.singleton $ v
    bvsToSet :: NonEmptyVector (Maybe BoundVar) -> Set Hash
    bvsToSet = NEVector.foldl' go Set.empty
    go :: Set Hash -> Maybe BoundVar -> Set Hash
    go acc = \case
      Nothing -> acc
      Just (BoundVar h _) -> Set.insert h acc
    collectRawStatistics ::
      forall (s :: Type).
      Int ->
      MVector s Int ->
      MVector s (Set Hash) ->
      ST s (Map Hash Int)
    collectRawStatistics len countMV neededVarsMV = flip execStateT Map.empty $
      for_ [0, 1 .. len - 1] $ \i -> case binds NEVector.! i of
        ANFLeaf ell -> MVector.write countMV i $ case ell of
          LConstant _ c ->
            if smallEnoughToInline c
              then (-1)
              else 0
          LBuiltin _ _ -> (-1)
          LError _ -> (-1)
          _ -> 0
        ANFForce _ r -> do
          updateCountAt countMV r
          neededVars <- getNeededFrom neededVarsMV r
          MVector.write countMV i 0
          MVector.write neededVarsMV i neededVars
        ANFDelay _ r -> do
          updateCountAt countMV r
          neededVars <- getNeededFrom neededVarsMV r
          MVector.write countMV i 0
          MVector.write neededVarsMV i neededVars
        ANFLam _ boundVars r -> do
          updateCountAt countMV r
          neededVars <- getNeededFrom neededVarsMV r
          -- Lambdas act as binding sites, so they only need variables bound
          -- 'above' them. Thus, we remove any used bound vars, but also note
          -- that this is where said variables get bound.
          let bvAsSet = bvsToSet boundVars
          let neededVars' = Set.difference neededVars bvAsSet
          MVector.write countMV i 0
          MVector.write neededVarsMV i neededVars'
          modify (\acc -> Set.foldr (`Map.insert` i) acc bvAsSet)
        ANFFix _ (BoundVar varHash _) r -> do
          updateCountAt countMV r
          neededVars <- getNeededFrom neededVarsMV r
          -- Fixpoint self arguments aren't needed by fixes themselves. Thus, we
          -- remove it from used vars, but also note that it was bound here.
          let neededVars' = Set.delete varHash neededVars
          MVector.write countMV i 0
          MVector.write neededVarsMV i neededVars'
          modify (Map.insert varHash i)
        ANFApply _ f xs -> do
          updateCountAt countMV f
          traverse_ (updateCountAt countMV) xs
          neededF <- getNeededFrom neededVarsMV f
          neededXs <- traverse (getNeededFrom neededVarsMV) xs
          let neededVars = NEVector.foldl' Set.union neededF neededXs
          MVector.write countMV i 0
          MVector.write neededVarsMV i neededVars
        ANFConstr _ _ fields -> do
          traverse_ (updateCountAt countMV) fields
          neededFields <- traverse (getNeededFrom neededVarsMV) fields
          MVector.write countMV i 0
          MVector.write neededVarsMV i . fold $ neededFields
        ANFCase _ scrut handlers -> do
          updateCountAt countMV scrut
          traverse_ (updateCountAt countMV) handlers
          neededScrut <- getNeededFrom neededVarsMV scrut
          neededHandlers <- traverse (getNeededFrom neededVarsMV) handlers
          let neededVars = NEVector.foldl' Set.union neededScrut neededHandlers
          MVector.write countMV i 0
          MVector.write neededVarsMV i neededVars
        ANFCompose _ components -> do
          traverse_ (updateCountAt countMV) components
          neededComponents <- traverse (getNeededFrom neededVarsMV) components
          MVector.write countMV i 0
          MVector.write neededVarsMV i . fold $ neededComponents

bvWithMultiplicity ::
  forall (a :: Type) (m :: Type -> Type).
  MonadState (a, IntMap (ANFBind ())) m =>
  Ref -> Hash -> m BoundVar
bvWithMultiplicity r h = case r of
  AVar h' ->
    if h' == h
      then pure . BoundVar h . Multiplicity $ 1
      else error "Argument claimed as used, but body shows it as unused. If you see this, report a bug."
  AnId (Id i) -> do
    (existingBinds, _) <- gets (IntMap.split i . snd)
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

-- Pretty Printer Helpers

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
