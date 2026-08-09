{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoPartialTypeSignatures #-}

{- | A collection of helper types, as well as more compilation-oriented abstract
syntax tree. Acts as a first stage in compilation.

= A note on hashes

Two kinds of hashes frequently get used in this module: a \'structural\' hash
and a \'combined\' one. This corresponds directly to the hash of the
@Structure@ data type, and the entire e-summary, respectively; both of these
are described in the /Hashing Modulo Alpha-Equivalence/ paper. Intuitively,
the \'structural\' hash ignores all variable naming, following /only/ the
structure of the 'RawTerm', while a \'combined\' hash also includes the
'VarMap' at that subcomputation.

= Links

- [The original paper](https://arxiv.org/pdf/2105.02856)

@since wip
-}
module Plutarch.Backend.AST (
  -- * Common
  Hash (..),

  -- * AST
  Leaf (..),
  AST (..),
  fromRawTerm,
  astLeafAnn,
  astNodeAnn,
) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Control.Monad.RWS.CPS (
  MonadReader (ask, local),
  RWS,
  asks,
  evalRWS,
 )
import Data.Hashable (Hashable (hash))
import Data.Kind (Type)
import Data.These (These (That, These, This))
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEVector
import Data.Word (Word64)
import Plutarch.Backend.PosTree (
  PosTree (
    PCase,
    PCompose,
    PHere,
    PMany,
    POne,
    PTwo
  ),
 )
import Plutarch.Backend.RawTerm (
  RawTerm (
    RApply,
    RBuiltin,
    RCase,
    RCompiled,
    RCompose,
    RConstant,
    RConstr,
    RDelay,
    RError,
    RFix,
    RForce,
    RLamAbs,
    RLet,
    RPlaceholder,
    RVar
  ),
 )
import Plutarch.Backend.UPLC (UPLCTerm)
import Plutarch.Backend.VarMap (
  VarMap,
  vmEmpty,
  vmExtend,
  vmFold,
  vmMap,
  vmMerge,
  vmSingleton,
 )
import Plutarch.Helpers.Backend (getFresh)
import Plutarch.Helpers.Pretty (
  blockList,
  compactReadableVar,
  prettyValueOf,
  taggedNode,
 )
import PlutusCore (Some (Some), ValueOf (ValueOf))
import PlutusCore qualified as PLC
import Prettyprinter (
  Doc,
  Pretty (pretty),
  align,
  braces,
  brackets,
  flatAlt,
  group,
  hardline,
  hsep,
  indent,
  viaShow,
  (<+>),
 )
import Prelude hiding (until)

{- | A clarity newtype for hashes, both \'structural\' and \'combined\'.

@since wip
-}
newtype Hash = Hash Int
  deriving (Eq, Ord, Hashable) via Int
  deriving stock
    ( -- | @since wip
      Show
    )

-- Hashing will give huge Ints which are hard to read
-- so we turn them into something readable

-- | @since wip
instance Pretty Hash where
  pretty (Hash h) = pretty . compactReadableVar . fromIntegral $ h

{- | A leaf computation (namely, one that cannot have dependencies).

@since wip
-}
data Leaf (ann :: Type)
  = LVar ann Hash
  | LConstant ann (Some (ValueOf PLC.DefaultUni))
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
    LVar _ h ->
      pretty h
    LConstant _ (Some (ValueOf uni x)) -> prettyValueOf uni x
    LBuiltin _ bi -> viaShow bi
    LCompiled _ uplcTerm -> "COMPILED" <> ":" <> align (braces (align . group $ pretty uplcTerm))
    LError _ -> "ERROR"

{- | A compilation-friendly abstract syntax tree. This is in contrast to
'RawTerm', which is designed to match more closely to the eDSL constructs,
and thus be easier to prettyprint and generate.

More precisely, this closely follows 'RawTerm', except that:

* 'RLet' nodes have been removed
* Lambdas and applications have been uncurried
* Position trees have been replaced by 'BoundVar's, whose hashes are
  hashes of the corresponding position tree
* Lambdas that do nothing but forward their arguments (in the same order) to
  a builtin are erased and replaced with that builtin.
* Applications involving literal error nodes (as either the function or an
  argument) are replaced with the error node.
* `constr` nodes involving literal error nodes are replaced with the error
  node.
* `case` nodes scrutinizing the error node are replaced with the error node.
* `Force` adjacent to `Delay` have been replaced with just the body of `Delay`.

@since wip
-}
data AST (ann :: Type)
  = ASTLeaf (Leaf ann)
  | ASTForce ann (AST ann)
  | ASTDelay ann (AST ann)
  | ASTLam ann (NonEmptyVector (Maybe Hash)) (AST ann)
  | ASTFix ann Hash (AST ann)
  | ASTApply ann (AST ann) (NonEmptyVector (AST ann))
  | ASTConstr ann Word64 (Vector (AST ann))
  | ASTCase ann (AST ann) (NonEmptyVector (AST ann))
  | ASTCompose ann (NonEmptyVector (AST ann))
  deriving stock
    ( -- | @since wip
      Functor
    , -- | @since wip
      Show
    , -- | @since wip
      Eq
    )

-- For the sake of consistency this uses the same formatting as the overlapping instance,
-- although this *could* be prettier if we wanted it to be.

-- | @since wip
instance {-# OVERLAPPABLE #-} Pretty (AST ()) where
  pretty = \case
    ASTLeaf l -> pretty l
    ASTForce _ arg -> "force" <+> pretty arg
    ASTDelay _ arg -> "delay" <+> pretty arg
    ASTLam _ vars body -> "\\" <> mkArgs vars <+> "->" <> hardline <> indent 2 (pretty body)
    ASTFix _ self body -> "Fix" <> brackets (pretty self) <+> pretty body
    ASTApply _ fun args ->
      "Apply"
        <+> hardline
        <+> align (indent 2 (pretty fun))
        <+> hardline
        <+> align (indent 2 (blockList . map pretty . NEVector.toList $ args))
    ASTConstr _ cix args -> "Constr" <> brackets (pretty cix) <+> blockList (pretty <$> Vector.toList args)
    ASTCase _ scrut handlers ->
      "case"
        <+> pretty scrut
        <+> hardline
        <> indent 2 (blockList . map pretty . NEVector.toList $ handlers)
    ASTCompose _ args -> "Compose" <+> align (blockList (pretty <$> NEVector.toList args))

-- | @since wip
instance {-# OVERLAPS #-} Pretty ann => Pretty (AST ann) where
  pretty = \case
    ASTLeaf l -> case l of
      LVar {} -> pretty l
      _ -> taggedNode "" (pretty $ astLeafAnn l) $ pretty l
    ASTForce ann arg -> taggedNode "" (pretty ann) $ "force" <+> pretty arg
    ASTDelay ann arg -> taggedNode "" (pretty ann) $ "delay" <+> pretty arg
    ASTLam ann vars body ->
      let cxt = "\\" <> mkArgs vars <+> "-> "
       in taggedNode cxt (pretty ann)
            . align
            . group
            $ pretty body
    ASTFix ann self body ->
      let oneLine = brackets (pretty self) <+> pretty body
          multiLine = brackets (pretty self) <> hardline <> group (pretty body)
       in taggedNode "Fix" (pretty ann) $ flatAlt multiLine oneLine
    ASTApply ann fun args ->
      let oneLine = align (pretty fun) <+> align (blockList . map pretty . NEVector.toList $ args)
          multiLine = align $ align (pretty fun) <> hardline <> align (blockList . map pretty . NEVector.toList $ args)
       in taggedNode "Apply" (pretty ann) $ flatAlt multiLine oneLine
    ASTConstr ann cix args ->
      taggedNode "Constr " (pretty ann) $ brackets (pretty cix) <+> blockList (pretty <$> Vector.toList args)
    ASTCase ann scrut handlers ->
      taggedNode "case" (pretty ann)
        . align
        . group
        $ pretty scrut
          <+> hardline
          <> (blockList . map pretty . NEVector.toList $ handlers)
    ASTCompose ann args ->
      taggedNode "Compose" (pretty ann) $ align (blockList (pretty <$> NEVector.toList args))

{- | Given a 'RawTerm', construct its AST, using hashing to mark
alpha-equivalent subcomputations.

@since wip
-}
fromRawTerm :: RawTerm () -> AST Hash
fromRawTerm t = snd . fst . evalRWS (go t) vmEmpty $ 0
  where
    go :: RawTerm () -> RWS VarMap () Word64 (Int, AST Hash)
    go = \case
      RVar _ _ -> do
        let structuralHash = hash (0 :: Int)
        mkHashed structuralHash (\h -> ASTLeaf . LVar h $ h)
      RConstant _ c -> do
        let structuralHash = hash (1 :: Int, c)
        mkHashed structuralHash (\h -> ASTLeaf (LConstant h c))
      RBuiltin _ f -> do
        let structuralHash = hash (2 :: Int, f)
        mkHashed structuralHash (\h -> ASTLeaf (LBuiltin h f))
      RCompiled _ code -> do
        let structuralHash = hash (3 :: Int, code)
        mkHashed structuralHash (\h -> ASTLeaf (LCompiled h code))
      RError () -> mkHashed errorHash (ASTLeaf . LError)
      RPlaceholder _ _ -> go (RError ())
      RForce _ body -> do
        vm' <- asks (vmMap stepDownOne)
        case body of
          -- We have a Force directly next to a Delay, which is effectively id.
          -- We have to step down the VarMap before we continue here.
          RDelay _ body' -> local (const (vmMap stepDownOne vm')) (go body')
          _ -> do
            (structuralHashBody, body') <- local (const vm') (go body)
            let structuralHash = hash (5 :: Int, structuralHashBody)
            mkHashed structuralHash (`ASTForce` body')
      RDelay _ body -> do
        (structuralHashBody, body') <- local (vmMap stepDownOne) (go body)
        let structuralHash = hash (6 :: Int, structuralHashBody)
        mkHashed structuralHash (`ASTDelay` body')
      RFix _ pt body -> do
        fresh <- getFresh
        let boundVar = mkVarHash fresh
        (structuralHashBody, body') <- local (vmExtend fresh pt . vmMap stepDownOne) (go body)
        let structuralHash = hash (7 :: Int, structuralHashBody)
        mkHashed structuralHash (\h -> ASTFix h boundVar body')
      RLet _ mpt v f -> do
        let node = RApply () (RLamAbs () mpt f) v
        (vmv, vmf) <- asks (vmFold separateTwo (vmEmpty, vmEmpty))
        let vmf' = vmMap POne vmf
        let extendedVMV = vmMap (PTwo . That) vmv
        let extendedVMF = vmMap (PTwo . This) vmf'
        let vm' = vmMerge mergeLet extendedVMF extendedVMV
        local (const vm') (go node)
      RConstr _ tag fields -> do
        let len = Vector.length fields
        fieldVMs <- asks (vmFold separateConstr (Vector.replicate len vmEmpty))
        let descendConstr i rt = local (const (fieldVMs Vector.! i)) (go rt)
        (structuralHashesFields, fields') <- Vector.unzip <$> Vector.imapM descendConstr fields
        -- If any of our fields are the error node, we'll get the error node no
        -- matter what else we have lying around.
        if Vector.any (errorHash ==) structuralHashesFields
          then mkHashed errorHash (ASTLeaf . LError)
          else do
            let structuralHash = hash (8 :: Int, tag, structuralHashesFields)
            mkHashed structuralHash (\h -> ASTConstr h tag fields')
      RCase _ scrut handlers -> do
        let len = NEVector.length handlers
        (scrutVM, handlerVMs) <- asks (vmFold separateCase (vmEmpty, NEVector.replicate1 len vmEmpty))
        (structuralHashScrut, scrut') <- local (const scrutVM) (go scrut)
        -- If we're scrutinizing the error node, we'll get the error node no
        -- matter what else we have lying around.
        if structuralHashScrut == errorHash
          then mkHashed errorHash (ASTLeaf . LError)
          else do
            let descendCase i rt = local (const (handlerVMs NEVector.! i)) (go rt)
            (structuralHashesHandlers, handlers') <- NEVector.unzip <$> NEVector.imapM descendCase handlers
            let structuralHash = hash (9 :: Int, structuralHashScrut, NEVector.toVector structuralHashesHandlers)
            mkHashed structuralHash (\h -> ASTCase h scrut' handlers')
      RApply _ f x -> do
        (fVM, xVM) <- asks (vmFold separateTwo (vmEmpty, vmEmpty))
        (structuralHashF, f') <- local (const fVM) (go f)
        -- If we're trying to apply arguments to the error node, we'll get the
        -- error node no matter what.
        if structuralHashF == errorHash
          then mkHashed errorHash (ASTLeaf . LError)
          else do
            (structuralHashX, x') <- local (const xVM) (go x)
            -- If we try to apply the error node to anything, we'll get the
            -- error node no matter what.
            if structuralHashX == errorHash
              then mkHashed errorHash (ASTLeaf . LError)
              else case f' of
                -- We're part of a curried apply, none of whose arguments are
                -- the error node. We need to add one more argument.
                ASTApply _ g ys -> do
                  let structuralHash = hash (structuralHashF, structuralHashX)
                  mkHashed structuralHash (\h -> ASTApply h g . NEVector.snoc ys $ x')
                -- We are neither an error node, nor another application.
                _ -> do
                  let structuralHash = hash (10 :: Int, structuralHashF, structuralHashX)
                  mkHashed structuralHash (\h -> ASTApply h f' . NEVector.singleton $ x')
      RLamAbs _ mpt body -> do
        fresh <- getFresh
        let mbv = mkVarHash fresh <$ mpt
        vm' <- asks (vmMap stepDownOne)
        let extendedVM = case mpt of
              Nothing -> vm'
              Just pt -> vmExtend fresh pt vm'
        (structuralHashBody, body') <- local (const extendedVM) (go body)
        let (structuralHashAll, fullBVs, fullBody) = case body' of
              -- We're part of a curried lambda. We need to add one more bound
              -- var.
              ASTLam _ bvs body'' ->
                let structuralHash = hash (structuralHashBody, mpt)
                 in (structuralHash, NEVector.cons mbv bvs, body'')
              _ ->
                let structuralHash = hash (11 :: Int, mbv, structuralHashBody)
                 in (structuralHash, NEVector.singleton mbv, body')
        case fullBody of
          -- If our body is an application, we want to check that the arguments
          -- are being forwarded directly to a builtin. If they are, we
          -- should eliminate the lambda entirely in favour of that builtin.
          ASTApply _ f xs -> case alignBVsArgs fullBVs xs of
            -- Not a literal forward, nothing to do.
            Nothing -> mkHashed structuralHashAll (\h -> ASTLam h fullBVs fullBody)
            Just () -> case getBuiltinStructure f of
              Nothing -> mkHashed structuralHashAll (\h -> ASTLam h fullBVs fullBody)
              -- If we made it here, we know that we have a lambda body that
              -- just forwards its arguments, in the same order, to a builtin,
              -- possibly with some `force`s in the way. Since such a term is
              -- closed by definition, we can build its AST using the empty
              -- varmap.
              Just structure -> local (const vmEmpty) (go structure)
          _ -> mkHashed structuralHashAll (\h -> ASTLam h fullBVs fullBody)
      RCompose _ components -> do
        let len = NEVector.length components
        fieldVMs <- asks (vmFold separateCompose (NEVector.replicate1 len vmEmpty))
        let descendCompose i rt = local (const (fieldVMs NEVector.! i)) (go rt)
        (structuralHashesComponents, components') <- NEVector.unzip <$> NEVector.imapM descendCompose components
        let structuralHash = hash (12 :: Int, NEVector.toVector structuralHashesComponents)
        mkHashed structuralHash (`ASTCompose` components')

-- Helpers

errorHash :: Int
errorHash = hash (4 :: Int)

-- Tries to 'dig out' a builtin, possibly wrapped in some number of `force`s. As
-- we know such terms must be closed, we can just 'rebuild' them as `RawTerm` to
-- make it easier to deal with their hashes.
getBuiltinStructure :: AST Hash -> Maybe (RawTerm ())
getBuiltinStructure = \case
  ASTLeaf (LBuiltin _ f) -> pure . RBuiltin () $ f
  ASTForce _ body -> RForce () <$> getBuiltinStructure body
  _ -> Nothing

-- check that the bound vars of a lambda align exactly with some arguments to an
-- application.
alignBVsArgs ::
  NonEmptyVector (Maybe Hash) ->
  NonEmptyVector (AST Hash) ->
  Maybe ()
alignBVsArgs bvs args =
  let (bv, bvs') = NEVector.uncons bvs
      (arg, args') = NEVector.uncons args
   in go bv arg bvs' args'
  where
    go ::
      Maybe Hash ->
      AST Hash ->
      Vector (Maybe Hash) ->
      Vector (AST Hash) ->
      Maybe ()
    go mBv arg restBVs restArgs = do
      bvHash <- mBv
      case arg of
        ASTLeaf (LVar _ varHash) -> do
          guard (bvHash == varHash)
          case Vector.uncons restBVs of
            Nothing -> case Vector.uncons restArgs of
              Nothing -> pure ()
              Just _ -> Nothing
            Just (mBV', restBVs') -> do
              (arg', restArgs') <- Vector.uncons restArgs
              go mBV' arg' restBVs' restArgs'
        _ -> Nothing

mergeLet :: PosTree -> PosTree -> PosTree
mergeLet (PCase f1 xs1) (PCase f2 xs2) = PCase (f1 <|> f2) (NEVector.zipWith (<|>) xs1 xs2)
mergeLet x _ = x -- impossible

mkHashed ::
  forall (m :: Type -> Type).
  MonadReader VarMap m =>
  Int -> (Hash -> AST Hash) -> m (Int, AST Hash)
mkHashed structuralHash f = do
  vm <- ask
  let combinedHash = hash (structuralHash, vm)
  pure (structuralHash, f . Hash $ combinedHash)

stepDownOne :: PosTree -> PosTree
stepDownOne = \case
  POne t -> t
  t -> t

separateTwo :: (VarMap, VarMap) -> Word64 -> PosTree -> (VarMap, VarMap)
separateTwo acc@(accL, accR) k = \case
  PTwo ts -> case ts of
    This tl -> (vmExtend k tl accL, accR)
    That tr -> (accL, vmExtend k tr accR)
    These tl tr -> (vmExtend k tl accL, vmExtend k tr accR)
  _ -> acc

separateConstr :: Vector VarMap -> Word64 -> PosTree -> Vector VarMap
separateConstr acc k = \case
  PMany ts -> Vector.zipWith go acc ts
  _ -> acc
  where
    go :: VarMap -> Maybe PosTree -> VarMap
    go vm = \case
      Nothing -> vm
      Just t -> vmExtend k t vm

separateCompose :: NonEmptyVector VarMap -> Word64 -> PosTree -> NonEmptyVector VarMap
separateCompose acc k = \case
  PCompose ts -> NEVector.zipWith go acc ts
  _ -> acc
  where
    go :: VarMap -> Maybe PosTree -> VarMap
    go vm = \case
      Nothing -> vm
      Just t -> vmExtend k t vm

separateCase :: (VarMap, NonEmptyVector VarMap) -> Word64 -> PosTree -> (VarMap, NonEmptyVector VarMap)
separateCase acc@(scrutVM, handlerVMs) k = \case
  PCase mpt mpts -> case mpt of
    Nothing -> (scrutVM, NEVector.zipWith go handlerVMs mpts)
    Just pt -> (vmExtend k pt scrutVM, NEVector.zipWith go handlerVMs mpts)
  _ -> acc
  where
    go :: VarMap -> Maybe PosTree -> VarMap
    go vm = \case
      Nothing -> vm
      Just t -> vmExtend k t vm

mkVarHash :: Word64 -> Hash
mkVarHash fresh = Hash (hash (hash (0 :: Int), vmSingleton fresh PHere))

astLeafAnn :: forall (ann :: Type). Leaf ann -> ann
astLeafAnn = \case
  LVar ann _ -> ann
  LConstant ann _ -> ann
  LBuiltin ann _ -> ann
  LCompiled ann _ -> ann
  LError ann -> ann

astNodeAnn :: forall (ann :: Type). AST ann -> ann
astNodeAnn = \case
  ASTLeaf l -> astLeafAnn l
  ASTForce ann _ -> ann
  ASTDelay ann _ -> ann
  ASTLam ann _ _ -> ann
  ASTFix ann _ _ -> ann
  ASTApply ann _ _ -> ann
  ASTConstr ann _ _ -> ann
  ASTCase ann _ _ -> ann
  ASTCompose ann _ -> ann

mkArgs :: NonEmptyVector (Maybe Hash) -> Doc ann
mkArgs (NEVector.toList -> xs) =
  hsep
    . fmap (\case Nothing -> "_"; Just m -> pretty m)
    $ xs
