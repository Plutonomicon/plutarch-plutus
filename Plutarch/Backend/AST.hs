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
  Multiplicity (..),

  -- * AST
  Leaf (..),
  AST (..),
  fromRawTerm,
) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Control.Monad.RWS.CPS (
  MonadReader (ask, local),
  MonadState (get),
  RWS,
  asks,
  evalRWS,
  modify,
 )
import Data.Bool (bool)
import Data.Hashable (hash)
import Data.Kind (Type)
import Data.Maybe (fromJust)
import Data.These (These (That, These, This))
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEVector
import Data.Word (Word64)
import Plutarch.Backend.PosTree (
  PosTree (
    PCase,
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
import PlutusCore (Some, ValueOf)
import PlutusCore qualified as PLC
import Prelude hiding (until)

{- | A clarity newtype for hashes, both \'structural\' and \'combined\'.

@since wip
-}
newtype Hash = Hash Int
  deriving (Eq, Ord) via Int
  deriving stock
    ( -- | @since wip
      Show
    )

{- | A hash identifying a bound variable argument, together with whether it
occurs once, or more than once, in the body where it is bound.

@since wip
-}
data Multiplicity
  = MultiplicityOne Hash
  | MultiplicityMany Hash
  deriving stock
    ( -- | @since wip
      Show
    )

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
    )

{- | A compilation-friendly abstract syntax tree. This is in contrast to
'RawTerm', which is designed to match more closely to the eDSL constructs,
and thus be easier to prettyprint and generate.

More precisely, this closely follows 'RawTerm', except that:

* 'RLet' nodes have been removed
* Lambdas and applications have been uncurried
* Position trees have been replaced by 'Multiplicity', whose hashes are
  hashes of the corresponding position tree
* Lambdas and fixpoints have been annotated with their 'Liftability'
* Lambdas that do nothing but forward their arguments (in the same order) to
  a builtin are erased and replaced with that builtin.

@since wip
-}
data AST (ann :: Type)
  = ASTLeaf (Leaf ann)
  | ASTForce ann (AST ann)
  | ASTDelay ann (AST ann)
  | ASTLam ann (NonEmptyVector (Maybe Multiplicity)) (AST ann)
  | ASTFix ann Multiplicity (AST ann)
  | ASTApply ann (AST ann) (NonEmptyVector (AST ann))
  | ASTConstr ann Word64 (Vector (AST ann))
  | ASTCase ann (AST ann) (NonEmptyVector (AST ann))
  deriving stock
    ( -- | @since wip
      Functor
    , -- | @since wip
      Show
    )

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
      RError () -> do
        let structuralHash = hash (4 :: Int)
        mkHashed structuralHash (ASTLeaf . LError)
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
        let mult = case fromJust . findVarUsage fresh (Just pt) $ body of
              (h, False) -> MultiplicityOne h
              (h, True) -> MultiplicityMany h
        (structuralHashBody, body') <- local (vmExtend fresh pt . vmMap stepDownOne) (go body)
        let structuralHash = hash (7 :: Int, structuralHashBody)
        mkHashed structuralHash (\h -> ASTFix h mult body')
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
        let structuralHash = hash (8 :: Int, tag, structuralHashesFields)
        mkHashed structuralHash (\h -> ASTConstr h tag fields')
      RCase _ scrut handlers -> do
        let len = NEVector.length handlers
        (scrutVM, handlerVMs) <- asks (vmFold separateCase (vmEmpty, NEVector.replicate1 len vmEmpty))
        (structuralHashScrut, scrut') <- local (const scrutVM) (go scrut)
        let descendCase i rt = local (const (handlerVMs NEVector.! i)) (go rt)
        (structuralHashesHandlers, handlers') <- NEVector.unzip <$> NEVector.imapM descendCase handlers
        let structuralHash = hash (9 :: Int, structuralHashScrut, NEVector.toVector structuralHashesHandlers)
        mkHashed structuralHash (\h -> ASTCase h scrut' handlers')
      RApply _ f x -> do
        (fVM, xVM) <- asks (vmFold separateTwo (vmEmpty, vmEmpty))
        (structuralHashF, f') <- local (const fVM) (go f)
        (structuralHashX, x') <- local (const xVM) (go x)
        case f' of
          -- We're part of a curried apply. We need to add one more argument.
          ASTApply _ g ys -> do
            let structuralHash = hash (structuralHashF, structuralHashX)
            mkHashed structuralHash (\h -> ASTApply h g . NEVector.snoc ys $ x')
          _ -> do
            let structuralHash = hash (10 :: Int, structuralHashF, structuralHashX)
            mkHashed structuralHash (\h -> ASTApply h f' . NEVector.singleton $ x')
      RLamAbs _ mpt body -> do
        fresh <- getFresh
        let multiplicity = case findVarUsage fresh mpt body of
              Nothing -> Nothing
              Just (h, b) -> Just . bool MultiplicityOne MultiplicityMany b $ h
        vm' <- asks (vmMap stepDownOne)
        let extendedVM = case mpt of
              Nothing -> vm'
              Just pt -> vmExtend fresh pt vm'
        (structuralHashBody, body') <- local (const extendedVM) (go body)
        let (structuralHashAll, fullMults, fullBody) = case body' of
              -- We're part of a curried lambda. We need to add one more
              -- multiplicity.
              ASTLam _ mults body'' ->
                let structuralHash = hash (structuralHashBody, mpt)
                 in (structuralHash, NEVector.cons multiplicity mults, body'')
              _ ->
                let structuralHash = hash (11 :: Int, mpt, structuralHashBody)
                 in (structuralHash, NEVector.singleton multiplicity, body')
        case fullBody of
          -- If our body is an application, we want to check that the arguments
          -- are being forwarded directly to a builtin. If they are, we should
          -- eliminate the lambda entirely in favour of that builtin.
          ASTApply _ f xs -> case alignMultsArgs fullMults xs of
            Nothing -> mkHashed structuralHashAll (\h -> ASTLam h fullMults fullBody)
            Just () -> case getBuiltinStructure f of
              Nothing -> mkHashed structuralHashAll (\h -> ASTLam h fullMults fullBody)
              -- If we've made it here, we know that we have a lambda body that
              -- just forwards the lambda's arguments, in the same order, to a
              -- builtin, possibly with some `force`s in the way. Since such a
              -- term is closed by definition, we can built its AST using the
              -- empty variable map.
              Just structure -> local (const vmEmpty) (go structure)
          _ -> mkHashed structuralHashAll (\h -> ASTLam h fullMults fullBody)

-- Helpers

-- Tries to 'dig out' a builtin, possibly wrapped in some number of `force`s. As
-- we know such terms must be closed, we can just 'rebuild' them as `RawTerm` to
-- make it easier to deal with their hashes.
getBuiltinStructure :: AST Hash -> Maybe (RawTerm ())
getBuiltinStructure = \case
  ASTLeaf (LBuiltin _ f) -> pure . RBuiltin () $ f
  ASTForce _ body -> RForce () <$> getBuiltinStructure body
  _ -> Nothing

-- Check that the arguments of a lambda align exactly with some arguments to an
-- application. This means they must:
--

-- * Have the same number;

-- * All be variables;

-- * Correspond positionally to the lambda's arguments
alignMultsArgs :: NonEmptyVector (Maybe Multiplicity) -> NonEmptyVector (AST Hash) -> Maybe ()
alignMultsArgs mults args =
  let (mult, mults') = NEVector.uncons mults
      (arg, args') = NEVector.uncons args
   in go mult arg mults' args'
  where
    go ::
      Maybe Multiplicity ->
      AST Hash ->
      Vector (Maybe Multiplicity) ->
      Vector (AST Hash) ->
      Maybe ()
    go mMult arg restMult restArgs = do
      mult <- mMult
      case arg of
        ASTLeaf (LVar _ varHash) -> do
          let multHash = case mult of MultiplicityOne h -> h; MultiplicityMany h -> h
          guard (multHash == varHash)
          case Vector.uncons restMult of
            Nothing -> case Vector.uncons restArgs of
              Nothing -> pure ()
              _ -> Nothing
            Just (mMult', restMult') -> do
              (arg', restArgs') <- Vector.uncons restArgs
              go mMult' arg' restMult' restArgs'
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

getFresh ::
  forall (a :: Type) (m :: Type -> Type).
  (MonadState a m, Num a) =>
  m a
getFresh = do
  fresh <- get
  modify (+ 1)
  pure fresh

findVarUsage :: Word64 -> Maybe PosTree -> RawTerm () -> Maybe (Hash, Bool)
findVarUsage name mpt t = case mpt of
  Nothing -> Nothing
  Just PHere -> case t of
    RVar _ _ -> do
      let structuralHash = hash (0 :: Int)
      let trivialVM = vmSingleton name PHere
      let combinedHash = hash (structuralHash, trivialVM)
      pure (Hash combinedHash, False)
    _ -> Nothing
  Just (POne pt) -> case t of
    RFix _ _ body -> findVarUsage name (Just pt) body
    RLamAbs _ _ body -> findVarUsage name (Just pt) body
    RForce _ body -> findVarUsage name (Just pt) body
    RDelay _ body -> findVarUsage name (Just pt) body
    _ -> Nothing
  Just (PTwo pts) -> case t of
    RLet _ _ v f -> case pts of
      This pt -> findVarUsage name (Just pt) v
      That pt -> findVarUsage name (Just pt) f
      -- Bind is likely to be smaller, so we'll search there.
      These ptv _ -> (\(h, _) -> (h, True)) <$> findVarUsage name (Just ptv) v
    RApply _ f x -> case pts of
      This pt -> findVarUsage name (Just pt) f
      That pt -> findVarUsage name (Just pt) x
      -- Argument is likely to be smaller, so we'll search there.
      These _ ptv -> (\(h, _) -> (h, True)) <$> findVarUsage name (Just ptv) x
    _ -> Nothing
  Just (PMany pts) -> case t of
    RConstr _ _ fields -> Vector.foldl' go Nothing . Vector.zip pts $ fields
    _ -> Nothing
  Just (PCase pt pts) -> case t of
    RCase _ scrut handlers ->
      let resScrut = findVarUsage name pt scrut
       in case resScrut of
            Just (_, True) -> resScrut
            _ -> NEVector.foldl' go resScrut . NEVector.zip pts $ handlers
    _ -> Nothing
  where
    go :: Maybe (Hash, Bool) -> (Maybe PosTree, RawTerm ()) -> Maybe (Hash, Bool)
    go acc (mptInner, tInner) = case acc of
      Nothing -> findVarUsage name mptInner tInner
      Just (h, False) -> case mptInner of
        Nothing -> acc
        Just _ -> Just (h, True)
      Just (_, True) -> acc
