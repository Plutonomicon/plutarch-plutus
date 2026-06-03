{-# LANGUAGE NoPartialTypeSignatures #-}

module Plutarch.Backend.AST (
  -- * Common
  Hash (..),
  Multiplicity (..),
  Liftability (..),
  Leaf (..),

  -- * AST
  AST (..),
  fromRawTerm,

  -- * ANF
  ANFBind (..),
  ANF (..),
  fromHashedAST,
) where

import Control.Applicative ((<|>))
import Control.Monad.RWS.CPS (
  MonadReader (ask, local),
  MonadState (get),
  RWS,
  asks,
  evalRWS,
  modify,
 )
import Control.Monad.State.Strict (State, gets, runState)
import Data.Bifunctor (bimap)
import Data.Bimap (Bimap)
import Data.Bimap qualified as Bimap
import Data.Bool (bool)
import Data.Hashable (hash)
import Data.IntMap (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Kind (Type)
import Data.Maybe (fromJust)
import Data.These (These (That, These, This))
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEVector
import Data.Word (Word64)
import Plutarch.Backend.PosTree (PosTree (PApplyCase, PHere, PMany, POne, PTwo))
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

newtype Hash = Hash Int
  deriving (Eq, Ord) via Int
  deriving stock (Show)

data Multiplicity
  = MultiplicityOne Hash
  | MultiplicityMany Hash
  deriving stock (Show)

data Liftability = Trivial | NonTrivial
  deriving stock (Show)

data Leaf (ann :: Type)
  = LVar ann Hash
  | LConstant ann (Some (ValueOf PLC.DefaultUni))
  | LBuiltin ann PLC.DefaultFun
  | LCompiled ann UPLCTerm
  | LError ann
  deriving stock (Functor, Show)

data AST (ann :: Type)
  = ASTLeaf (Leaf ann)
  | ASTForce ann (AST ann)
  | ASTDelay ann (AST ann)
  | ASTLam ann (NonEmptyVector (Maybe Multiplicity)) Liftability (AST ann)
  | ASTFix ann Multiplicity Liftability (AST ann)
  | ASTApply ann (AST ann) (NonEmptyVector (AST ann))
  | ASTConstr ann Word64 (Vector (AST ann))
  | ASTCase ann (AST ann) (NonEmptyVector (AST ann))
  deriving stock (Functor, Show)

newtype Id = Id Int
  deriving (Eq, Ord) via Int
  deriving stock (Show)

data ANFBind (ann :: Type)
  = ANFLeaf (Leaf ann)
  | ANFForce ann Id
  | ANFDelay ann Id
  | ANFLam ann (NonEmptyVector (Maybe Multiplicity)) Liftability Id
  | ANFFix ann Multiplicity Liftability Id
  | ANFApply ann Id (NonEmptyVector Id)
  | ANFConstr ann Word64 (Vector Id)
  | ANFCase ann Id (NonEmptyVector Id)
  deriving stock (Show)

data ANF (ann :: Type) = ANF (Bimap Id Hash) (NonEmptyVector (ANFBind ann))

fromHashedAST :: AST Hash -> ANF ()
fromHashedAST ast = case runState (go ast) (Bimap.empty, IntMap.empty) of
  (_, (bm, im)) -> ANF bm . NEVector.generate1 (IntMap.size im) $ \i -> fromJust . IntMap.lookup i $ im
  where
    go :: AST Hash -> State (Bimap Id Hash, IntMap (ANFBind ())) Id
    go = \case
      ASTLeaf ell -> doLeaf ell
      ASTForce h body -> withLookup h $ do
        bodyId <- go body
        newBind h (ANFForce () bodyId)
      ASTDelay h body -> withLookup h $ do
        bodyId <- go body
        newBind h (ANFDelay () bodyId)
      ASTLam h mults liftability body -> withLookup h $ do
        bodyId <- go body
        newBind h (ANFLam () mults liftability bodyId)
      ASTFix h mult liftability body -> withLookup h $ do
        bodyId <- go body
        newBind h (ANFFix () mult liftability bodyId)
      ASTApply h f xs -> withLookup h $ do
        fId <- go f
        xsIds <- traverse go xs
        newBind h (ANFApply () fId xsIds)
      ASTConstr h tag fields -> withLookup h $ do
        fieldsIds <- traverse go fields
        newBind h (ANFConstr () tag fieldsIds)
      ASTCase h scrut handlers -> withLookup h $ do
        scrutId <- go scrut
        handlersIds <- traverse go handlers
        newBind h (ANFCase () scrutId handlersIds)
    doLeaf :: Leaf Hash -> State (Bimap Id Hash, IntMap (ANFBind ())) Id
    doLeaf = \case
      LVar _ h -> withLookup h $ newBind h (ANFLeaf (LVar () h))
      LConstant h c -> withLookup h $ newBind h (ANFLeaf (LConstant () c))
      LBuiltin h f -> withLookup h $ newBind h (ANFLeaf (LBuiltin () f))
      LCompiled h code -> withLookup h $ newBind h (ANFLeaf (LCompiled () code))
      LError h -> withLookup h $ newBind h (ANFLeaf (LError ()))
    withLookup ::
      Hash ->
      State (Bimap Id Hash, IntMap (ANFBind ())) Id ->
      State (Bimap Id Hash, IntMap (ANFBind ())) Id
    withLookup h act = do
      mref <- gets (Bimap.lookupR h . fst)
      maybe act pure mref
    newBind :: Hash -> ANFBind () -> State (Bimap Id Hash, IntMap (ANFBind ())) Id
    newBind h bind = do
      firstAvailable <- gets (maybe 0 ((+ 1) . fst) . IntMap.lookupMax . snd)
      let asId = Id firstAvailable
      modify (bimap (Bimap.insert asId h) (IntMap.insert firstAvailable bind))
      pure asId

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
        liftability <- asks (bool NonTrivial Trivial . (vmEmpty ==))
        (structuralHashBody, body') <- local (vmExtend fresh pt . vmMap stepDownOne) (go body)
        let structuralHash = hash (7 :: Int, structuralHashBody)
        mkHashed structuralHash (\h -> ASTFix h mult liftability body')
      RLet _ mpt v f -> do
        let node = RApply () (RLamAbs () (NEVector.singleton mpt) f) . NEVector.singleton $ v
        (vmv, vmf) <- asks (vmFold separateTwo (vmEmpty, vmEmpty))
        let vmf' = vmMap POne vmf
        let extendedVMV = vmMap (PApplyCase Nothing . NEVector.singleton . Just) vmv
        let extendedVMF = vmMap (\pt -> PApplyCase (Just pt) (NEVector.singleton Nothing)) vmf'
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
        (scrutVM, handlerVMs) <- asks (vmFold separateApplyCase (vmEmpty, NEVector.replicate1 len vmEmpty))
        (structuralHashScrut, scrut') <- local (const scrutVM) (go scrut)
        let descendCase i rt = local (const (handlerVMs NEVector.! i)) (go rt)
        (structuralHashesHandlers, handlers') <- NEVector.unzip <$> NEVector.imapM descendCase handlers
        let structuralHash = hash (9 :: Int, structuralHashScrut, NEVector.toVector structuralHashesHandlers)
        mkHashed structuralHash (\h -> ASTCase h scrut' handlers')
      RApply _ f xs -> do
        let len = NEVector.length xs
        (fVM, xsVMs) <- asks (vmFold separateApplyCase (vmEmpty, NEVector.replicate1 len vmEmpty))
        (structuralHashF, f') <- local (const fVM) (go f)
        let descendApply i rt = local (const (xsVMs NEVector.! i)) (go rt)
        (structuralHashesXs, xs') <- NEVector.unzip <$> NEVector.imapM descendApply xs
        let structuralHash = hash (10 :: Int, structuralHashF, NEVector.toVector structuralHashesXs)
        mkHashed structuralHash (\h -> ASTApply h f' xs')
      RLamAbs _ mpts body -> do
        let len = NEVector.length mpts
        withFreshes <- NEVector.zip <$> NEVector.replicate1M len getFresh <*> pure mpts
        let multiplicities = fmap (\(name, mpt) -> (\(h, b) -> bool MultiplicityOne MultiplicityMany b h) <$> findVarUsage name mpt body) withFreshes
        liftability <- asks (bool NonTrivial Trivial . (vmEmpty ==))
        vm' <- asks (vmMap stepDownOne)
        let bodyVM = NEVector.foldl' (\acc (k, mv) -> case mv of Nothing -> acc; Just v -> vmExtend k v acc) vm' withFreshes
        (structuralHashBody, body') <- local (const bodyVM) (go body)
        let structuralHash = hash (11 :: Int, NEVector.toVector mpts, structuralHashBody)
        mkHashed structuralHash (\h -> ASTLam h multiplicities liftability body')

-- Helpers

mergeLet :: PosTree -> PosTree -> PosTree
mergeLet (PApplyCase f1 xs1) (PApplyCase f2 xs2) = PApplyCase (f1 <|> f2) (NEVector.zipWith (<|>) xs1 xs2)
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

separateApplyCase :: (VarMap, NonEmptyVector VarMap) -> Word64 -> PosTree -> (VarMap, NonEmptyVector VarMap)
separateApplyCase acc@(scrutVM, handlerVMs) k = \case
  PApplyCase mpt mpts -> case mpt of
    Nothing -> (scrutVM, NEVector.zipWith go handlerVMs mpts)
    Just pt -> (vmExtend k pt scrutVM, NEVector.zipWith go handlerVMs mpts)
  _ -> acc
  where
    go :: VarMap -> Maybe PosTree -> VarMap
    go vm = \case
      Nothing -> vm
      Just t -> vmExtend k t vm

getFresh ::
  forall (m :: Type -> Type).
  MonadState Word64 m =>
  m Word64
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
      These ptv _ -> (\(h, _) -> (h, True)) <$> findVarUsage name (Just ptv) v
    _ -> Nothing
  Just (PMany pts) -> case t of
    RConstr _ _ fields -> Vector.foldl' go Nothing . Vector.zip pts $ fields
    _ -> Nothing
  Just (PApplyCase pt pts) -> case t of
    RCase _ scrut handlers ->
      let resScrut = findVarUsage name pt scrut
       in case resScrut of
            Just (_, True) -> resScrut
            _ -> NEVector.foldl' go resScrut . NEVector.zip pts $ handlers
    RApply _ f xs ->
      let resF = findVarUsage name pt f
       in case resF of
            Just (_, True) -> resF
            _ -> NEVector.foldl' go resF . NEVector.zip pts $ xs
    _ -> Nothing
  where
    go :: Maybe (Hash, Bool) -> (Maybe PosTree, RawTerm ()) -> Maybe (Hash, Bool)
    go acc (mptInner, tInner) = case acc of
      Nothing -> findVarUsage name mptInner tInner
      Just (h, False) -> case mptInner of
        Nothing -> acc
        Just _ -> Just (h, True)
      Just (_, True) -> acc
