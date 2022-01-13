module Plutarch.Internal (
  -- | $hoisted
  (:-->),
  PDelayed,
  -- | $term
  Term (Term, asRawTerm),
  mapTerm,
  plam',
  plet,
  papp,
  pdelay,
  pforce,
  phoistAcyclic,
  perror,
  punsafeCoerce,
  punsafeBuiltin,
  punsafeConstant,
  punsafeConstantInternal,
  compile,
  ClosedTerm,
  Dig,
  hashTerm,
  hashOpenTerm,
  RawTerm (..),
  TermCont (..),
  TermResult (TermResult, getDeps, getTerm),
) where

import Crypto.Hash (Context, Digest, hashFinalize, hashInit, hashUpdate)
import Crypto.Hash.Algorithms (Blake2b_160)
import Crypto.Hash.IO (HashAlgorithm)
import qualified Data.ByteString as BS
import Data.Kind (Type)
import Data.List (foldl', groupBy, sortOn)
import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import qualified Flat.Run as F
import GHC.Stack (HasCallStack)
import Numeric.Natural (Natural)
import Plutarch.Evaluate (evaluateScript)
import Plutus.V1.Ledger.Scripts (Script (Script))
import PlutusCore (Some, ValueOf)
import qualified PlutusCore as PLC
import PlutusCore.DeBruijn (DeBruijn (DeBruijn), Index (Index))
import qualified UntypedPlutusCore as UPLC

{- $hoisted
 __Explanation for hoisted terms:__
 Hoisting is a convenient way of importing terms without duplicating them
 across your tree. Currently, hoisting is only supported on terms that do
 not refer to any free variables.

 An RHoisted contains a term and its hash. A RawTerm will have a DAG
 of hoisted terms, where an edge represents a dependency.
 We topologically sort these hoisted terms, such that each has an index.

 We wrap our RawTerm in RLamAbs and RApply in an order corresponding to the
 indices. Each level can refer to levels above it by the nature of De Bruijn naming,
 though the name is relative to the current level.
-}

type Dig = Digest Blake2b_160

data HoistedTerm = HoistedTerm Dig RawTerm
  deriving stock (Show)

data RawTerm
  = RVar Natural
  | RLamAbs Natural RawTerm
  | RApply RawTerm [RawTerm]
  | RForce RawTerm
  | RDelay RawTerm
  | RConstant (Some (ValueOf PLC.DefaultUni))
  | RBuiltin PLC.DefaultFun
  | RError
  | RHoisted HoistedTerm
  deriving stock (Show)

hashRawTerm' :: HashAlgorithm alg => RawTerm -> Context alg -> Context alg
hashRawTerm' (RVar x) = flip hashUpdate ("0" :: BS.ByteString) . flip hashUpdate (F.flat (fromIntegral x :: Integer))
hashRawTerm' (RLamAbs n x) = flip hashUpdate ("1" :: BS.ByteString) . flip hashUpdate (F.flat (fromIntegral n :: Integer)) . hashRawTerm' x
hashRawTerm' (RApply x y) = flip hashUpdate ("2" :: BS.ByteString) . hashRawTerm' x . flip (foldl' $ flip hashRawTerm') y
hashRawTerm' (RForce x) = flip hashUpdate ("3" :: BS.ByteString) . hashRawTerm' x
hashRawTerm' (RDelay x) = flip hashUpdate ("4" :: BS.ByteString) . hashRawTerm' x
hashRawTerm' (RConstant x) = flip hashUpdate ("5" :: BS.ByteString) . flip hashUpdate (F.flat x)
hashRawTerm' (RBuiltin x) = flip hashUpdate ("6" :: BS.ByteString) . flip hashUpdate (F.flat x)
hashRawTerm' RError = flip hashUpdate ("7" :: BS.ByteString)
hashRawTerm' (RHoisted (HoistedTerm hash _)) = flip hashUpdate ("8" :: BS.ByteString) . flip hashUpdate hash

hashRawTerm :: RawTerm -> Dig
hashRawTerm t = hashFinalize . hashRawTerm' t $ hashInit

data TermResult = TermResult
  { getTerm :: RawTerm
  , getDeps :: [HoistedTerm]
  }

mapTerm :: (RawTerm -> RawTerm) -> TermResult -> TermResult
mapTerm f (TermResult t d) = TermResult (f t) d

mkTermRes :: RawTerm -> TermResult
mkTermRes r = TermResult r []
{- $term
 Source: Unembedding Domain-Specific Languages by Robert Atkey, Sam Lindley, Jeremy Yallop
 Thanks!
 NB: Hoisted terms must be sorted such that the dependents are first and dependencies last.

 s: This parameter isn't ever instantiated with something concrete. It is merely here
 to ensure that `compile` and `phoistAcyclic` only accept terms without any free variables.

 __Explanation of how the unembedding works:__
 Each term must be instantiated with its de-Bruijn level.
 `plam'`, given its own level, will create an `RVar` that figures out the
 de-Bruijn index needed to reach its own level given the level it itself is
 instantiated with.
-}
newtype Term (s :: k) (a :: k -> Type) = Term {asRawTerm :: Natural -> TermResult}

{- |
  *Closed* terms with no free variables.
-}
type ClosedTerm (a :: k -> Type) = forall (s :: k). Term s a

data (:-->) (a :: k -> Type) (b :: k -> Type) (s :: k)
infixr 0 :-->

data PDelayed (a :: k -> Type) (s :: k)

{- |
  Lambda abstraction.

  Only works with a single argument.
  Use 'plam' instead, to support currying.
-}
plam' :: (Term s a -> Term s b) -> Term s (a :--> b)
plam' f = Term $ \i ->
  let v = Term $ \j -> mkTermRes $ RVar (j - (i + 1))
   in case asRawTerm (f v) (i + 1) of
        -- eta-reduce for arity 1
        t@(getTerm -> RApply t'@(getArity -> Just _) [RVar 0]) -> t {getTerm = t'}
        -- eta-reduce for arity 2 + n
        t@(getTerm -> RLamAbs n (RApply t'@(getArity -> Just n') args))
          | (maybe False (== [0 .. n + 1]) $ traverse (\case RVar n -> Just n; _ -> Nothing) args)
              && n' >= n + 1 ->
            t {getTerm = t'}
        -- increment arity
        t@(getTerm -> RLamAbs n t') -> t {getTerm = RLamAbs (n + 1) t'}
        -- new lambda
        t -> mapTerm (RLamAbs 0) t
  where
    -- 0 is 1
    getArity :: RawTerm -> Maybe Natural
    -- We only do this if it's hoisted, since it's only safe if it doesn't
    -- refer to any of the variables in the wrapping lambda.
    getArity (RHoisted (HoistedTerm _ (RLamAbs n _))) = Just n
    getArity (RHoisted (HoistedTerm _ t)) = getArityBuiltin t
    getArity t = getArityBuiltin t

    getArityBuiltin :: RawTerm -> Maybe Natural
    getArityBuiltin (RBuiltin PLC.AddInteger) = Just 1
    getArityBuiltin (RBuiltin PLC.SubtractInteger) = Just 1
    getArityBuiltin (RBuiltin PLC.MultiplyInteger) = Just 1
    getArityBuiltin (RBuiltin PLC.DivideInteger) = Just 1
    getArityBuiltin (RBuiltin PLC.QuotientInteger) = Just 1
    getArityBuiltin (RBuiltin PLC.RemainderInteger) = Just 1
    getArityBuiltin (RBuiltin PLC.ModInteger) = Just 1
    getArityBuiltin (RBuiltin PLC.EqualsInteger) = Just 1
    getArityBuiltin (RBuiltin PLC.LessThanInteger) = Just 1
    getArityBuiltin (RBuiltin PLC.LessThanEqualsInteger) = Just 1
    getArityBuiltin (RBuiltin PLC.AppendByteString) = Just 1
    getArityBuiltin (RBuiltin PLC.ConsByteString) = Just 1
    getArityBuiltin (RBuiltin PLC.SliceByteString) = Just 2
    getArityBuiltin (RBuiltin PLC.LengthOfByteString) = Just 0
    getArityBuiltin (RBuiltin PLC.IndexByteString) = Just 1
    getArityBuiltin (RBuiltin PLC.EqualsByteString) = Just 1
    getArityBuiltin (RBuiltin PLC.LessThanByteString) = Just 1
    getArityBuiltin (RBuiltin PLC.LessThanEqualsByteString) = Just 1
    getArityBuiltin (RBuiltin PLC.Sha2_256) = Just 0
    getArityBuiltin (RBuiltin PLC.Sha3_256) = Just 0
    getArityBuiltin (RBuiltin PLC.Blake2b_256) = Just 0
    getArityBuiltin (RBuiltin PLC.VerifySignature) = Just 2
    getArityBuiltin (RBuiltin PLC.AppendString) = Just 1
    getArityBuiltin (RBuiltin PLC.EqualsString) = Just 1
    getArityBuiltin (RBuiltin PLC.EncodeUtf8) = Just 0
    getArityBuiltin (RBuiltin PLC.DecodeUtf8) = Just 0
    getArityBuiltin (RForce (RBuiltin PLC.IfThenElse)) = Just 2
    getArityBuiltin (RForce (RBuiltin PLC.ChooseUnit)) = Just 1
    getArityBuiltin (RForce (RBuiltin PLC.Trace)) = Just 1
    getArityBuiltin (RForce (RForce (RBuiltin PLC.FstPair))) = Just 0
    getArityBuiltin (RForce (RForce (RBuiltin PLC.SndPair))) = Just 0
    getArityBuiltin (RForce (RForce (RBuiltin PLC.ChooseList))) = Just 2
    getArityBuiltin (RForce (RBuiltin PLC.MkCons)) = Just 1
    getArityBuiltin (RForce (RBuiltin PLC.HeadList)) = Just 0
    getArityBuiltin (RForce (RBuiltin PLC.TailList)) = Just 0
    getArityBuiltin (RForce (RBuiltin PLC.NullList)) = Just 0
    getArityBuiltin (RForce (RBuiltin PLC.ChooseData)) = Just 5
    getArityBuiltin (RBuiltin PLC.ConstrData) = Just 1
    getArityBuiltin (RBuiltin PLC.MapData) = Just 0
    getArityBuiltin (RBuiltin PLC.ListData) = Just 0
    getArityBuiltin (RBuiltin PLC.IData) = Just 0
    getArityBuiltin (RBuiltin PLC.BData) = Just 0
    getArityBuiltin (RBuiltin PLC.UnConstrData) = Just 0
    getArityBuiltin (RBuiltin PLC.UnMapData) = Just 0
    getArityBuiltin (RBuiltin PLC.UnListData) = Just 0
    getArityBuiltin (RBuiltin PLC.UnIData) = Just 0
    getArityBuiltin (RBuiltin PLC.UnBData) = Just 0
    getArityBuiltin (RBuiltin PLC.EqualsData) = Just 1
    getArityBuiltin (RBuiltin PLC.MkPairData) = Just 1
    getArityBuiltin (RBuiltin PLC.MkNilData) = Just 0
    getArityBuiltin (RBuiltin PLC.MkNilPairData) = Just 0
    getArityBuiltin _ = Nothing

{- |
  Let bindings.

  This is appoximately a shorthand for a lambda and application:

  @plet v f@ == @ papp (plam f) v@

  But sufficiently small terms in WHNF may be inlined for efficiency.
-}
plet :: Term s a -> (Term s a -> Term s b) -> Term s b
plet v f = Term $ \i -> case asRawTerm v i of
  -- Inline sufficiently small terms in WHNF
  (getTerm -> RVar _) -> asRawTerm (f v) i
  (getTerm -> RBuiltin _) -> asRawTerm (f v) i
  (getTerm -> RHoisted _) -> asRawTerm (f v) i
  _ -> asRawTerm (papp (plam' f) v) i

-- | Lambda Application.
papp :: Term s (a :--> b) -> Term s a -> Term s b
papp x y = Term $ \i -> case (asRawTerm x i, asRawTerm y i) of
  -- Applying anything to an error is an error.
  (getTerm -> RError, _) -> mkTermRes RError
  -- Applying an error to anything is an error.
  (_, getTerm -> RError) -> mkTermRes RError
  -- Applying to `id` changes nothing.
  (getTerm -> RLamAbs 0 (RVar 0), y') -> y'
  (getTerm -> RHoisted (HoistedTerm _ (RLamAbs 0 (RVar 0))), y') -> y'
  -- append argument
  (x'@(getTerm -> RApply x'l x'r), y') -> TermResult (RApply x'l (getTerm y' : x'r)) (getDeps x' <> getDeps y')
  -- new RApply
  (x', y') -> TermResult (RApply (getTerm x') [getTerm y']) (getDeps x' <> getDeps y')

{- |
  Plutus \'delay\', used for laziness.
-}
pdelay :: Term s a -> Term s (PDelayed a)
pdelay x = Term $ \i -> mapTerm RDelay $ asRawTerm x i

{- |
  Plutus \'force\',
  used to force evaluation of 'PDelayed' terms.
-}
pforce :: Term s (PDelayed a) -> Term s a
pforce x = Term $ \i -> case asRawTerm x i of
  -- A force cancels a delay
  t@(getTerm -> RDelay t') -> t {getTerm = t'}
  t -> mapTerm RForce t

{- |
  Plutus \'error\'.

  When using this explicitly, it should be ensured that
  the containing term is delayed, avoiding premature evaluation.
-}
perror :: Term s a
perror = Term $ \_ -> mkTermRes RError

{- |
  Unsafely coerce the type-tag of a Term.

  This should mostly be avoided, though it can be safely
  used to assert known types of Datums, Redeemers or ScriptContext.
-}
punsafeCoerce :: Term s a -> Term s b
punsafeCoerce (Term x) = Term x

punsafeBuiltin :: UPLC.DefaultFun -> Term s a
punsafeBuiltin f = Term $ \_ -> mkTermRes $ RBuiltin f

{-# DEPRECATED punsafeConstant "Use `pconstant` instead." #-}
punsafeConstant :: Some (ValueOf PLC.DefaultUni) -> Term s a
punsafeConstant = punsafeConstantInternal

punsafeConstantInternal :: Some (ValueOf PLC.DefaultUni) -> Term s a
punsafeConstantInternal c = Term $ \_ -> mkTermRes $ RConstant c

asClosedRawTerm :: ClosedTerm a -> TermResult
asClosedRawTerm t = asRawTerm t 0

-- FIXME: Give proper error message when mutually recursive.
phoistAcyclic :: HasCallStack => ClosedTerm a -> Term s a
phoistAcyclic t = Term $ \_ -> case asRawTerm t 0 of
  -- FIXME: is this worth it?
  t'@(getTerm -> RBuiltin _) -> t'
  t' -> case evaluateScript . Script $ UPLC.Program () (PLC.defaultVersion ()) (compile' t') of
    Right _ ->
      let hoisted = HoistedTerm (hashRawTerm . getTerm $ t') (getTerm t')
       in TermResult (RHoisted hoisted) (hoisted : getDeps t')
    Left e -> error $ "Hoisted term errs! " <> show e

rawTermToUPLC :: (HoistedTerm -> Natural -> UPLC.Term DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()) -> Natural -> RawTerm -> UPLC.Term DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()
rawTermToUPLC _ _ (RVar i) = UPLC.Var () (DeBruijn . Index $ i + 1) -- Why the fuck does it start from 1 and not 0?
rawTermToUPLC m l (RLamAbs n t) = foldr (.) id (replicate (fromIntegral $ n + 1) $ UPLC.LamAbs () (DeBruijn . Index $ 0)) $ (rawTermToUPLC m (l + n + 1) t)
rawTermToUPLC m l (RApply x y) = foldr (.) id ((\y' t -> UPLC.Apply () t (rawTermToUPLC m l y')) <$> y) $ (rawTermToUPLC m l x)
rawTermToUPLC m l (RDelay t) = UPLC.Delay () (rawTermToUPLC m l t)
rawTermToUPLC m l (RForce t) = UPLC.Force () (rawTermToUPLC m l t)
rawTermToUPLC _ _ (RBuiltin f) = UPLC.Builtin () f
rawTermToUPLC _ _ (RConstant c) = UPLC.Constant () c
rawTermToUPLC _ _ RError = UPLC.Error ()
--rawTermToUPLC m l (RHoisted hoisted) = UPLC.Var () . DeBruijn . Index $ l - m hoisted
rawTermToUPLC m l (RHoisted hoisted) = m hoisted l -- UPLC.Var () . DeBruijn . Index $ l - m hoisted

-- The logic is mostly for hoisting
compile' :: TermResult -> UPLC.Term DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()
compile' t =
  let t' = getTerm t
      deps = getDeps t

      f :: Natural -> Maybe Natural -> (Bool, Maybe Natural)
      f n Nothing = (True, Just n)
      f _ (Just n) = (False, Just n)

      g :: HoistedTerm -> (M.Map Dig Natural, [(Natural, RawTerm)], Natural) -> (M.Map Dig Natural, [(Natural, RawTerm)], Natural)
      g (HoistedTerm hash term) (map, defs, n) = case M.alterF (f n) hash map of
        (True, map) -> (map, (n, term) : defs, n + 1)
        (False, map) -> (map, defs, n)

      toInline :: S.Set Dig
      toInline = S.fromList . fmap (\(HoistedTerm hash _) -> hash) . (head <$>) . filter ((== 1) . length) . groupBy (\(HoistedTerm x _) (HoistedTerm y _) -> x == y) . sortOn (\(HoistedTerm hash _) -> hash) $ deps

      -- map: term -> de Bruijn level
      -- defs: the terms, level 0 is last
      -- n: # of terms
      (map, defs, n) = foldr g (M.empty, [], 0) $ filter (\(HoistedTerm hash _) -> not $ S.member hash toInline) deps

      map' (HoistedTerm hash term) l = case M.lookup hash map of
        Just l' -> UPLC.Var () . DeBruijn . Index $ l - l'
        Nothing -> rawTermToUPLC map' l term

      body = rawTermToUPLC map' n t'

      wrapped = foldl' (\b (lvl, def) -> UPLC.Apply () (UPLC.LamAbs () (DeBruijn . Index $ 0) b) (rawTermToUPLC map' lvl def)) body defs
   in wrapped

-- | Compile a (closed) Plutus Term to a usable script
compile :: ClosedTerm a -> Script
compile t = Script $ UPLC.Program () (PLC.defaultVersion ()) (compile' $ asClosedRawTerm $ t)

newtype TermCont s a = TermCont {runTermCont :: forall b. (a -> Term s b) -> Term s b}

instance Functor (TermCont s) where
  fmap f (TermCont g) = TermCont $ \h -> g (h . f)

instance Applicative (TermCont s) where
  pure x = TermCont $ \f -> f x
  x <*> y = do
    x <- x
    y <- y
    pure (x y)

instance Monad (TermCont s) where
  (TermCont f) >>= g = TermCont $ \h ->
    f
      ( \x ->
          runTermCont (g x) h
      )

hashTerm :: ClosedTerm a -> Dig
hashTerm t =
  let t' = asRawTerm t 0
   in hashRawTerm . getTerm $ t'

hashOpenTerm :: Term s a -> TermCont s Dig
hashOpenTerm x = TermCont $ \f -> Term $ \i ->
  let inner = f $ hashRawTerm . getTerm $ asRawTerm x i
   in asRawTerm inner i
