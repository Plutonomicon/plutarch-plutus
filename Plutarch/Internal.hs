{-# LANGUAGE RoleAnnotations #-}
-- FIXME remove
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}

module Plutarch.Internal (
  -- | $hoisted
  (:-->),
  PDelayed,
  -- | $term
  Term (Term, asRawTerm),
  asClosedRawTerm,
  mapTerm,
  plam',
  plet,
  papp,
  pdelay,
  pforce,
  phoistAcyclic,
  punsafeAsClosedTerm,
  perror,
  punsafeCoerce,
  punsafeBuiltin,
  punsafeConstantInternal,
  compile,
  compile',
  ClosedTerm,
  Dig,
  hashTerm,
  hashRawTerm,
  RawTerm (..),
  TermResult (TermResult, getDeps, getTerm),
  S (SI),
  PType,
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
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import GHC.Word (Word64)
import Plutarch.Evaluate (evalScript)
import Plutus.V1.Ledger.Scripts (Script (Script))
import PlutusCore (Some (Some), ValueOf (ValueOf))
import qualified PlutusCore as PLC
import PlutusCore.DeBruijn (DeBruijn (DeBruijn), Index (Index))
import qualified UntypedPlutusCore as UPLC
import Data.Text (Text)
import Data.String (fromString)
import Data.Coerce (Coercible)
import Data.Functor.Const (Const(Const))

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
  = RVar Word64
  | RLamAbs Word64 RawTerm
  | RApply RawTerm [RawTerm]
  | RForce RawTerm
  | RDelay RawTerm
  | RConstant (Some (ValueOf PLC.DefaultUni))
  | RBuiltin PLC.DefaultFun
  | RCompiled (UPLC.Term DeBruijn UPLC.DefaultUni UPLC.DefaultFun ())
  | RError
  | RHoisted HoistedTerm
  deriving stock (Show)

hashRawTerm' :: HashAlgorithm alg => RawTerm -> Context alg -> Context alg
hashRawTerm' (RVar x) = flip hashUpdate ("0" :: BS.ByteString) . flip hashUpdate (F.flat (fromIntegral x :: Integer))
hashRawTerm' (RLamAbs n x) =
  flip hashUpdate ("1" :: BS.ByteString) . flip hashUpdate (F.flat (fromIntegral n :: Integer)) . hashRawTerm' x
hashRawTerm' (RApply x y) =
  flip hashUpdate ("2" :: BS.ByteString) . hashRawTerm' x . flip (foldl' $ flip hashRawTerm') y
hashRawTerm' (RForce x) = flip hashUpdate ("3" :: BS.ByteString) . hashRawTerm' x
hashRawTerm' (RDelay x) = flip hashUpdate ("4" :: BS.ByteString) . hashRawTerm' x
hashRawTerm' (RConstant x) = flip hashUpdate ("5" :: BS.ByteString) . flip hashUpdate (F.flat x)
hashRawTerm' (RBuiltin x) = flip hashUpdate ("6" :: BS.ByteString) . flip hashUpdate (F.flat x)
hashRawTerm' RError = flip hashUpdate ("7" :: BS.ByteString)
hashRawTerm' (RHoisted (HoistedTerm hash _)) = flip hashUpdate ("8" :: BS.ByteString) . flip hashUpdate hash
hashRawTerm' (RCompiled code) = flip hashUpdate ("9" :: BS.ByteString) . flip hashUpdate (F.flat code)

hashRawTerm :: RawTerm -> Dig
hashRawTerm t = hashFinalize . hashRawTerm' t $ hashInit

data TermResult (a :: PType) = TermResult
  { getTerm :: RawTerm
  , getDeps :: [HoistedTerm]
  , getAssoc :: PAssociated a
  }

mapTerm :: (RawTerm -> RawTerm) -> TermResult a -> TermResult a
mapTerm f (TermResult t d assoc) = TermResult (f t) d assoc

mkTermRes :: PAssociated a -> RawTerm -> TermResult a
mkTermRes assoc r = TermResult r [] assoc

{- Type of `s` in `Term s a`. See: "What is the `s`?" section on the Plutarch guide.

`SI` is the identity type of kind `S`. It is used in type class/family instances
to "forget" the `s`.
-}
data S = SI

newtype PType' = MkPType' PType

type PTypeF = PType' -> Type

type MkPType' :: PType -> PType'
type MkPType' = 'MkPType'

type family UnPType' (u :: PType') :: PType where
  UnPType' (MkPType' x) = x
--
-- | Plutarch type.
type PType = PTypeF -> Type

type role Term phantom nominal

type TermCtx = Word64 -- level

type TEM a = Either Text a

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
newtype Term (s :: S) (a :: PType) = Term {asRawTerm :: TermCtx -> TEM (TermResult a)}

pextract :: PlutusType a => Term s a -> (PAssociated a -> Term s b) -> Term s b
pextract (Term t) f = Term $ \i -> do
  t' <- t i
  asRawTerm (f (getAssoc t')) i

{- |
  *Closed* terms with no free variables.
-}
type ClosedTerm (a :: PType) = forall (s :: S). Term s a

data (:-->) (a :: PType) (b :: PType) (f :: PTypeF)
infixr 0 :-->

data PDelayed (a :: PType) (f :: PTypeF) = PDelayed (Pf f (PDelayed a)) (PAssociated a)
instance PlutusType (PDelayed a) where
  type PInner (PDelayed a) _ = PDelayed a
  type PAssociated (PDelayed a) = PAssociated a
  pcon' (PDelayed t assoc) = (t, assoc)
  pmatch' (PDelayed t assoc, assoc') f = punsafeCoerce (f t) assoc'

{- |
  Lambda abstraction.

  Only works with a single argument.
  Use 'plam' instead, to support currying.
-}
plam' :: forall s a b. (Term s a -> Term s b) -> Term s (a :--> b)
plam' f = Term $ \i ->
    let v = Term $ \j -> pure $ mkTermRes undefined $ RVar (j - (i + 1))
     in asRawTerm (f v) (i + 1) >>= \case
          -- eta-reduce for arity 1
          t@(getTerm -> RApply t'@(getArity -> Just _) [RVar 0]) -> pure t {getTerm = t', getAssoc = undefined}
          -- eta-reduce for arity 2 + n
          t@(getTerm -> RLamAbs n (RApply t'@(getArity -> Just n') args))
            | (maybe False (== [0 .. n + 1]) $ traverse (\case RVar n -> Just n; _ -> Nothing) args)
                && n' >= n + 1 ->
                pure t {getTerm = t', getAssoc = undefined}
          -- increment arity
          t@(getTerm -> RLamAbs n t') -> pure t {getTerm = RLamAbs (n + 1) t', getAssoc = undefined}
          -- new lambda
          t -> pure $ mapTerm (RLamAbs 0) t { getAssoc = undefined }
  where
    -- 0 is 1
    getArity :: RawTerm -> Maybe Word64
    -- We only do this if it's hoisted, since it's only safe if it doesn't
    -- refer to any of the variables in the wrapping lambda.
    getArity (RHoisted (HoistedTerm _ (RLamAbs n _))) = Just n
    getArity (RHoisted (HoistedTerm _ t)) = getArityBuiltin t
    getArity t = getArityBuiltin t

    getArityBuiltin :: RawTerm -> Maybe Word64
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
plet :: forall s a b. Term s a -> (Term s a -> Term s b) -> Term s b
plet v f = Term $ \i -> do
  v' <- asRawTerm v i
  case v' of
    -- Inline sufficiently small terms in WHNF
    (getTerm -> RVar _) -> asRawTerm (f v) i
    (getTerm -> RBuiltin _) -> asRawTerm (f v) i
    (getTerm -> RHoisted _) -> asRawTerm (f v) i
    _ -> asRawTerm (papp (plam' f) v) i

-- | Lambda Application.
papp :: forall s a b. Term s (a :--> b) -> Term s a -> Term s b
papp x y = Term $ \i -> do
  x' <- asRawTerm x i
  y' <- asRawTerm y i
  pure $ case (x', y') of
    -- Applying anything to an error is an error.
    (getTerm -> RError, _) -> mkTermRes undefined RError
    -- Applying an error to anything is an error.
    (_, getTerm -> RError) -> mkTermRes undefined RError
    -- Applying to `id` changes nothing.
    (getTerm -> RLamAbs 0 (RVar 0), y') -> y' { getAssoc = undefined }
    (getTerm -> RHoisted (HoistedTerm _ (RLamAbs 0 (RVar 0))), y') -> y' { getAssoc = undefined }
    -- append argument
    (x'@(getTerm -> RApply x'l x'r), y') -> TermResult (RApply x'l (getTerm y' : x'r)) (getDeps x' <> getDeps y') undefined
    -- new RApply
    (x', y') -> TermResult (RApply (getTerm x') [getTerm y']) (getDeps x' <> getDeps y') undefined

{- |
  Plutus \'delay\', used for laziness.
-}
pdelay :: Term s a -> Term s (PDelayed a)
pdelay x = Term $ \i -> do
  x' <- asRawTerm x i
  pure $ (mapTerm RDelay x') { getAssoc = undefined }

{- |
  Plutus \'force\',
  used to force evaluation of 'PDelayed' terms.
-}
pforce :: Term s (PDelayed a) -> Term s a
pforce x = Term $ \i -> asRawTerm x i >>= \case
    -- A force cancels a delay
    t@(getTerm -> RDelay t') -> pure $ t {getTerm = t', getAssoc = undefined}
    t -> pure $ (mapTerm RForce t) { getAssoc = undefined }

{- |
  Plutus \'error\'.

  When using this explicitly, it should be ensured that
  the containing term is delayed, avoiding premature evaluation.
-}
perror :: PlutusType a => PAssociated a -> Term s a
perror assoc = Term $ \_ -> pure $ mkTermRes assoc RError

pfail' :: HasCallStack => Text -> TEM a
pfail' msg = Left $ msg <> "\n\n" <> fromString (prettyCallStack callStack)

pfail :: HasCallStack => Text -> Term s a
pfail msg = Term $ \_ -> Left $ msg <> "\n\n" <> fromString (prettyCallStack callStack)

{- |
  Unsafely coerce the type-tag of a Term.

  This should mostly be avoided, though it can be safely
  used to assert known types of Datums, Redeemers or ScriptContext.
-}
punsafeCoerce :: PlutusType b => Term s a -> PAssociated b -> Term s b
punsafeCoerce (Term t) assoc = Term $ \i -> (\x -> x { getAssoc = assoc }) <$> (t i)

punsafeBuiltin :: PlutusType a => UPLC.DefaultFun -> PAssociated a -> Term s a
punsafeBuiltin f assoc = Term $ \_ -> pure . mkTermRes assoc $ RBuiltin f

punsafeConstantInternal :: PlutusType a => Some (ValueOf PLC.DefaultUni) -> PAssociated a -> Term s a
punsafeConstantInternal c assoc = Term $ \_ ->
  pure $ case c of
    -- These constants are smaller than variable references.
    Some (ValueOf PLC.DefaultUniBool _) -> mkTermRes assoc $ RConstant c
    Some (ValueOf PLC.DefaultUniUnit _) -> mkTermRes assoc $ RConstant c
    Some (ValueOf PLC.DefaultUniInteger n) | n < 256 -> mkTermRes assoc $ RConstant c
    _ ->
      let hoisted = HoistedTerm (hashRawTerm $ RConstant c) (RConstant c)
       in TermResult (RHoisted hoisted) [hoisted] assoc

asClosedRawTerm :: ClosedTerm a -> TEM (TermResult a)
asClosedRawTerm t = asRawTerm t 0

-- FIXME: Give proper error message when mutually recursive.
phoistAcyclic :: HasCallStack => ClosedTerm a -> Term s a
phoistAcyclic t = Term $ \_ -> asClosedRawTerm t >>= \case
  -- Built-ins are smaller than variable references
  t'@(getTerm -> RBuiltin _) -> pure t'
  t' -> case evalScript . Script . UPLC.Program () (PLC.defaultVersion ()) $ compile' t' of
    (Right _, _, _) ->
      let hoisted = HoistedTerm (hashRawTerm . getTerm $ t') (getTerm t')
       in pure $ TermResult (RHoisted hoisted) (hoisted : getDeps t') (getAssoc t')
    (Left e, _, _) -> pfail' $ "Hoisted term errs! " <> fromString (show e)

punsafeAsClosedTerm :: Term s a -> ClosedTerm a
punsafeAsClosedTerm (Term t) = (Term t)

-- Couldn't find a definition for this in plutus-core
subst :: Word64 -> (Word64 -> UPLC.Term DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()) -> UPLC.Term DeBruijn UPLC.DefaultUni UPLC.DefaultFun () -> UPLC.Term DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()
subst idx x (UPLC.Apply () yx yy) = UPLC.Apply () (subst idx x yx) (subst idx x yy)
subst idx x (UPLC.LamAbs () name y) = UPLC.LamAbs () name (subst (idx + 1) x y)
subst idx x (UPLC.Delay () y) = UPLC.Delay () (subst idx x y)
subst idx x (UPLC.Force () y) = UPLC.Force () (subst idx x y)
subst idx x (UPLC.Var () (DeBruijn (Index idx'))) | idx == idx' = x idx
subst idx _ y@(UPLC.Var () (DeBruijn (Index idx'))) | idx > idx' = y
subst idx _ (UPLC.Var () (DeBruijn (Index idx'))) | idx < idx' = UPLC.Var () (DeBruijn . Index $ idx' - 1)
subst _ _ y = y

rawTermToUPLC ::
  (HoistedTerm -> Word64 -> UPLC.Term DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()) ->
  Word64 ->
  RawTerm ->
  UPLC.Term DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()
rawTermToUPLC _ _ (RVar i) = UPLC.Var () (DeBruijn . Index $ i + 1) -- Why the fuck does it start from 1 and not 0?
rawTermToUPLC m l (RLamAbs n t) =
  foldr
    (.)
    id
    (replicate (fromIntegral $ n + 1) $ UPLC.LamAbs () (DeBruijn . Index $ 0))
    $ (rawTermToUPLC m (l + n + 1) t)
rawTermToUPLC m l (RApply x y) =
  let f y t@(UPLC.LamAbs () _ body) =
        case rawTermToUPLC m l y of
          -- Inline unconditionally if it's a variable or built-in.
          -- These terms are very small and are always WHNF.
          UPLC.Var () (DeBruijn (Index idx)) -> subst 1 (\lvl -> UPLC.Var () (DeBruijn . Index $ idx + lvl - 1)) body
          arg@UPLC.Builtin {} -> subst 1 (\_ -> arg) body
          arg -> UPLC.Apply () t arg
      f y t = UPLC.Apply () t (rawTermToUPLC m l y)
   in foldr (.) id (f <$> y) $ (rawTermToUPLC m l x)
rawTermToUPLC m l (RDelay t) = UPLC.Delay () (rawTermToUPLC m l t)
rawTermToUPLC m l (RForce t) = UPLC.Force () (rawTermToUPLC m l t)
rawTermToUPLC _ _ (RBuiltin f) = UPLC.Builtin () f
rawTermToUPLC _ _ (RConstant c) = UPLC.Constant () c
rawTermToUPLC _ _ (RCompiled code) = code
rawTermToUPLC _ _ RError = UPLC.Error ()
-- rawTermToUPLC m l (RHoisted hoisted) = UPLC.Var () . DeBruijn . Index $ l - m hoisted
rawTermToUPLC m l (RHoisted hoisted) = m hoisted l -- UPLC.Var () . DeBruijn . Index $ l - m hoisted

-- The logic is mostly for hoisting
compile' :: TermResult a -> UPLC.Term DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()
compile' t =
  let t' = getTerm t
      deps = getDeps t

      f :: Word64 -> Maybe Word64 -> (Bool, Maybe Word64)
      f n Nothing = (True, Just n)
      f _ (Just n) = (False, Just n)

      g ::
        HoistedTerm ->
        (M.Map Dig Word64, [(Word64, RawTerm)], Word64) ->
        (M.Map Dig Word64, [(Word64, RawTerm)], Word64)
      g (HoistedTerm hash term) (map, defs, n) = case M.alterF (f n) hash map of
        (True, map) -> (map, (n, term) : defs, n + 1)
        (False, map) -> (map, defs, n)

      toInline :: S.Set Dig
      toInline =
        S.fromList
          . fmap (\(HoistedTerm hash _) -> hash)
          . (head <$>)
          . filter ((== 1) . length)
          . groupBy (\(HoistedTerm x _) (HoistedTerm y _) -> x == y)
          . sortOn (\(HoistedTerm hash _) -> hash)
          $ deps

      -- map: term -> de Bruijn level
      -- defs: the terms, level 0 is last
      -- n: # of terms
      (map, defs, n) = foldr g (M.empty, [], 0) $ filter (\(HoistedTerm hash _) -> not $ S.member hash toInline) deps

      map' (HoistedTerm hash term) l = case M.lookup hash map of
        Just l' -> UPLC.Var () . DeBruijn . Index $ l - l'
        Nothing -> rawTermToUPLC map' l term

      body = rawTermToUPLC map' n t'

      wrapped =
        foldl'
          (\b (lvl, def) -> UPLC.Apply () (UPLC.LamAbs () (DeBruijn . Index $ 0) b) (rawTermToUPLC map' lvl def))
          body
          defs
   in wrapped

-- | Compile a (closed) Plutus Term to a usable script
compile :: ClosedTerm a -> TEM Script
compile t = do
  t' <- asClosedRawTerm t
  pure $ Script $ UPLC.Program () (PLC.defaultVersion ()) (compile' t')

hashTerm :: ClosedTerm a -> TEM Dig
hashTerm t = do
  t' <- asClosedRawTerm t
  pure . hashRawTerm . getTerm $ t'

newtype PAsData (a :: PType) (f :: PTypeF) = MkPAsData (f (MkPType' (PAsData a)))
newtype PAsData' (a :: PType) (f :: PTypeF) = MkPAsData' (a (PCompose PAsData' f))

-- This should have been a type family but those can't be partially applied yet...
type PCompose :: (PType -> PType) -> PTypeF -> PTypeF
newtype PCompose (g :: PType -> PType) (f :: PTypeF) (x :: PType') = PCompose (f (MkPType' (g (UnPType' x))))

type ToPTypeF :: (PType -> Type) -> PTypeF
newtype ToPTypeF (f :: PType -> Type) (x :: PType') = ToPTypeF (f (UnPType' x))

class (Coercible (Reduce x) x) => Reducible (x :: Type) where
  type Reduce x :: Type

instance Reducible () where
  type Reduce () = ()

instance Reducible (Term s a) where
  type Reduce (Term s a) = Term s a

instance Reducible (f (UnPType' x)) => Reducible (ToPTypeF f x) where
  type Reduce (ToPTypeF f x) = Reduce (f (UnPType' x))

instance Reducible (f (MkPType' (g (UnPType' x)))) => Reducible (PCompose g f x) where
  type Reduce (PCompose g f x) = Reduce (f (MkPType' (g (UnPType' x))))

instance Reducible x => Reducible (Const x y) where
  type Reduce (Const x y) = Reduce x

type Pf (f :: PTypeF) (x :: PType) = Reduce (f (MkPType' x))

data PPair (a :: PType) (b :: PType) (f :: PTypeF) = MkPPair (Pf f a) (Pf f b)

--instance Reducible (PCompose )

class PlutusType (a :: PType) where
  type PInner a (b' :: PType) :: PType -- the kind of `b` should really be free, but Haskell isn't expressive enough
  type PAssociated a :: Type
  pcon' :: forall s b. a (ToPTypeF (Term s)) -> (Term s (PInner a b), PAssociated a)
  pmatch' :: forall s b. (Term s (PInner a b), PAssociated a) -> (a (ToPTypeF (Term s)) -> Term s b) -> Term s b

type PInteger :: PType
data PInteger f

data PMoney (f :: PTypeF) = PMoney
  { amount :: Pf f PInteger
  , moneyclass :: String
  }

instance PlutusType PMoney where
  type PInner PMoney _ = PInteger
  type PAssociated PMoney = PMoney (Const ())
  pcon' (PMoney a m) = (a, PMoney () m)
  pmatch' (a, PMoney () m) f = f (PMoney a m)

pcon :: PlutusType a => a (ToPTypeF (Term s)) -> Term s a
pcon x = uncurry punsafeCoerce (pcon' x)

--pmatch :: PlutusType a => Term s a -> (a s -> Term s b) -> Term s b
--pmatch x f = pmatch' (punsafeCoerce x) f
