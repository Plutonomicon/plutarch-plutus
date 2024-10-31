{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ViewPatterns #-}

module Plutarch.Internal (
  -- | \$hoisted
  (:-->) (PLam),
  PDelayed,
  -- | \$term
  Term (..),
  asClosedRawTerm,
  Script (Script),
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
  compileOptimized,
  compile',
  ClosedTerm,
  Dig,
  hashTerm,
  hashRawTerm,
  RawTerm (..),
  HoistedTerm (..),
  TermResult (TermResult, getDeps, getTerm),
  S (SI),
  PType,
  pthrow,
  Config (NoTracing, Tracing),
  TracingMode (..),
  LogLevel (..),
  tracingMode,
  logLevel,
  pgetConfig,
  TermMonad (..),
  (#),
  (#$),
) where

import Control.Monad.Reader (ReaderT (ReaderT), ask, runReaderT)
import Control.Monad.State.Strict (evalStateT)
import Crypto.Hash (Context, Digest, hashFinalize, hashInit, hashUpdate)
import Crypto.Hash.Algorithms (Blake2b_160)
import Crypto.Hash.IO (HashAlgorithm)
import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toEncoding, toJSON),
  object,
  pairs,
  withObject,
  withText,
  (.:),
  (.=),
 )
import Data.ByteString qualified as BS
import Data.Default (def)
import Data.Kind (Type)
import Data.List (foldl', groupBy, sortOn)
import Data.Map.Lazy qualified as M
import Data.Monoid (Last (Last))
import Data.Set qualified as S
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as V
import Flat.Run qualified as F
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import GHC.Word (Word64)
import Plutarch.Internal.Evaluate (evalScript, uplcVersion)
import Plutarch.Script (Script (Script))
import PlutusCore (Some (Some), ValueOf (ValueOf))
import PlutusCore qualified as PLC
import PlutusCore.Compiler.Types (initUPLCSimplifierTrace)
import PlutusCore.DeBruijn (DeBruijn (DeBruijn), Index (Index))
import Prettyprinter (Pretty (pretty), (<+>))
import UntypedPlutusCore qualified as UPLC

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

type UTerm = UPLC.Term UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()

data RawTerm
  = RVar Word64
  | RLamAbs Word64 RawTerm
  | RApply RawTerm [RawTerm]
  | RForce RawTerm
  | RDelay RawTerm
  | RConstant (Some (ValueOf PLC.DefaultUni))
  | RBuiltin PLC.DefaultFun
  | RCompiled UTerm
  | RError
  | RHoisted HoistedTerm
  | RPlaceHolder Dig
  | RConstr Word64 [RawTerm]
  | RCase RawTerm [RawTerm]
  deriving stock (Show)

addHashIndex :: forall alg. HashAlgorithm alg => Integer -> Context alg -> Context alg
addHashIndex i = flip hashUpdate ((fromString $ show i) :: BS.ByteString)

hashUTerm :: forall alg. HashAlgorithm alg => UTerm -> Context alg -> Context alg
hashUTerm (UPLC.Var _ name) = addHashIndex 0 . flip hashUpdate (F.flat name)
hashUTerm (UPLC.LamAbs _ name uterm) = addHashIndex 1 . flip hashUpdate (F.flat name) . hashUTerm uterm
hashUTerm (UPLC.Apply _ uterm1 uterm2) = addHashIndex 2 . hashUTerm uterm1 . hashUTerm uterm2
hashUTerm (UPLC.Force _ uterm) = addHashIndex 3 . hashUTerm uterm
hashUTerm (UPLC.Delay _ uterm) = addHashIndex 4 . hashUTerm uterm
hashUTerm (UPLC.Constant _ val) = addHashIndex 5 . flip hashUpdate (F.flat val)
hashUTerm (UPLC.Builtin _ fun) = addHashIndex 6 . flip hashUpdate (F.flat fun)
hashUTerm (UPLC.Error _) = addHashIndex 7
hashUTerm (UPLC.Constr _ idx uterms) =
  addHashIndex 8 . addHashIndex (fromIntegral idx) . foldl1 (.) (hashUTerm <$> uterms)
hashUTerm (UPLC.Case _ uterm uterms) = addHashIndex 9 . hashUTerm uterm . foldl1 (.) (hashUTerm <$> uterms)

hashRawTerm' :: forall alg. HashAlgorithm alg => RawTerm -> Context alg -> Context alg
hashRawTerm' (RVar x) = addHashIndex 0 . flip hashUpdate (F.flat (fromIntegral x :: Integer))
hashRawTerm' (RLamAbs n x) =
  addHashIndex 1 . flip hashUpdate (F.flat (fromIntegral n :: Integer)) . hashRawTerm' x
hashRawTerm' (RApply x y) =
  addHashIndex 2 . hashRawTerm' x . flip (foldl' $ flip hashRawTerm') y
hashRawTerm' (RForce x) = addHashIndex 3 . hashRawTerm' x
hashRawTerm' (RDelay x) = addHashIndex 4 . hashRawTerm' x
hashRawTerm' (RConstant x) = addHashIndex 5 . flip hashUpdate (F.flat x)
hashRawTerm' (RBuiltin x) = addHashIndex 6 . flip hashUpdate (F.flat x)
hashRawTerm' RError = addHashIndex 7
hashRawTerm' (RHoisted (HoistedTerm hash _)) = addHashIndex 8 . flip hashUpdate hash
hashRawTerm' (RCompiled code) = addHashIndex 9 . flip hashUpdate (hashUTerm @alg code hashInit)
hashRawTerm' (RPlaceHolder hash) = addHashIndex 10 . flip hashUpdate hash
hashRawTerm' (RConstr x y) =
  addHashIndex 11 . flip hashUpdate (F.flat (fromIntegral x :: Integer)) . flip (foldl' $ flip hashRawTerm') y
hashRawTerm' (RCase x y) =
  addHashIndex 12 . hashRawTerm' x . flip (foldl' $ flip hashRawTerm') y

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

{- Type of `s` in `Term s a`. See: "What is the `s`?" section on the Plutarch guide.

`SI` is the identity type of kind `S`. It is used in type class/family instances
to "forget" the `s`.
-}
data S = SI

-- | Shorthand for Plutarch types.
type PType = S -> Type

{- | How to trace.

@since 1.6.0
-}
data TracingMode = DetTracing | DoTracing | DoTracingAndBinds
  deriving stock
    ( -- | @since 1.6.0
      Eq
    , -- | @since 1.6.0
      Show
    )

{- | We have a linear order of generality, so this instance reflects it:
\'smaller\' values are more specific. Generality is in the following order,
from least to most general:

1. @DetTracing@
2. @DoTracing@
3. @DoTracingAndBinds@

@since 1.6.0
-}
instance Ord TracingMode where
  -- Note: We write this by hand so someone re-ordering or adding 'arms' won't
  -- silently break this.
  tm1 <= tm2 = case tm1 of
    DetTracing -> True
    DoTracing -> case tm2 of
      DetTracing -> False
      _ -> True
    DoTracingAndBinds -> case tm2 of
      DoTracingAndBinds -> True
      _ -> False

{- | More general tracing supersedes less general.

@since 1.6.0
-}
instance Semigroup TracingMode where
  (<>) = max

-- | @since 1.6.0
instance Pretty TracingMode where
  pretty = \case
    DetTracing -> "DetTracing"
    DoTracing -> "DoTracing"
    DoTracingAndBinds -> "DoTracingAndBinds"

-- | @since 1.6.0
instance ToJSON TracingMode where
  {-# INLINEABLE toJSON #-}
  toJSON =
    toJSON @Text . \case
      DetTracing -> "DetTracing"
      DoTracing -> "DoTracing"
      DoTracingAndBinds -> "DoTracingAndBinds"
  {-# INLINEABLE toEncoding #-}
  toEncoding =
    toEncoding @Text . \case
      DetTracing -> "DetTracing"
      DoTracing -> "DoTracing"
      DoTracingAndBinds -> "DoTracingAndBinds"

-- | @since 1.6.0
instance FromJSON TracingMode where
  {-# INLINEABLE parseJSON #-}
  parseJSON = withText "TracingMode" $ \case
    "DetTracing" -> pure DetTracing
    "DoTracing" -> pure DoTracing
    "DoTracingAndBinds" -> pure DoTracingAndBinds
    x -> fail $ "Not a valid encoding: " <> Text.unpack x

{- | What logging level we want to use.

@since 1.6.0
-}
data LogLevel = LogInfo | LogDebug
  deriving stock
    ( -- | @since 1.6.0
      Eq
    , -- | @since 1.6.0
      Show
    )

{- | We have a linear order of generality, so this instance reflects it:
@LogDebug@ is more general than @LogInfo@.

@since 1.6.0
-}
instance Ord LogLevel where
  -- Note: We write this by hand so someone re-ordering or adding 'arms' won't
  -- silently break this.
  ll1 <= ll2 = case ll1 of
    LogInfo -> True
    LogDebug -> case ll2 of
      LogDebug -> True
      _ -> False

{- | More general logging supersedes less general.

@since 1.6.0
-}
instance Semigroup LogLevel where
  (<>) = max

-- | @since 1.6.0
instance Pretty LogLevel where
  pretty = \case
    LogInfo -> "LogInfo"
    LogDebug -> "LogDebug"

-- | @since 1.6.0
instance ToJSON LogLevel where
  {-# INLINEABLE toJSON #-}
  toJSON =
    toJSON @Text . \case
      LogInfo -> "LogInfo"
      LogDebug -> "LogDebug"
  {-# INLINEABLE toEncoding #-}
  toEncoding =
    toEncoding @Text . \case
      LogInfo -> "LogInfo"
      LogDebug -> "LogDebug"

-- | @since 1.6.0
instance FromJSON LogLevel where
  {-# INLINEABLE parseJSON #-}
  parseJSON = withText "LogLevel" $ \case
    "LogInfo" -> pure LogInfo
    "LogDebug" -> pure LogDebug
    x -> fail $ "Not a valid encoding: " <> Text.unpack x

{- | Configuration for Plutarch scripts at compile time. This indicates whether
we want to trace, and if so, under what log level and mode.

@since 1.6.0
-}
newtype Config = Config (Last (LogLevel, TracingMode))
  deriving
    ( -- | @since 1.6.0
      Semigroup
    , -- | @since 1.6.0
      Monoid
    )
    via (Last (LogLevel, TracingMode))
  deriving stock
    ( -- | @since 1.6.0
      Eq
    , -- | @since 1.6.0
      Show
    )

-- | @since 1.6.0
instance Pretty Config where
  pretty (Config (Last x)) = case x of
    Nothing -> "NoTracing"
    Just (ll, tm) -> "Tracing " <+> pretty ll <+> pretty tm

-- | @since 1.6.0
instance ToJSON Config where
  -- We serialize Config as if it were a sum type for consistency. We also label
  -- its fields (when present).
  {-# INLINEABLE toJSON #-}
  toJSON =
    object . \case
      NoTracing -> ["tag" .= (0 :: Int)]
      Tracing ll tm ->
        [ "tag" .= (1 :: Int)
        , "logLevel" .= ll
        , "tracingMode" .= tm
        ]
  {-# INLINEABLE toEncoding #-}
  toEncoding =
    pairs . \case
      NoTracing -> "tag" .= (0 :: Int)
      Tracing ll tm ->
        ("tag" .= (1 :: Int))
          <> ("logLevel" .= ll)
          <> ("tracingMode" .= tm)

-- | @since 1.6.0
instance FromJSON Config where
  parseJSON = withObject "Config" $ \v ->
    v .: "tag" >>= \(tag :: Int) -> case tag of
      0 -> return NoTracing
      1 -> Tracing <$> v .: "logLevel" <*> v .: "tracingMode"
      _ -> fail "Invalid tag"

{- | If the config indicates that we want to trace, get its mode.

@since 1.6.0
-}
tracingMode :: Config -> Maybe TracingMode
tracingMode (Config (Last x)) = snd <$> x

{- | If the config indicates that we want to trace, get its log level.

@since 1.6.0
-}
logLevel :: Config -> Maybe LogLevel
logLevel (Config (Last x)) = fst <$> x

{- | Pattern for the config that does no tracing (also the default).

@since 1.6.0
-}
pattern NoTracing :: Config
pattern NoTracing <- Config (Last Nothing)
  where
    NoTracing = Config (Last Nothing)

{- | Pattern for a tracing config, with both its log level and mode.

@since 1.6.0
-}
pattern Tracing :: LogLevel -> TracingMode -> Config
pattern Tracing ll tm <- Config (Last (Just (ll, tm)))
  where
    Tracing ll tm = Config (Last (Just (ll, tm)))

{-# COMPLETE NoTracing, Tracing #-}

newtype TermMonad m = TermMonad {runTermMonad :: ReaderT Config (Either Text) m}
  deriving newtype (Functor, Applicative, Monad)

type role Term nominal nominal

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
newtype Term (s :: S) (a :: PType) = Term {asRawTerm :: Word64 -> TermMonad TermResult}

{- |
  *Closed* terms with no free variables.
-}
type ClosedTerm (a :: PType) = forall (s :: S). Term s a

newtype (:-->) (a :: PType) (b :: PType) (s :: S)
  = PLam (Term s a -> Term s b)
infixr 0 :-->

data PDelayed (a :: PType) (s :: S)

{- |
  Lambda abstraction.

  Only works with a single argument.
  Use 'plam' instead, to support currying.
-}
plam' :: (Term s a -> Term s b) -> Term s (a :--> b)
plam' f = Term \i ->
  let v = Term \j -> pure $ mkTermRes $ RVar (j - (i + 1))
   in flip fmap (asRawTerm (f v) (i + 1)) \case
        -- eta-reduce for arity 1
        t@(getTerm -> RApply t'@(getArity -> Just _) [RVar 0]) -> t {getTerm = t'}
        -- eta-reduce for arity 2 + n
        t@(getTerm -> RLamAbs n (RApply t'@(getArity -> Just n') args))
          | (== Just [0 .. n + 1]) (traverse (\case RVar n -> Just n; _ -> Nothing) args)
              && n' >= n + 1 ->
              t {getTerm = t'}
        -- increment arity
        t@(getTerm -> RLamAbs n t') -> t {getTerm = RLamAbs (n + 1) t'}
        -- new lambda
        t -> mapTerm (RLamAbs 0) t
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
    getArityBuiltin (RBuiltin PLC.ExpModInteger) = Just 2
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
    getArityBuiltin (RBuiltin PLC.IntegerToByteString) = Just 2
    getArityBuiltin (RBuiltin PLC.ByteStringToInteger) = Just 1
    getArityBuiltin (RBuiltin PLC.AndByteString) = Just 2
    getArityBuiltin (RBuiltin PLC.OrByteString) = Just 2
    getArityBuiltin (RBuiltin PLC.XorByteString) = Just 2
    getArityBuiltin (RBuiltin PLC.ComplementByteString) = Just 0
    getArityBuiltin (RBuiltin PLC.ReadBit) = Just 1
    getArityBuiltin (RBuiltin PLC.WriteBits) = Just 1
    getArityBuiltin (RBuiltin PLC.ReplicateByte) = Just 1
    getArityBuiltin (RBuiltin PLC.ShiftByteString) = Just 1
    getArityBuiltin (RBuiltin PLC.RotateByteString) = Just 1
    getArityBuiltin (RBuiltin PLC.CountSetBits) = Just 0
    getArityBuiltin (RBuiltin PLC.FindFirstSetBit) = Just 0
    getArityBuiltin (RBuiltin PLC.Bls12_381_G1_add) = Just 1
    getArityBuiltin (RBuiltin PLC.Bls12_381_G1_neg) = Just 0
    getArityBuiltin (RBuiltin PLC.Bls12_381_G1_scalarMul) = Just 1
    getArityBuiltin (RBuiltin PLC.Bls12_381_G1_equal) = Just 1
    getArityBuiltin (RBuiltin PLC.Bls12_381_G1_hashToGroup) = Just 1
    getArityBuiltin (RBuiltin PLC.Bls12_381_G1_compress) = Just 0
    getArityBuiltin (RBuiltin PLC.Bls12_381_G1_uncompress) = Just 0
    getArityBuiltin (RBuiltin PLC.Bls12_381_G2_add) = Just 1
    getArityBuiltin (RBuiltin PLC.Bls12_381_G2_neg) = Just 0
    getArityBuiltin (RBuiltin PLC.Bls12_381_G2_scalarMul) = Just 1
    getArityBuiltin (RBuiltin PLC.Bls12_381_G2_equal) = Just 1
    getArityBuiltin (RBuiltin PLC.Bls12_381_G2_hashToGroup) = Just 1
    getArityBuiltin (RBuiltin PLC.Bls12_381_G2_compress) = Just 0
    getArityBuiltin (RBuiltin PLC.Bls12_381_G2_uncompress) = Just 0
    getArityBuiltin (RBuiltin PLC.Bls12_381_millerLoop) = Just 1
    getArityBuiltin (RBuiltin PLC.Bls12_381_mulMlResult) = Just 1
    getArityBuiltin (RBuiltin PLC.Bls12_381_finalVerify) = Just 1
    getArityBuiltin (RBuiltin PLC.Sha2_256) = Just 0
    getArityBuiltin (RBuiltin PLC.Sha3_256) = Just 0
    getArityBuiltin (RBuiltin PLC.Blake2b_224) = Just 0
    getArityBuiltin (RBuiltin PLC.Blake2b_256) = Just 0
    getArityBuiltin (RBuiltin PLC.Keccak_256) = Just 0
    getArityBuiltin (RBuiltin PLC.Ripemd_160) = Just 0
    getArityBuiltin (RBuiltin PLC.VerifyEd25519Signature) = Just 2
    getArityBuiltin (RBuiltin PLC.VerifyEcdsaSecp256k1Signature) = Just 2
    getArityBuiltin (RBuiltin PLC.VerifySchnorrSecp256k1Signature) = Just 2
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

  This is approximately a shorthand for a lambda and application:

  @plet v f@ == @ papp (plam f) v@

  But sufficiently small terms in WHNF may be inlined for efficiency.
-}
plet :: Term s a -> (Term s a -> Term s b) -> Term s b
plet v f = Term \i ->
  asRawTerm v i >>= \case
    -- Inline sufficiently small terms in WHNF
    (getTerm -> RVar _) -> asRawTerm (f v) i
    (getTerm -> RBuiltin _) -> asRawTerm (f v) i
    (getTerm -> RHoisted _) -> asRawTerm (f v) i
    _ -> asRawTerm (papp (plam' f) v) i

pthrow' :: HasCallStack => Text -> TermMonad a
pthrow' msg = TermMonad $ ReaderT $ const $ Left (fromString (prettyCallStack callStack) <> "\n\n" <> msg)

pthrow :: HasCallStack => Text -> Term s a
pthrow = Term . pure . pthrow'

-- | Lambda Application.
papp :: Term s (a :--> b) -> Term s a -> Term s b
papp x y = Term \i ->
  (,) <$> asRawTerm x i <*> asRawTerm y i >>= \case
    -- Applying anything to an error is an error.
    (getTerm -> RError, _) -> pure $ mkTermRes RError
    -- Applying an error to anything is an error.
    (_, getTerm -> RError) -> pure $ mkTermRes RError
    -- Applying to `id` changes nothing.
    (getTerm -> RLamAbs 0 (RVar 0), y') -> pure y'
    (getTerm -> RHoisted (HoistedTerm _ (RLamAbs 0 (RVar 0))), y') -> pure y'
    -- append argument
    (x'@(getTerm -> RApply x'l x'r), y') -> pure $ TermResult (RApply x'l (getTerm y' : x'r)) (getDeps x' <> getDeps y')
    -- new RApply
    (x', y') -> pure $ TermResult (RApply (getTerm x') [getTerm y']) (getDeps x' <> getDeps y')

{- |
  Plutus \'delay\', used for laziness.
-}
pdelay :: Term s a -> Term s (PDelayed a)
pdelay x = Term (fmap (mapTerm RDelay) . asRawTerm x)

{- |
  Plutus \'force\',
  used to force evaluation of 'PDelayed' terms.
-}
pforce :: Term s (PDelayed a) -> Term s a
pforce x =
  Term
    ( fmap
        ( \case
            -- A force cancels a delay
            t@(getTerm -> RDelay t') -> t {getTerm = t'}
            t -> mapTerm RForce t
        )
        . asRawTerm x
    )

{- |
  Plutus \'error\'.

  When using this explicitly, it should be ensured that
  the containing term is delayed, avoiding premature evaluation.
-}
perror :: Term s a
perror = Term \_ -> pure $ mkTermRes RError

pgetConfig :: (Config -> Term s a) -> Term s a
pgetConfig f = Term \lvl -> TermMonad $ do
  config <- ask
  runTermMonad $ asRawTerm (f config) lvl

{- |
  Unsafely coerce the type-tag of a Term.

  This should mostly be avoided, though it can be safely
  used to assert known types of Datums, Redeemers or ScriptContext.
-}
punsafeCoerce :: Term s a -> Term s b
punsafeCoerce (Term x) = Term x

punsafeBuiltin :: UPLC.DefaultFun -> Term s a
punsafeBuiltin f = Term \_ -> pure $ mkTermRes $ RBuiltin f

{-# DEPRECATED punsafeConstant "Use `pconstant` instead." #-}
punsafeConstant :: Some (ValueOf PLC.DefaultUni) -> Term s a
punsafeConstant = punsafeConstantInternal

punsafeConstantInternal :: Some (ValueOf PLC.DefaultUni) -> Term s a
punsafeConstantInternal c = Term \_ ->
  pure $ case c of
    -- These constants are smaller than variable references.
    Some (ValueOf PLC.DefaultUniBool _) -> mkTermRes $ RConstant c
    Some (ValueOf PLC.DefaultUniUnit _) -> mkTermRes $ RConstant c
    Some (ValueOf PLC.DefaultUniInteger n) | n < 256 -> mkTermRes $ RConstant c
    _ ->
      let hoisted = HoistedTerm (hashRawTerm $ RConstant c) (RConstant c)
       in TermResult (RHoisted hoisted) [hoisted]

asClosedRawTerm :: ClosedTerm a -> TermMonad TermResult
asClosedRawTerm t = asRawTerm t 0

-- FIXME: Give proper error message when mutually recursive.
phoistAcyclic :: HasCallStack => ClosedTerm a -> Term s a
phoistAcyclic t = Term \_ ->
  asRawTerm t 0 >>= \case
    -- Built-ins are smaller than variable references
    t'@(getTerm -> RBuiltin _) -> pure t'
    t' -> case evalScript . Script . UPLC.Program () uplcVersion $ compile' t' of
      (Right _, _, _) ->
        let hoisted = HoistedTerm (hashRawTerm . getTerm $ t') (getTerm t')
         in pure $ TermResult (RHoisted hoisted) (hoisted : getDeps t')
      (Left e, _, _) -> pthrow' $ "Hoisted term errs! " <> fromString (show e)

-- Couldn't find a definition for this in plutus-core
subst :: Word64 -> (Word64 -> UTerm) -> UTerm -> UTerm
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
  foldr ($) (rawTermToUPLC m (l + n + 1) t) (replicate (fromIntegral $ n + 1) $ UPLC.LamAbs () (DeBruijn . Index $ 0))
rawTermToUPLC m l (RApply x y) =
  let f y t@(UPLC.LamAbs () _ body) =
        case rawTermToUPLC m l y of
          -- Inline unconditionally if it's a variable or built-in.
          -- These terms are very small and are always WHNF.
          UPLC.Var () (DeBruijn (Index idx)) -> subst 1 (\lvl -> UPLC.Var () (DeBruijn . Index $ idx + lvl - 1)) body
          arg@UPLC.Builtin {} -> subst 1 (const arg) body
          arg -> UPLC.Apply () t arg
      f y t = UPLC.Apply () t (rawTermToUPLC m l y)
   in foldr f (rawTermToUPLC m l x) y
rawTermToUPLC m l (RDelay t) = UPLC.Delay () (rawTermToUPLC m l t)
rawTermToUPLC m l (RForce t) = UPLC.Force () (rawTermToUPLC m l t)
rawTermToUPLC _ _ (RBuiltin f) = UPLC.Builtin () f
rawTermToUPLC _ _ (RConstant c) = UPLC.Constant () c
rawTermToUPLC _ _ (RCompiled code) = code
rawTermToUPLC _ _ (RPlaceHolder _) = UPLC.Error ()
rawTermToUPLC _ _ RError = UPLC.Error ()
rawTermToUPLC m l (RConstr i xs) = UPLC.Constr () i (rawTermToUPLC m l <$> xs)
rawTermToUPLC m l (RCase x xs) = UPLC.Case () (rawTermToUPLC m l x) $ V.fromList (rawTermToUPLC m l <$> xs)
-- rawTermToUPLC m l (RHoisted hoisted) = UPLC.Var () . DeBruijn . Index $ l - m hoisted
rawTermToUPLC m l (RHoisted hoisted) = m hoisted l -- UPLC.Var () . DeBruijn . Index $ l - m hoisted

-- The logic is mostly for hoisting
compile' :: TermResult -> UTerm
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
      g (HoistedTerm hash term) (m, defs, n) = case M.alterF (f n) hash m of
        (True, m) -> (m, (n, term) : defs, n + 1)
        (False, m) -> (m, defs, n)

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
      (m, defs, n) = foldr g (M.empty, [], 0) $ filter (\(HoistedTerm hash _) -> not $ S.member hash toInline) deps

      map' (HoistedTerm hash term) l = case M.lookup hash m of
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
compile :: Config -> ClosedTerm a -> Either Text Script
compile config t = case asClosedRawTerm t of
  TermMonad (ReaderT t') -> Script . UPLC.Program () uplcVersion . compile' <$> t' config

{- | As 'compile', but performs UPLC optimizations. Furthermore, this will
always elide tracing (as if with 'NoTracing').

@since WIP
-}
compileOptimized ::
  forall (a :: S -> Type).
  (forall (s :: S). Term s a) ->
  Either Text Script
compileOptimized t = case asClosedRawTerm t of
  TermMonad (ReaderT t') -> do
    configured <- t' NoTracing
    let compiled = compile' configured
    case go compiled of
      Left err -> Left . Text.pack . show $ err
      Right simplified -> pure . Script . UPLC.Program () uplcVersion $ simplified
  where
    go ::
      UPLC.Term UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun () ->
      Either (PLC.Error UPLC.DefaultUni UPLC.DefaultFun ()) (UPLC.Term DeBruijn UPLC.DefaultUni UPLC.DefaultFun ())
    go compiled = flip evalStateT initUPLCSimplifierTrace . PLC.runQuoteT $ do
      unDB <- UPLC.unDeBruijnTerm . UPLC.termMapNames UPLC.fakeNameDeBruijn $ compiled
      simplified <- UPLC.simplifyTerm UPLC.defaultSimplifyOpts def unDB
      debruijnd <- UPLC.deBruijnTerm simplified
      pure . UPLC.termMapNames UPLC.unNameDeBruijn $ debruijnd

hashTerm :: Config -> ClosedTerm a -> Either Text Dig
hashTerm config t = hashRawTerm . getTerm <$> runReaderT (runTermMonad $ asRawTerm t 0) config

{- |
  High precedence infixl synonym of 'papp', to be used like
  function juxtaposition. e.g.:

  >>> f # x # y
  f x y
-}
(#) :: Term s (a :--> b) -> Term s a -> Term s b
(#) = papp

infixl 8 #

{- |
  Low precedence infixr synonym of 'papp', to be used like
  '$', in combination with '#'. e.g.:

  >>> f # x #$ g # y # z
  f x (g y z)
-}
(#$) :: Term s (a :--> b) -> Term s a -> Term s b
(#$) = papp

infixr 0 #$
