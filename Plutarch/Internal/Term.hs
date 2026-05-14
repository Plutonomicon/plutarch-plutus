{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE NoPartialTypeSignatures #-}

module Plutarch.Internal.Term (
  -- | \$hoisted
  (:-->) (PLam),
  PDelayed,
  -- | \$term
  Term (..),
  Script (Script),
  mapTerm,
  plam',
  plet,
  papp,
  pdelay,
  pforce,
  phoistAcyclic,
  perror,
  pplaceholder,
  punsafeCoerce,
  punsafeBuiltin,
  punsafeConstantInternal,
  compile,
  compileWithInternalConfig,
  compileOptimized,
  compileOptimizedWithInternalConfig,
  compile',
  optimizeTerm,
  RawTerm (..),
  HoistedTerm (..),
  TermResult (TermResult, getDeps, getTerm),
  S (SI),
  pthrow,
  Config (NoTracing, Tracing),
  InternalConfig (..),
  TracingMode (..),
  LogLevel (..),
  tracingMode,
  logLevel,
  pgetConfig,
  pgetInternalConfig,
  pwithInternalConfig,
  (#),
  (#$),
) where

import Control.Monad.Reader (
  ReaderT,
  ask,
  asks,
  lift,
  local,
  runReaderT,
 )
import Control.Monad.State.Strict (evalStateT)
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
import Data.Default (def)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable (hash, hashWithSalt), defaultHashWithSalt)
import Data.Kind (Type)
import Data.List (foldl')
import Data.Monoid (Last (Last))
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as V
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import GHC.Word (Word64)
import Plutarch.Internal.Evaluate (evalScript, uplcVersion)
import Plutarch.Script (Script (Script))
import PlutusCore (Some (Some), ValueOf (ValueOf))
import PlutusCore qualified as PLC
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

data HoistedTerm = HoistedTerm {htHash :: Int, htRawTerm :: RawTerm}
  deriving stock (Show)

{- | Hoisted term carry their own non-cryptographic hash, making it incredibly
cheap to hold these in hashmaps.
-}
instance Hashable HoistedTerm where
  hashWithSalt = defaultHashWithSalt
  {-# INLINE hashWithSalt #-}

  -- The instance uses that this is basically 'Hashed a'.
  hash = htHash
  {-# INLINE hash #-}

{- | Equality of hoisted terms is first checked via their non-cryptographic
hash, then via term equality. Inequality, which should happen much more
often is thus cheap.

Note that we could just as well derive this instance...
-}
instance Eq HoistedTerm where
  l == r =
    htHash l == htHash r
      && htRawTerm l == htRawTerm r
  {-# INLINE (==) #-}

data RawTerm
  = RVar Word64
  | RLamAbs Word64 RawTerm
  | RApply RawTerm [RawTerm] -- NB: (f a b c d) ~ RApply f [b c d a]
  | RForce RawTerm
  | RDelay RawTerm
  | RConstant (Some (ValueOf PLC.DefaultUni))
  | RBuiltin PLC.DefaultFun
  | RCompiled (UPLC.Term UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun ())
  | RError
  | RHoisted HoistedTerm
  | RPlaceHolder Integer
  | RConstr Word64 [RawTerm]
  | RCase RawTerm [RawTerm]
  | -- Let x (\x' -> ...)
    RLet RawTerm RawTerm
  | RFix RawTerm
  deriving stock (Show, Eq)

-- | A very cheap hash which cheapens equality, but is also needed for using an unordered container.
instance Hashable RawTerm where
  hashWithSalt = defaultHashWithSalt
  {-# INLINE hashWithSalt #-}
  hash = \case
    RVar x -> hash (0 :: Int, fromIntegral x :: Int)
    RLamAbs n x -> hash (1 :: Int, n, x)
    RApply x y -> hash (2 :: Int, x : y)
    RForce x -> hash (3 :: Int, x)
    RDelay x -> hash (4 :: Int, x)
    RConstant x -> hash (5 :: Int, x)
    RBuiltin x -> hash (6 :: Int, x)
    RError -> 7 :: Int
    RHoisted (HoistedTerm {htHash}) -> hash (8 :: Int, htHash :: Int)
    RCompiled code -> hash (9 :: Int, code)
    RPlaceHolder x -> hash (10 :: Int, x)
    RConstr x y -> hash (11 :: Int, x, y)
    RCase x y -> hash (12 :: Int, x, y)
    RLet x y -> hash (13 :: Int, x, y)
    RFix x -> hash (14 :: Int, x)
  {-# INLINE hash #-}

data TermResult = TermResult
  { getTerm :: RawTerm
  , getDeps :: [HoistedTerm]
  }
  deriving stock (Show)

mapTerm :: (RawTerm -> RawTerm) -> TermResult -> TermResult
mapTerm f (TermResult t d) = TermResult (f t) d

mkTermRes :: RawTerm -> TermResult
mkTermRes r = TermResult r []

{- Type of `s` in `Term s a`. See: "What is the `s`?" section on the Plutarch guide.

`SI` is the identity type of kind `S`. It is used in type class/family instances
to "forget" the `s`.
-}
data S = SI

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
    Just (ll, tm) -> "Tracing" <+> pretty ll <+> pretty tm

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

-- These are settings we need internally
data InternalConfig = InternalConfig
  { internalConfig'dataRecPMatchOptimization :: Bool
  , internalConfig'phoistAcyclicEvalCheck :: Bool
  }
  deriving stock (Show, Eq)

defaultInternalConfig :: InternalConfig
defaultInternalConfig = InternalConfig True True

data TermEnv = TermEnv
  { teDeBruijnDepth :: Word64
  , teInternalConfig :: InternalConfig
  , teConfig :: Config
  }

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
newtype Term (s :: S) (a :: S -> Type) = Term
  { asRawTerm :: ReaderT TermEnv (Either Text) TermResult
  }

type role Term nominal nominal

newtype (:-->) (a :: S -> Type) (b :: S -> Type) (s :: S)
  = PLam (Term s a -> Term s b)
infixr 0 :-->

data PDelayed (a :: S -> Type) (s :: S)

{- |
  Lambda abstraction.

  Only works with a single argument.
  Use 'plam' instead, to support currying.
-}
plam' :: (Term s a -> Term s b) -> Term s (a :--> b)
plam' f = Term $ do
  i <- asks teDeBruijnDepth
  let v = Term $ do
        j <- asks teDeBruijnDepth
        pure . mkTermRes $ RVar (j - (i + 1))
  originalEnv <- ask
  let newEnv = originalEnv {teDeBruijnDepth = i + 1}
  lift . fmap go . runReaderT (asRawTerm (f v)) $ newEnv
  where
    go :: TermResult -> TermResult
    go res@(TermResult t _) =
      let newLambda = mapTerm (RLamAbs 0) res
       in case t of
            -- eta-reduce for arity 1
            RApply t' [RVar 0] -> case getArity t' of
              Nothing -> newLambda
              Just _ -> res {getTerm = t'}
            -- eta-reduce for arity 2 + n
            RLamAbs n (RApply t' args) -> case getArity t' of
              Nothing -> newLambda
              Just n' -> case unpackArgs args of
                Just args' ->
                  if args' == [0 .. n + 1] && n' >= n + 1
                    then res {getTerm = t'}
                    else newLambda
                _ -> newLambda
            -- increment arity
            RLamAbs n t' -> res {getTerm = RLamAbs (n + 1) t'}
            -- New lambda
            _ -> newLambda
    -- Arities are one less than they should be. Thus, 'Nothing' means 'arity
    -- 0', 'Just 0' means 'arity 1', etc.
    getArity :: RawTerm -> Maybe Word64
    getArity = \case
      RHoisted (HoistedTerm _ (RLamAbs n _)) -> Just n
      RHoisted (HoistedTerm _ t) -> getArityBuiltin t
      t -> getArityBuiltin t
    unpackArgs :: [RawTerm] -> Maybe [Word64]
    unpackArgs = traverse (\case RVar n -> Just n; _ -> Nothing)
    -- Note (Koz, 14/04/2026): It is essential that _all_ Plutus Core builtins
    -- are given arities here. Thus, we keep them in a similar order to
    -- PlutusCore.Default.Builtins. However, as some of these builtins are
    -- polymorphic, we have to look inside an `RForce` to 'see' them. Thus,
    -- these get put in their own case branch, but in the same order as they
    -- would appear otherwise.
    getArityBuiltin :: RawTerm -> Maybe Word64
    getArityBuiltin = \case
      RBuiltin t -> case t of
        -- Integers
        PLC.AddInteger -> Just 1
        PLC.SubtractInteger -> Just 1
        PLC.MultiplyInteger -> Just 1
        PLC.DivideInteger -> Just 1
        PLC.QuotientInteger -> Just 1
        PLC.RemainderInteger -> Just 1
        PLC.ModInteger -> Just 1
        PLC.EqualsInteger -> Just 1
        PLC.LessThanEqualsInteger -> Just 1
        -- Bytestrings
        PLC.AppendByteString -> Just 1
        PLC.ConsByteString -> Just 1
        PLC.SliceByteString -> Just 2
        PLC.LengthOfByteString -> Just 0
        PLC.IndexByteString -> Just 1
        PLC.EqualsByteString -> Just 1
        PLC.LessThanByteString -> Just 1
        PLC.LessThanEqualsByteString -> Just 1
        -- Cryptography and hashes
        PLC.Sha2_256 -> Just 0
        PLC.Sha3_256 -> Just 0
        PLC.Blake2b_256 -> Just 0
        PLC.VerifyEd25519Signature -> Just 2
        PLC.VerifyEcdsaSecp256k1Signature -> Just 2
        PLC.VerifySchnorrSecp256k1Signature -> Just 2
        -- Strings
        PLC.AppendString -> Just 1
        PLC.EqualsString -> Just 1
        PLC.EncodeUtf8 -> Just 0
        PLC.DecodeUtf8 -> Just 0
        -- Data
        PLC.ConstrData -> Just 1
        PLC.MapData -> Just 0
        PLC.ListData -> Just 0
        PLC.IData -> Just 0
        PLC.BData -> Just 0
        PLC.UnConstrData -> Just 0
        PLC.UnMapData -> Just 0
        PLC.UnListData -> Just 0
        PLC.UnIData -> Just 0
        PLC.UnBData -> Just 0
        PLC.EqualsData -> Just 1
        PLC.SerialiseData -> Just 0
        -- Misc monomorphized constructors
        PLC.MkPairData -> Just 1
        PLC.MkNilData -> Just 0
        PLC.MkNilPairData -> Just 0
        -- BLS operations
        -- G1
        PLC.Bls12_381_G1_add -> Just 1
        PLC.Bls12_381_G1_neg -> Just 0
        PLC.Bls12_381_G1_scalarMul -> Just 1
        PLC.Bls12_381_G1_equal -> Just 1
        PLC.Bls12_381_G1_hashToGroup -> Just 1
        PLC.Bls12_381_G1_compress -> Just 0
        PLC.Bls12_381_G1_uncompress -> Just 0
        -- G2
        PLC.Bls12_381_G2_add -> Just 1
        PLC.Bls12_381_G2_neg -> Just 0
        PLC.Bls12_381_G2_scalarMul -> Just 1
        PLC.Bls12_381_G2_equal -> Just 1
        PLC.Bls12_381_G2_hashToGroup -> Just 1
        PLC.Bls12_381_G2_compress -> Just 0
        PLC.Bls12_381_G2_uncompress -> Just 0
        -- Pairing
        PLC.Bls12_381_millerLoop -> Just 1
        PLC.Bls12_381_mulMlResult -> Just 1
        PLC.Bls12_381_finalVerify -> Just 1
        -- Keccak, Blake
        PLC.Keccak_256 -> Just 0
        PLC.Blake2b_224 -> Just 0
        -- Conversions
        PLC.IntegerToByteString -> Just 2
        PLC.ByteStringToInteger -> Just 1
        -- Logical
        PLC.AndByteString -> Just 2
        PLC.OrByteString -> Just 2
        PLC.XorByteString -> Just 2
        PLC.ComplementByteString -> Just 0
        PLC.ReadBit -> Just 1
        PLC.WriteBits -> Just 2
        PLC.ReplicateByte -> Just 1
        -- Bitwise
        PLC.ShiftByteString -> Just 1
        PLC.RotateByteString -> Just 1
        PLC.CountSetBits -> Just 0
        PLC.FindFirstSetBit -> Just 0
        -- Ripemd
        PLC.Ripemd_160 -> Just 0
        -- Expmod
        PLC.ExpModInteger -> Just 2
        -- Multi-scalar mult
        PLC.Bls12_381_G1_multiScalarMul -> Just 1
        PLC.Bls12_381_G2_multiScalarMul -> Just 1
        -- Value
        PLC.InsertCoin -> Just 3
        PLC.LookupCoin -> Just 2
        PLC.UnionValue -> Just 1
        PLC.ValueContains -> Just 1
        PLC.ValueData -> Just 0
        PLC.UnValueData -> Just 0
        PLC.ScaleValue -> Just 1
        _ -> Nothing
      RForce (RBuiltin t) -> case t of
        -- Bool
        PLC.IfThenElse -> Just 2
        -- Unit
        PLC.ChooseUnit -> Just 1
        -- Tracing
        PLC.Trace -> Just 1
        -- Pairs
        PLC.FstPair -> Just 0
        PLC.SndPair -> Just 0
        -- Lists
        PLC.ChooseList -> Just 2
        PLC.MkCons -> Just 1
        PLC.HeadList -> Just 0
        PLC.TailList -> Just 0
        PLC.NullList -> Just 0
        -- Data
        PLC.ChooseData -> Just 5
        -- Drop
        PLC.DropList -> Just 1
        -- Arrays
        PLC.LengthOfArray -> Just 0
        PLC.ListToArray -> Just 0
        PLC.IndexArray -> Just 1
        _ -> Nothing
      _ -> Nothing

{- |
  Let bindings.

  This is approximately a shorthand for a lambda and application:

  @plet v f@ == @ papp (plam f) v@

  But sufficiently small terms in WHNF may be inlined for efficiency.
-}
plet :: Term s a -> (Term s a -> Term s b) -> Term s b
plet v f = Term $ do
  env <- ask
  let vRes = runReaderT (asRawTerm v) env
  case vRes of
    Left msg -> lift . Left $ "plet failed: " <> msg
    Right (TermResult vt _vDeps) -> do
      let doInline = asRawTerm (f v)
          noInline (TermResult ft _fDeps) = case ft of
            RError -> pure $ mkTermRes RError
            RLamAbs 0 (RVar 0) -> asRawTerm v
            RHoisted (HoistedTerm _ (RLamAbs 0 (RVar 0))) -> asRawTerm v
            RApply xl xr -> pure . TermResult (RApply xl (vt : xr)) $ _fDeps <> _vDeps
            _ -> pure $ TermResult (RLet vt ft) (_fDeps <> _vDeps)
      case vt of
        -- Inline sufficiently small terms in WHNF
        RVar _ -> doInline
        RBuiltin _ -> doInline
        RHoisted _ -> doInline
        RError -> pure . mkTermRes $ RError
        _ -> noInline =<< asRawTerm (plam' f)

pthrow :: HasCallStack => Text -> Term s a
pthrow msg = Term . lift . Left $ fromString (prettyCallStack callStack) <> "\n\n" <> msg

-- | Lambda Application.
papp :: Term s (a :--> b) -> Term s a -> Term s b
papp x y = Term $ do
  env <- ask
  let yRaw = asRawTerm y
  let fRes = runReaderT (asRawTerm x) env
  let xRes = runReaderT yRaw env
  case (,) <$> fRes <*> xRes of
    Left msg -> lift . Left $ msg
    Right (TermResult ft fDeps, TermResult xt xDeps) -> case (ft, xt) of
      -- Applying anything to an error is an error.
      (RError, _) -> pure . mkTermRes $ RError
      -- Applying an error to anything is an error.
      (_, RError) -> pure . mkTermRes $ RError
      -- Applying anything to `id` does nothing.
      (RLamAbs 0 (RVar 0), _) -> yRaw
      (RHoisted (HoistedTerm _ (RLamAbs 0 (RVar 0))), _) -> yRaw
      -- Append an argument to an existing application.
      (RApply x'l x'r, _) -> pure . TermResult (RApply x'l (xt : x'r)) $ fDeps <> xDeps
      -- New application.
      _ -> pure . TermResult (RApply ft [xt]) $ fDeps <> xDeps

{- |
  Plutus \'delay\', used for laziness.
-}
pdelay :: Term s a -> Term s (PDelayed a)
pdelay = Term . fmap (mapTerm RDelay) . asRawTerm

{- |
  Plutus \'force\',
  used to force evaluation of 'PDelayed' terms.
-}
pforce :: Term s (PDelayed a) -> Term s a
pforce = Term . fmap go . asRawTerm
  where
    go :: TermResult -> TermResult
    go res@(TermResult t deps) = case t of
      -- `force` cancels `delay`
      RDelay t' -> TermResult t' deps
      _ -> mapTerm RForce res

{- |
  Plutus \'error\'.

  When using this explicitly, it should be ensured that
  the containing term is delayed, avoiding premature evaluation.
-}
perror :: Term s a
perror = Term . pure . mkTermRes $ RError

{- |
Same as @perror@ except this holds integer id for AST look-ahead.

This can be used to "tag" branch and generate AST first to see if that branch is actually used or not,
allowing optimization cutting unused branches. For more detailed uscases, check @pmatchDataRec@.
-}
pplaceholder :: Integer -> Term s a
pplaceholder = Term . pure . mkTermRes . RPlaceHolder

pgetConfig :: (Config -> Term s a) -> Term s a
pgetConfig f = Term $ do
  conf <- asks teConfig
  asRawTerm (f conf)

pgetInternalConfig :: (InternalConfig -> Term s a) -> Term s a
pgetInternalConfig f = Term $ do
  iconf <- asks teInternalConfig
  asRawTerm (f iconf)

pwithInternalConfig :: InternalConfig -> Term s a -> Term s a
pwithInternalConfig cfg t = Term $ local (\env -> env {teInternalConfig = cfg}) (asRawTerm t)

{- |
  Unsafely coerce the type-tag of a Term.

  This should mostly be avoided, though it can be safely
  used to assert known types of Datums, Redeemers or ScriptContext.
-}
punsafeCoerce :: forall b a s. Term s a -> Term s b
punsafeCoerce (Term x) = Term x

punsafeBuiltin :: UPLC.DefaultFun -> Term s a
punsafeBuiltin = Term . pure . mkTermRes . RBuiltin

punsafeConstantInternal :: Some (ValueOf PLC.DefaultUni) -> Term s a
punsafeConstantInternal c = Term $ do
  let hoisted = HoistedTerm (hash . RConstant $ c) (RConstant c)
   in pure $ TermResult (RHoisted hoisted) [hoisted]

-- TODO: Give proper error message when mutually recursive.
phoistAcyclic :: forall (a :: S -> Type) (s :: S). HasCallStack => (forall (s' :: S). Term s' a) -> Term s a
phoistAcyclic t = pgetInternalConfig $ \InternalConfig {internalConfig'phoistAcyclicEvalCheck = chk} ->
  Term $ do
    env <- ask
    let res = runReaderT (asRawTerm t) env {teDeBruijnDepth = 0}
    case res of
      Left _ -> lift res
      Right res'@(TermResult t' deps) -> case t' of
        RBuiltin _ -> lift res
        _ ->
          if chk
            then case evalScript . Script . UPLC.Program () uplcVersion $ compile' res' of
              (Left e, _, _) -> asRawTerm . pthrow $ "Hoisted term errors! " <> fromString (show e)
              (Right _, _, _) ->
                let hoisted = HoistedTerm (hash t') t'
                 in pure . TermResult (RHoisted hoisted) $ hoisted : deps
            else
              let hoisted = HoistedTerm (hash t') t'
               in pure . TermResult (RHoisted hoisted) $ hoisted : deps

-- Couldn't find a definition for this in plutus-core
subst ::
  Word64 ->
  (Word64 -> UPLC.Term UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()) ->
  UPLC.Term UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun () ->
  UPLC.Term UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()
subst idx x (UPLC.Apply () yx yy) = UPLC.Apply () (subst idx x yx) (subst idx x yy)
subst idx x (UPLC.LamAbs () name y) = UPLC.LamAbs () name (subst (idx + 1) x y)
subst idx x (UPLC.Delay () y) = UPLC.Delay () (subst idx x y)
subst idx x (UPLC.Force () y) = UPLC.Force () (subst idx x y)
subst idx x y@(UPLC.Var () (DeBruijn (Index idx'))) =
  case compare idx idx' of
    EQ -> x idx
    GT -> y
    LT -> UPLC.Var () (DeBruijn . Index $ idx' - 1)
subst idx x (UPLC.Case () t handlers) = UPLC.Case () (subst idx x t) (fmap (subst idx x) handlers)
subst idx x (UPLC.Constr () w fields) = UPLC.Constr () w (fmap (subst idx x) fields)
subst _ _ y@(UPLC.Constant () _) = y
subst _ _ y@(UPLC.Builtin () _) = y
subst _ _ y@(UPLC.Error ()) = y

rawTermToUPLC ::
  (HoistedTerm -> Word64 -> UPLC.Term DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()) ->
  Word64 ->
  RawTerm ->
  UPLC.Term DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()
rawTermToUPLC resolveHoist level = \case
  -- Variable DeBruijn indices begin from 1
  RVar i -> UPLC.Var () (DeBruijn . Index $ i + 1)
  RDelay t -> UPLC.Delay () (rawTermToUPLC resolveHoist level t)
  RForce t -> UPLC.Force () (rawTermToUPLC resolveHoist level t)
  RBuiltin f -> UPLC.Builtin () f
  RConstant c -> UPLC.Constant () c
  RCompiled code -> code
  RPlaceHolder _ -> UPLC.Error ()
  RError -> UPLC.Error ()
  RConstr i xs -> UPLC.Constr () i (rawTermToUPLC resolveHoist level <$> xs)
  RCase scrutinee cases ->
    let compiledCases = V.fromList (rawTermToUPLC resolveHoist level <$> cases)
     in UPLC.Case () (rawTermToUPLC resolveHoist level scrutinee) compiledCases
  RHoisted hoisted -> resolveHoist hoisted level
  -- The second part of a `let` bind is the 'function part (to follow ordinary
  -- `let` structure from Haskell etc)
  RLet v f -> rawTermToUPLC resolveHoist level (RApply f [v])
  RApply f xs ->
    let fAsRaw = rawTermToUPLC resolveHoist level f
        xsAsRaw = rawTermToUPLC resolveHoist level <$> xs
        -- Some cases are worth inlining
        (body, args) = inline' 0 fAsRaw (reverse xsAsRaw)
     in -- Note (Koz, 13/05/2026): As `UPLC.Apply` is single-arity,
        -- when we need to apply `n` arguments, we have to have `n`
        -- `UPLC.Apply` nodes. This is fine for a small number, but
        -- for a large number, we take a linear blowout to AST size.
        -- In such cases, it is more efficient to do the following:
        --
        -- 1. Temporarily 'wrap up' our arguments in a SOP `Constr`.
        -- 2. Make a single-handler `Case` that's just the body of
        --    whatever we want to apply.
        --
        -- This changes the cost from `n` nodes to just 2, without
        -- changing the semantics.
        if length args <= 2
          then foldl' (UPLC.Apply ()) body args
          else UPLC.Case () (UPLC.Constr () 0 args) (V.singleton body)
  RLamAbs n t ->
    let deAritiedBody = rawTermToUPLC resolveHoist (level + n + 1) t
     in foldr (\_ -> UPLC.LamAbs () (DeBruijn . Index $ 0)) deAritiedBody ([0, 1 .. n] :: [Word64])
  RFix functional ->
    let compiled = rawTermToUPLC resolveHoist level functional
        ownArg = UPLC.Var () . DeBruijn . Index $ 1
        -- M = \x -> x x
        mCombinator = rawLam . UPLC.Apply () ownArg $ ownArg
        (body, args) = inline' 0 mCombinator [compiled]
     in foldl' (UPLC.Apply ()) body args
  where
    {-
      case ty of
        -- Build the necessary functional, then apply it to M
        FixHoisted -> let compiled = rawTermToUPLC resolveHoist level functional
                          ownArg = UPLC.Var () . DeBruijn . Index $ 1
                          parentArg = UPLC.Var () . DeBruijn . Index $ 2
                          -- \x -> F (\v -> (x x) v), where F is the original
                          -- functional
                          functional' = rawLam . UPLC.Apply () compiled . rawLam . UPLC.Apply () (UPLC.Apply () parentArg parentArg) $ ownArg
                          mCombinator = rawTermToUPLC resolveHoist level fixer
                          (body, args) = inline' 0 mCombinator [functional']
                        in foldl' (UPLC.Apply ()) body args
        -- Apply the functional directly to M
        FixNormal -> rawTermToUPLC resolveHoist level (RApply fixer [functional])
    -}

    rawLam ::
      UPLC.Term UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun () ->
      UPLC.Term UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()
    rawLam = UPLC.LamAbs () (DeBruijn . Index $ 0)
    inline' ::
      Word64 ->
      UPLC.Term UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun () ->
      [UPLC.Term UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()] ->
      (UPLC.Term UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun (), [UPLC.Term UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()])
    inline' target func = \case
      -- If there are no arguments at all, there's nothing to inline
      [] -> (func, [])
      (arg : args) -> case func of
        UPLC.LamAbs () x body -> case arg of
          -- If something we're applying is a variable, constant or builtin,
          -- inlining it is cheaper than another `Apply` node.
          UPLC.Var () (DeBruijn (Index idx)) ->
            inline' target (subst 1 (\lvl -> UPLC.Var () (DeBruijn (Index $ idx + lvl - 1 + target))) body) args
          UPLC.Builtin {} -> inline' target (subst 1 (const arg) body) args
          UPLC.Constant {} -> inline' target (subst 1 (const arg) body) args
          -- Skip for now, but there might be something worthwhile later
          _ ->
            let (func', args') = inline' (target + 1) body args
             in (UPLC.LamAbs () x func', arg : args')
        -- Skip for now, but there might be something worthwhile later
        _ ->
          let (func', args') = inline' target func args
           in (func', arg : args')

smallEnoughToInline :: RawTerm -> Bool
smallEnoughToInline = \case
  RConstant (Some (ValueOf PLC.DefaultUniBool _)) -> True
  RConstant (Some (ValueOf PLC.DefaultUniUnit _)) -> True
  RConstant (Some (ValueOf PLC.DefaultUniInteger n)) | n < 256 -> True
  _ -> False

-- The logic is mostly for hoisting.
compile' :: TermResult -> UPLC.Term UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()
compile' t =
  let t' = getTerm t
      deps = getDeps t

      g ::
        HoistedTerm ->
        (HM.HashMap HoistedTerm Word64, [(Word64, RawTerm)], Word64) ->
        (HM.HashMap HoistedTerm Word64, [(Word64, RawTerm)], Word64)
      g hoistedTerm@(HoistedTerm _ term) (m, defs, n) = case HM.lookup hoistedTerm m of
        Nothing -> (HM.insert hoistedTerm n m, (n, term) : defs, n + 1)
        Just _w64 -> (m, defs, n)

      toInline :: HM.HashMap HoistedTerm Int
      toInline =
        -- keep only count "1"s or small enough to inline ones
        HM.filterWithKey (\(HoistedTerm _ term) count -> count == 1 || smallEnoughToInline term) depsCount

      -- count how often a hoisted term occurs
      depsCount :: HM.HashMap HoistedTerm Int
      depsCount = HM.fromListWith (+) . map (,1) $ deps

      -- map: term -> de Bruijn level
      -- defs: the terms, level 0 is last
      -- n: # of terms
      (m :: HM.HashMap HoistedTerm Word64, defs, n) =
        foldr g (HM.empty, [], 0) $ filter (\hoistedTerm -> not $ HM.member hoistedTerm toInline) deps

      map' hoistedTerm@(HoistedTerm _ term) l = case HM.lookup hoistedTerm m of
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
compile :: forall (a :: S -> Type). Config -> (forall (s :: S). Term s a) -> Either Text Script
compile = compileWithInternalConfig defaultInternalConfig

{- | As 'compile' but exposes 'InternalConfig' options.

@since 1.12.0
-}
compileWithInternalConfig ::
  forall (a :: S -> Type).
  InternalConfig -> Config -> (forall (s :: S). Term s a) -> Either Text Script
compileWithInternalConfig iconf conf t = case runReaderT (asRawTerm t) (TermEnv 0 iconf conf) of
  res -> Script . UPLC.Program () uplcVersion . compile' <$> res

{- | As 'compile', but performs UPLC optimizations. Furthermore, this will
always elide tracing (as if with 'NoTracing').

@since 1.10.0
-}
compileOptimized ::
  forall (a :: S -> Type).
  (forall (s :: S). Term s a) ->
  Either Text Script
compileOptimized = compileOptimizedWithInternalConfig defaultInternalConfig

{- | As 'compileOptimized' but exposes 'InternalConfig' options.

@since 1.12.0
-}
compileOptimizedWithInternalConfig ::
  forall (a :: S -> Type).
  InternalConfig ->
  (forall (s :: S). Term s a) ->
  Either Text Script
compileOptimizedWithInternalConfig internalConfig t = do
  let env = TermEnv 0 internalConfig NoTracing
  built <- runReaderT (asRawTerm t) env
  let compiled = compile' built
  case go compiled of
    Left err -> Left . Text.pack . show $ err
    Right simplified -> pure . Script . UPLC.Program () uplcVersion $ simplified
  where
    go ::
      UPLC.Term UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun () ->
      Either UPLC.FreeVariableError (UPLC.Term DeBruijn UPLC.DefaultUni UPLC.DefaultFun ())
    go compiled = flip evalStateT UPLC.initSimplifierTrace . PLC.runQuoteT $ do
      unDB <- UPLC.unDeBruijnTerm . UPLC.termMapNames UPLC.fakeNameDeBruijn $ compiled
      simplified <- UPLC.simplifyTerm UPLC.defaultSimplifyOpts def unDB
      debruijnd <- UPLC.deBruijnTerm simplified
      pure . UPLC.termMapNames UPLC.unNameDeBruijn $ debruijnd

{- | Given a closed 'Term', run the UPLC optimizer on it.

= Important note

If the input term has any hoisted dependencies, these are completely erased
by this process. Thus, if the resulting 'Term' is used as part of a larger
computation with the same dependencies, the Plutarch compiler will not be
aware of them, and will not be able to optimize them properly. More
concretely, in a case like this:

@@
padd # optimizeTerm (f # pexpensive) # optimizeTerm (g # pexpensive)
@@

@pexpensive@ will end up being duplicated entirely into each \'arm\' of the
@padd@, even though under normal circumstances it could be shared.

Thus, if you plan to use this, ensure that it is done \'as late as
possible\'; embedding 'Term's produced by 'optimizeTerm' into larger
computations can lead to size blowout if not done carefully.
-}
optimizeTerm ::
  forall (a :: S -> Type).
  (forall (s :: S). Term s a) ->
  (forall (s :: S). Term s a)
optimizeTerm (Term raw) = Term $ do
  env <- ask
  let res = runReaderT raw env
  let compiled = compile' <$> res
  case compiled of
    Left err -> lift . Left $ err
    Right compiled' -> case go compiled' of
      Left err -> lift . Left . Text.pack . show $ err
      Right simplified -> pure . TermResult (RCompiled simplified) $ []
  where
    go ::
      UPLC.Term UPLC.DeBruijn UPLC.DefaultUni UPLC.DefaultFun () ->
      Either UPLC.FreeVariableError (UPLC.Term DeBruijn UPLC.DefaultUni UPLC.DefaultFun ())
    go compiled = flip evalStateT UPLC.initSimplifierTrace . PLC.runQuoteT $ do
      unDB <- UPLC.unDeBruijnTerm . UPLC.termMapNames UPLC.fakeNameDeBruijn $ compiled
      simplified <- UPLC.simplifyTerm UPLC.defaultSimplifyOpts def unDB
      debruijnd <- UPLC.deBruijnTerm simplified
      pure . UPLC.termMapNames UPLC.unNameDeBruijn $ debruijnd

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
