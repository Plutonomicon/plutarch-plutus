{-# Options_GHC -w #-}
{-# Language FlexibleInstances #-}
{-# Language ImpredicativeTypes #-}
{-# Language UndecidableInstances #-}
{-# Language AllowAmbiguousTypes #-}

module Plutarch.Internal2 where

import Data.Coerce
import Data.Kind
import Data.String (fromString)
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Except (ExceptT(..), Except)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader (MonadReader, ReaderT (ReaderT), asks)
import Control.Monad.Zip (MonadZip)
import Data.Default (Default (def))
import Data.Functor.Identity (Identity(..))
import Data.Kind (Type)
import Data.Text (Text)
import GHC.Word (Word64)

import Crypto.Hash (Context, Digest, hashFinalize, hashInit, hashUpdate)
import Crypto.Hash.Algorithms (Blake2b_160)
import PlutusCore (Some (Some), ValueOf (ValueOf))
import PlutusCore qualified as PLC
import PlutusCore.DeBruijn (DeBruijn (DeBruijn), Index (Index))
import PlutusCore.Default (DefaultUni(..))
import PlutusLedgerApi.V1.Scripts (Script (Script))
import UntypedPlutusCore qualified as UPLC

import Plutarch.PType
import Plutarch.Core

type Dig = Digest Blake2b_160

data HoistedTerm = HoistedTerm Dig RawTerm
  deriving stock (Show)

type UTerm :: Type
type UTerm = UPLC.Term DeBruijn UPLC.DefaultUni UPLC.DefaultFun ()

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
  deriving stock (Show)

data TermResult = TermResult
  { getTerm :: RawTerm
  , getDeps :: [HoistedTerm]
  }

mapTerm :: (RawTerm -> RawTerm) -> TermResult -> TermResult
mapTerm f (TermResult t d) = TermResult (f t) d

mkTermRes :: RawTerm -> TermResult
mkTermRes r = TermResult r []

data Config = Config
  { tracingMode :: TracingMode
  }

data TracingMode = NoTracing | DoTracing | DetTracing

-- | Default is to be efficient
instance Default Config where
  def =
    Config
      { tracingMode = NoTracing
      }

newtype TermMonad m = TermMonad {runTermMonad :: Config -> Either Text m}
  deriving
    ( Functor, Applicative, Alternative
    , Monad, MonadPlus, MonadFix, MonadZip, MonadReader Config
    )
  via ReaderT Config (Except Text)

deBruijnInitIndex :: Word64
deBruijnInitIndex = 0

succIndex :: Word64 -> Word64
succIndex = succ

type    Impl :: PType -> Type
newtype Impl a = Impl { runImpl :: Word64 -> TermMonad TermResult }

type Impl' :: PDSLKind
type Impl' = 'PDSLKind Impl

{-# Complete TheImpl #-}

type TheImpl :: PType -> Type
type TheImpl = Term Impl'

pattern TheImpl :: (Word64 -> TermMonad TermResult) -> Term Impl' a
pattern TheImpl { runTheImpl } = Term (Impl runTheImpl)

instance PDSL Impl'

instance Applicative f => PConstructable' Impl' PUnit where
  pconImpl :: PConcrete Impl' PUnit -> Impl PUnit
  pconImpl PUnit = Impl \_ -> pure unit
    where
    unit :: TermResult
    unit = TermResult 
      { getTerm = RConstant (Some (ValueOf DefaultUniUnit ()))
      , getDeps = []
      }

  pmatchImpl :: Impl PUnit -> (PConcrete Impl' PUnit -> Term Impl' b) -> Term Impl' b
  pmatchImpl _ next = next PUnit

instance PConstructable' Impl' (PLet a) where
  pconImpl :: PConcrete Impl' (PLet a) -> Impl (PLet a)
  pconImpl (PLet term) = coerce term

  pmatchImpl :: Impl (PLet a) -> (PConcrete Impl' (PLet a) -> Term Impl' b) -> Term Impl' b
  pmatchImpl term next = next do
    PLet (coerce term)

instance PConstructable' Impl' (a #-> b) where
  pconImpl :: PConcrete Impl' (a #-> b) -> Impl (a #-> b)
  pconImpl (PLam body) = Impl \lvl -> let

    f :: TermResult -> TermResult
    f t = case getTerm t of
      -- eta-reduce for arity 1
      RApply t'@(getArity -> Just _) [RVar 0] -> t {getTerm = t'}
      -- eta-reduce for arity 2 + n
      RLamAbs n (RApply t'@(getArity -> Just n') args)
        | (== Just [0 .. n + 1]) (traverse (\case RVar n -> Just n; _ -> Nothing) args)
            && n' >= n + 1 ->
            t {getTerm = t'}
      -- increment arity
      RLamAbs n t' -> t {getTerm = RLamAbs (n + 1) t'}
      -- new lambda
      _ -> mapTerm (RLamAbs 0) t

    x :: TermMonad TermResult
    x = runTheImpl (body v) (lvl + 1)

    v :: TheImpl a
    v = TheImpl \j -> pure $ mkTermRes $ RVar (j - (lvl + 1))

    in 
    f <$> x

  pmatchImpl :: IsPType Impl' x => Impl (a #-> b) -> (PConcrete Impl' (a #-> b) -> Term Impl' x) -> Term Impl' x
  pmatchImpl (Impl fun) next = next $ PLam \(TheImpl term) -> TheImpl \lvl -> 
    (,) <$> fun lvl <*> term lvl >>= \case
      -- Applying anything to an error is an error.
      (getTerm -> RError, _) -> pthrow' "application to an error"
      -- Applying an error to anything is an error.
      (_, getTerm -> RError) -> pthrow' "application with an error"
      -- Applying to `id` changes nothing.
      (getTerm -> RLamAbs 0 (RVar 0), y') -> pure y'
      (getTerm -> RHoisted (HoistedTerm _ (RLamAbs 0 (RVar 0))), y') -> pure y'
      -- append argument
      (x'@(getTerm -> RApply x'l x'r), y') -> pure $ TermResult (RApply x'l (getTerm y' : x'r)) (getDeps x' <> getDeps y')
      -- new RApply
      (x', y') -> pure $ TermResult (RApply (getTerm x') [getTerm y']) (getDeps x' <> getDeps y')

instance (PHasRepr a, PHasRepr b) => PConstructable' Impl' (PPair a b) where
  pconImpl :: PConcrete Impl' (PPair a b) -> Impl (PPair a b)
  pconImpl (PPair (TheImpl a) (TheImpl b)) = coerce (plam body) where

    body :: Term Impl' (a #-> b #-> PPair a b)
         -> Term Impl' (PPair a b)
    body pair = pair # TheImpl a # TheImpl b

  pmatchImpl
    :: forall x. ()
    => IsPType Impl' x
    => Impl (PPair a b)
    -> (PConcrete Impl' (PPair a b)
    -> Term Impl' x)
    -> Term Impl' x
  pmatchImpl (Impl term) next = undefined  where

    body :: Term Impl' x
    body = plet (TheImpl term) \(TheImpl thePair) -> let

      one :: Term Impl' a
      two :: Term Impl' b
      one = TheImpl thePair # plam2 \a b -> a
      two = TheImpl thePair # plam2 \a b -> b

      in next (PPair one two)

instance m ~ TermMonad => PAp m Impl' where
  papr :: TermMonad a -> Term Impl' b -> Term Impl' b
  papr as (TheImpl term) = TheImpl \lvl -> as *> term lvl

  papl :: Term Impl' a -> TermMonad b -> Term Impl' a
  papl (TheImpl term) bs = TheImpl \lvl -> term lvl <* bs
  
instance m ~ TermMonad => PEmbeds m Impl' where
  pembed :: TermMonad (Term Impl' a) -> Term Impl' a
  pembed terms = TheImpl \lvl -> do
    impl <- terms
    runTheImpl impl lvl 

asClosedRawTerm :: forall cls a edsl. ClosedTerm cls a -> cls edsl => UnEDSLKind edsl (PRepr a)
asClosedRawTerm closed
  | Term term <- closed @edsl
  = term

asClosedRawTerm' :: ClosedTerm cls a
                 -> (cls Impl' => TermMonad TermResult)
asClosedRawTerm' (TheImpl term) = term deBruijnInitIndex

compile' :: forall a. IsPType Impl' a => Term Impl' a -> TermMonad TermResult
compile' (TheImpl term) = term deBruijnInitIndex

-- compile :: forall cls a. Config -> ClosedTerm cls a -> Either Text Script
-- compile config t = case asClosedRawTerm @cls @a t of
--   TermMonad ok -> undefined 
--   -- TermMonad t' -> (Script . UPLC.Program () (UPLC.Version () 1 0 0) . compile') <$> t' config


-- -- -- type EUPLC :: EDSLKind -> Constraint
-- -- type EUPLC :: PDSLKind -> Constraint
-- -- type EUPLC edsl = () -- PLC edsl --, PPolymorphic edsl, PSOP edsl)

-- -- compile 
-- --   :: forall m a. HasCallStack
-- --   => Monad m
-- --   => forall edsl. EUPLC edsl => IsPType edsl a
-- --   => (forall edsl. (EUPLC edsl, PEmbeds m edsl) => Term edsl a) 
-- --   -> m UTerm
-- -- compile t = let _unused = callStack in compile' @a @m t
pthrow' :: HasCallStack => Text -> TermMonad a
pthrow' msg = TermMonad $ const $ Left (fromString (prettyCallStack callStack) <> "\n\n" <> msg)

pthrow :: HasCallStack => Text -> Impl a
pthrow = Impl . pure . pthrow'

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
