{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Test.Property.Util (
  Equality (..),
  Totality (..),
  NP ((:*), Nil), -- Re-exports for building Generator lists
  prop_equiv,
  prop_leftInverse,

  -- * For writing helper functions using `prop_equiv`
  LamArgs,
  HaskEquiv,

  -- * Maybe keep
  testDataEq,
  testPEq,
) where

import Plutarch (ClosedTerm, compile)
import Plutarch.Evaluate (EvalError, evalScript)
import Plutarch.Prelude
import Plutarch.Test.Property.Marshal (Marshal (marshal))

import Control.Exception (SomeException, evaluate, try)
import Control.Monad.IO.Class (liftIO)
import Data.SOP (NP (Nil, (:*)))
import Data.Text (Text)

import Plutus.V1.Ledger.Scripts (Script (..))
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget)

import Hedgehog (Gen, Property, PropertyT, annotate, annotateShow, assert, forAll, property, (===))

-- | The nature of equality between two Plutarch terms.
data Equality
  = OnPEq
  | OnPData
  | OnBoth
  deriving stock (Eq, Show, Ord)

-- | Whether a function is total or partial.
data Totality
  = TotalFun
  | PartialFun
  deriving stock (Eq, Show, Ord)

{- |
  Like `HaskEquiv` but limited to non-lambda terms.
-}
class HaskEquivTerm (e :: Equality) (t :: Totality) h (p :: PType) where
  haskEquivTerm :: h -> ClosedTerm p -> PropertyT IO ()

{- |
  Class of pairs of Plutarch and Haskell types that are semantically
  equivalent, upto the given equality and totality.
-}
class HaskEquiv equivalence totality hf pf args where
  -- | Test that `hf` and `pf` are equal when applied on the given arguments.
  haskEquiv :: hf -> ClosedTerm pf -> NP Gen args -> PropertyT IO ()

instance (HaskEquivTerm e t h p, LamArgs h ~ '[]) => HaskEquiv e t h p '[] where
  haskEquiv h p Nil = haskEquivTerm @e @t h p

instance
  (Show ha, Marshal ha pa, HaskEquiv e t hb pb (LamArgs hb), LamArgs (ha -> hb) ~ args) =>
  HaskEquiv e t (ha -> hb) (pa :--> pb) args
  where
  haskEquiv hf pf (a :* as) = do
    x <- forAll a
    haskEquiv @e @t (hf x) (pf # marshal x) as

instance (PIsData p, Marshal h p) => HaskEquivTerm 'OnPData 'TotalFun h p where
  haskEquivTerm h p = testDataEq h p

instance (PEq p, Marshal h p) => HaskEquivTerm 'OnPEq 'TotalFun h p where
  haskEquivTerm h p = testPEq (marshal h) p

instance
  ( PEq p
  , PIsData p
  , Marshal h p
  , HaskEquivTerm 'OnPEq 'TotalFun h p
  , HaskEquivTerm 'OnPData 'TotalFun h p
  ) =>
  HaskEquivTerm 'OnBoth 'TotalFun h p
  where
  haskEquivTerm h p = do
    haskEquivTerm @( 'OnPEq) @( 'TotalFun) h p
    haskEquivTerm @( 'OnPData) @( 'TotalFun) h p

instance
  (PIsData p, Marshal h p, HaskEquivTerm eq 'TotalFun h p) =>
  HaskEquivTerm eq 'PartialFun h p
  where
  haskEquivTerm h p = testPartial (\h' p' -> haskEquivTerm @eq @( 'TotalFun) h' p') h p

prop_equiv :: forall e t h p. HaskEquiv e t h p (LamArgs h) => h -> ClosedTerm p -> NP Gen (LamArgs h) -> Property
prop_equiv h p gens = do
  property $ haskEquiv @e @t h p gens

testDataEq :: (PIsData a, Marshal h a) => h -> ClosedTerm a -> PropertyT IO ()
testDataEq x y =
  pshouldBe (pdata $ marshal x) (pdata y)

testPartial :: (h -> ClosedTerm p -> PropertyT IO ()) -> h -> ClosedTerm p -> PropertyT IO ()
testPartial baseTest h p =
  liftIO (try $ evaluate h) >>= \case
    Left (_ :: SomeException) ->
      case run p of
        (Left _, _, _) -> assert True
        (Right _, _, _) -> do
          annotate "plutarch didn't fail but haskell did"
          assert False
    Right _ -> baseTest h p

testPEq :: PEq a => ClosedTerm a -> ClosedTerm a -> PropertyT IO ()
testPEq x y =
  case (run x, run y) of
    ((Right (Script x'), _, _), (Right (Script y'), _, _)) -> do
      annotateShow x'
      annotateShow y'
      pshouldBe (pcon PTrue) (x #== y)
    _ -> assert False

-- | Like `Plutarch.Test.pshouldBe` but in Hedgehog property monad.
pshouldBe :: ClosedTerm a -> ClosedTerm a -> PropertyT IO ()
pshouldBe x y =
  -- testing equality of Scott encoded types
  -- this way is generally prone to false errors
  -- hence this function not being directly exposed
  case (run x, run y) of
    ((Right script1, _, trace1), (Right script2, _, trace2)) -> do
      annotateShow trace1
      annotateShow trace2
      annotateShow $ unScript script1
      annotateShow $ unScript script2
      trace1 === trace2
      script1 === script2
    _ -> assert False

run :: ClosedTerm h -> (Either EvalError Script, ExBudget, [Text])
run t = evalScript $ compile t

prop_leftInverse ::
  forall e t p p' h.
  ( LamArgs h ~ '[]
  , HaskEquiv e t (h -> h) (p :--> p) '[h]
  , Show h
  , Marshal h p
  ) =>
  ClosedTerm (p' :--> p) ->
  ClosedTerm (p :--> p') ->
  Gen h ->
  Property
prop_leftInverse l r arg =
  prop_equiv @e @t (id @h) (plam $ \x -> l #$ r # x) (arg :* Nil)

type family LamArgs f :: [Type] where
  LamArgs (a -> b) = a ': LamArgs b
  LamArgs _ = '[]
