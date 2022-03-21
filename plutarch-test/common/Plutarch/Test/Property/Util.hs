{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Test.Property.Util (
  Marshal (marshal),
  leftInverse,
  Equiv (..),
  Totality (..),
  NP ((:*), Nil), -- Re-exports for building Generator lists
  prop_equiv,

  -- * TODO: remove
  runTest,
  viaData,
  viaPEq,
  viaBoth,
  viaPEqPartial,
  viaDataPartial,
  viaBothPartial,

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
import Data.SOP (NP (..), Proxy (..))
import Data.Text (Text)

import Plutus.V1.Ledger.Scripts (Script (..))
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget)

import Hedgehog (Gen, Property, PropertyT, annotate, annotateShow, assert, forAll, property, (===))

data EquivalenceMethod
  = -- test equivalence by converting to data with pdata
    -- and then comparing the Script output in haskell
    ViaPData
  | -- test equivalence inside plutarch with
    -- #== using the type's PEq instance
    ViaPEq
  | -- Partial modifies an EquivalenceMethod
    -- to allow testing partial functions
    -- if the haskell fails but the plutarch succeds
    -- or vice versa the test will fail
    -- but if both fail or both succed and agree
    -- up to the modified EquivalenceMethod
    -- the test will pass
    Partial EquivalenceMethod
  | -- And runs the same test with two
    -- equivalence methods and fails
    -- if either test fails
    And EquivalenceMethod EquivalenceMethod

-- The 'ed classes use tupples like (a,(b,c,())) which are more convenient to abstract over
-- the un'ed classes use regular tuples like (a,b,c) which are more convenient to use

class GensForArgs' h ~ args => HaskellPlutarchEquivalence' (e :: EquivalenceMethod) h (p :: PType) args | h -> p where
  runTest' :: Proxy e -> h -> ClosedTerm p -> args -> PropertyT IO ()

class GensForArgs h ~ args => HaskellPlutarchEquivalence (e :: EquivalenceMethod) h (p :: PType) args | h -> p where
  runTest :: Proxy e -> h -> ClosedTerm p -> AsTuple (GensForArgs' h) -> PropertyT IO ()

{-
 - e is the EquivalenceMethod indicating which sort of equivalence to use
 - ha is the haskell level type while pa is analogous PType

 - when h is a function args contains the generators for it's arguments
 - the first few instances start args as ()
 - then the last instance in this block add generators to the left with tuples
 - when ha is
 - pa , (pb :--> pa) , (pc :--> pb :--> pa) , (pd :--> pc :--> pb :--> pa)
 - args is
 - () , (Gen hb,())   , (Gen hc,(Gen hb,()))   , (Gen hd,(Gen hc,(Gen hb,()))) ...
 -}

instance (PIsData p, Marshal h p, NotLambda h) => HaskellPlutarchEquivalence' 'ViaPData h p () where
  runTest' Proxy ht pt () = testDataEq ht pt

instance (PEq p, Marshal h p, NotLambda h) => HaskellPlutarchEquivalence' 'ViaPEq h p () where
  runTest' Proxy ht pt () = testPEq (marshal ht) pt

instance
  HaskellPlutarchEquivalence' e h p () =>
  HaskellPlutarchEquivalence' ( 'Partial e) h p ()
  where
  runTest' Proxy ht pt () = testPartial baseTest ht pt
    where
      baseTest :: h -> ClosedTerm p -> PropertyT IO ()
      baseTest h p = runTest' (Proxy :: Proxy e) h p ()

instance
  ( HaskellPlutarchEquivalence' l h p ()
  , HaskellPlutarchEquivalence' r h p ()
  ) =>
  HaskellPlutarchEquivalence' ( 'And l r) h p ()
  where
  runTest' Proxy h p () = do
    runTest' (Proxy :: Proxy l) h p ()
    runTest' (Proxy :: Proxy r) h p ()

instance
  ( Show ha
  , Marshal ha pa
  , HaskellPlutarchEquivalence' e hb pb args
  ) =>
  HaskellPlutarchEquivalence' e (ha -> hb) (pa :--> pb) (Gen ha, args)
  where
  runTest' e hf pf (g, args) = do
    x <- forAll g
    runTest' e (hf x) (pf # marshal x) args

{-
 - the next instance moves from the 'ed to the un'ed class
 - to provide less cluttered options for args
 - when ha is
 - pa , (bp :--> pa) , (pc :--> pb :--> pa) , (pd :--> pc :--> pb :--> pa) ...
 - args is now
 - () , Gen hb       , (Gen hc,Gen hb)      , (Gen hd,Gen hc,Gen hb) ...
 - if we encounter higher arity functions
 - it would be easy enough to add
 - instances for clean tuples for them too
 -}

instance
  ( HaskellPlutarchEquivalence' e h p args'
  , AsTuple args' ~ args
  , IsTuple args'
  ) =>
  HaskellPlutarchEquivalence e h p args
  where
  runTest e hf pf args = runTest' e hf pf $ fromTuple args

{-
 - the functions haskPlutEquiv and the proxies viaData viaBoth etc
 - are exposed in the module (runtest and the HaskellPlutarchEquivalence class are not)
 - and are the intended way to indicate which sort
 - of test to use and to run haskell agreement tests in general
-}
leftInverse ::
  forall e t h p p' ha.
  ( LamArgs (h -> h) ~ '[ha]
  , EquivLamProp e t (h -> h) (p :--> p) '[ha]
  , Show ha
  , Marshal ha p
  ) =>
  ClosedTerm (p' :--> p) ->
  ClosedTerm (p :--> p') ->
  Gen h ->
  Property
leftInverse l r arg =
  prop_equiv @e @t (id :: h -> h) (plam $ \x -> l #$ r # x) (arg :* Nil)

viaData :: Proxy 'ViaPData
viaData = Proxy

viaPEq :: Proxy 'ViaPEq
viaPEq = Proxy

viaBoth :: Proxy ( 'And 'ViaPEq 'ViaPData)
viaBoth = Proxy

viaPEqPartial :: Proxy ( 'Partial 'ViaPEq)
viaPEqPartial = Proxy

viaDataPartial :: Proxy ( 'Partial 'ViaPData)
viaDataPartial = Proxy

viaBothPartial :: Proxy ( 'Partial ( 'And 'ViaPEq 'ViaPData))
viaBothPartial = Proxy

data Equiv
  = OnPEq
  | OnPData
  | OnBoth

data Totality
  = TotalFun
  | PartialFun

class EquivProp (e :: Equiv) (t :: Totality) h (p :: PType) where
  equivProp :: h -> ClosedTerm p -> PropertyT IO ()

class EquivLamProp e t hf pf args where
  equivLamProp :: hf -> ClosedTerm pf -> NP Gen args -> PropertyT IO ()

type family LamArgs f :: [Type] where
  LamArgs (a -> b) = a ': LamArgs b
  LamArgs _ = '[]

instance {-# OVERLAPPABLE #-} (EquivProp e t h p, LamArgs h ~ '[]) => EquivLamProp e t h p '[] where
  equivLamProp h p Nil = equivProp @e @t h p

instance
  {-# OVERLAPPING #-}
  (Show ha, Marshal ha pa, EquivLamProp e t hb pb (LamArgs hb), LamArgs (ha -> hb) ~ args) =>
  EquivLamProp e t (ha -> hb) (pa :--> pb) args
  where
  equivLamProp hf pf (a :* as) = do
    x <- forAll a
    equivLamProp @e @t (hf x) (pf # marshal x) as

instance (PIsData p, Marshal h p) => EquivProp 'OnPData 'TotalFun h p where
  equivProp h p = testDataEq h p

instance (PEq p, Marshal h p) => EquivProp 'OnPEq 'TotalFun h p where
  equivProp h p = testPEq (marshal h) p

instance
  ( PEq p
  , PIsData p
  , Marshal h p
  , EquivProp 'OnPEq 'TotalFun h p
  , EquivProp 'OnPData 'TotalFun h p
  ) =>
  EquivProp 'OnBoth 'TotalFun h p
  where
  equivProp h p = do
    equivProp @( 'OnPEq) @( 'TotalFun) h p
    equivProp @( 'OnPData) @( 'TotalFun) h p

instance
  {-# OVERLAPPING #-}
  (PIsData p, Marshal h p, EquivProp eq 'TotalFun h p) =>
  EquivProp eq 'PartialFun h p
  where
  equivProp h p = testPartial (\h' p' -> equivProp @eq @( 'TotalFun) h' p') h p

prop_equiv :: forall e t h p. EquivLamProp e t h p (LamArgs h) => h -> ClosedTerm p -> NP Gen (LamArgs h) -> Property
prop_equiv h p gens = do
  property $ equivLamProp @e @t h p gens

testDataEq :: (PIsData a, Marshal h a) => h -> ClosedTerm a -> PropertyT IO ()
testDataEq x y = testOutputEq (pdata $ marshal x) (pdata y)

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
      testOutputEq (pcon PTrue) (x #== y)
    _ -> assert False

-- testing equality of Scott encoded types
-- this way is generally prone to false errors
-- hence this function not being directly exposed
testOutputEq :: ClosedTerm ha -> ClosedTerm ha -> PropertyT IO ()
testOutputEq x y =
  case (run x, run y) of
    ((Right lb, _, la), (Right rb, _, ra)) -> do
      annotateShow la
      annotateShow ra
      annotateShow $ unScript lb
      annotateShow $ unScript rb
      la === ra
      lb === rb
    _ -> assert False

run :: ClosedTerm h -> (Either EvalError Script, ExBudget, [Text])
run t = evalScript $ compile t

-- GensForArgs f is the tuple of generators for the arguments of F
-- i.e. GensForArgs (a -> b -> c) ~ (Gen a,Gen b)
type family GensForArgs f :: Type where
  GensForArgs f = AsTuple (GensForArgs' f)

-- GensForArgs is the same as GenArgsFor except it uses tuples like (Gen a,(Gen b,()))
type family GensForArgs' f :: Type where
  GensForArgs' (a -> b) = (Gen a, GensForArgs' b)
  GensForArgs' h = ()

class IsTuple t where
  type AsTuple t
  fromTuple :: AsTuple t -> t

instance IsTuple (a, ()) where
  type AsTuple (a, ()) = a
  fromTuple a = (a, ())

instance IsTuple (a, (b, ())) where
  type AsTuple (a, (b, ())) = (a, b)
  fromTuple (a, b) = (a, fromTuple b)

instance IsTuple (a, (b, (c, ()))) where
  type AsTuple (a, (b, (c, ()))) = (a, b, c)
  fromTuple (a, b, c) = (a, fromTuple (b, c))

instance IsTuple (a, (b, (c, (d, ())))) where
  type AsTuple (a, (b, (c, (d, ())))) = (a, b, c, d)
  fromTuple (a, b, c, d) = (a, fromTuple (b, c, d))

instance IsTuple (a, (b, (c, (d, (e, ()))))) where
  type AsTuple (a, (b, (c, (d, (e, ()))))) = (a, b, c, d, e)
  fromTuple (a, b, c, d, e) = (a, fromTuple (b, c, d, e))

instance IsTuple (a, (b, (c, (d, (e, (f, ())))))) where
  type AsTuple (a, (b, (c, (d, (e, (f, ())))))) = (a, b, c, d, e, f)
  fromTuple (a, b, c, d, e, f) = (a, fromTuple (b, c, d, e, f))

type NotLambda a = GensForArgs' a ~ ()
