{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Test.Property.Util (
  Marshal (marshal),
  NotLambda,
  haskPlutEquiv,
  leftInverse,
  viaData,
  viaPEq,
  viaBoth,
  viaPEqPartial,
  viaDataPartial,
  viaBothPartial,
  testDataEq,
  testPEq,
  run,
) where

import Plutarch.Prelude

import Plutarch (ClosedTerm, compile)

import Plutarch.Evaluate (evaluateScript)

import Control.Exception (SomeException, evaluate, try)
import Control.Monad.IO.Class (liftIO)
import Data.Proxy (Proxy (..))
import Data.Text (Text)

import Plutus.V1.Ledger.Scripts (Script (..), ScriptError)
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget)

import Hedgehog (Gen, Property, PropertyT, annotate, annotateShow, assert, forAll, property, (===))

class Marshal h (p :: PType) | h -> p where
  marshal :: h -> ClosedTerm p

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

class GensForArgs' h ~ args => HaskellPlutarchEquivelence' (e :: EquivalenceMethod) h (p :: PType) args | h -> p where
  runTest' :: Proxy e -> h -> ClosedTerm p -> args -> PropertyT IO ()

class GensForArgs h ~ args => HaskellPlutarchEquivelence (e :: EquivalenceMethod) h (p :: PType) args | h -> p where
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

instance (PIsData p, Marshal h p, NotLambda h) => HaskellPlutarchEquivelence' 'ViaPData h p () where
  runTest' Proxy ht pt () = testDataEq (marshal ht) pt

instance (PEq p, Marshal h p, NotLambda h) => HaskellPlutarchEquivelence' 'ViaPEq h p () where
  runTest' Proxy ht pt () = testPEq (marshal ht) pt

instance
  HaskellPlutarchEquivelence' e h p () =>
  HaskellPlutarchEquivelence' ( 'Partial e) h p ()
  where
  runTest' Proxy ht pt () = testPartial baseTest ht pt
    where
      baseTest :: h -> ClosedTerm p -> PropertyT IO ()
      baseTest h p = runTest' (Proxy :: Proxy e) h p ()

instance
  ( HaskellPlutarchEquivelence' l h p ()
  , HaskellPlutarchEquivelence' r h p ()
  ) =>
  HaskellPlutarchEquivelence' ( 'And l r) h p ()
  where
  runTest' Proxy h p () = do
    runTest' (Proxy :: Proxy l) h p ()
    runTest' (Proxy :: Proxy r) h p ()

instance
  ( Show ha
  , Marshal ha pa
  , HaskellPlutarchEquivelence' e hb pb args
  ) =>
  HaskellPlutarchEquivelence' e (ha -> hb) (pa :--> pb) (Gen ha, args)
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
  ( HaskellPlutarchEquivelence' e h p args'
  , AsTuple args' ~ args
  , IsTuple args'
  ) =>
  HaskellPlutarchEquivelence e h p args
  where
  runTest e hf pf args = runTest' e hf pf $ fromTuple args

{-
- the functions haskPlutEquiv and the proxies viaData viaBoth etc
- are exposed in the module (runtest and the HaskellPlutarchEquivelence class are not)
- and are the intended way to indicate which sort
- of test to use and to run haskell agreement tests in general
-}

haskPlutEquiv ::
  HaskellPlutarchEquivelence e h p args =>
  Proxy (e :: EquivalenceMethod) ->
  h ->
  ClosedTerm p ->
  args ->
  Property
haskPlutEquiv proxy h p args = property $ runTest proxy h p args

leftInverse ::
  forall h e p p'.
  HaskellPlutarchEquivelence e (h -> h) (p :--> p) (Gen h) =>
  Proxy (e :: EquivalenceMethod) ->
  ClosedTerm (p' :--> p) ->
  ClosedTerm (p :--> p') ->
  Gen h ->
  Property
leftInverse proxy l r =
  haskPlutEquiv proxy (id :: h -> h) (plam $ \x -> l #$ r # x)

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

testDataEq :: PIsData a => ClosedTerm a -> ClosedTerm a -> PropertyT IO ()
testDataEq x y = testOutputEq (pdata x) (pdata y)

testPartial :: (h -> ClosedTerm p -> PropertyT IO ()) -> h -> ClosedTerm p -> PropertyT IO ()
testPartial baseTest h p =
  liftIO (try $ evaluate h) >>= \case
    Left (_ :: SomeException) ->
      case run p of
        Left _ -> assert True
        Right _ -> do
          annotate "plutarch didn't fail but haskell did"
          assert False
    Right _ -> baseTest h p

testPEq :: PEq a => ClosedTerm a -> ClosedTerm a -> PropertyT IO ()
testPEq x y =
  case (run x, run y) of
    (Right (_, _, Script x'), Right (_, _, Script y')) -> do
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
    (Right (_, la, lb), Right (_, ra, rb)) -> do
      annotateShow la
      annotateShow ra
      annotateShow $ unScript lb
      annotateShow $ unScript rb
      la === ra
      lb === rb
    _ -> assert False

run :: ClosedTerm h -> Either ScriptError (ExBudget, [Text], Script)
run t = evaluateScript $ compile t

instance Marshal h p => Marshal [h] (PList p) where
  marshal xs = foldr (\h t -> pcons # marshal h # t) pnil xs

instance Marshal ha pa => Marshal (Maybe ha) (PMaybe pa) where
  marshal (Just x) = pcon $ PJust $ marshal x
  marshal Nothing = pcon PNothing

instance (Marshal ha pa, Marshal hb pb) => Marshal (ha, hb) (PPair pa pb) where
  marshal (a, b) = pcon $ PPair (marshal a) (marshal b)

instance Marshal Integer PInteger where
  marshal n = fromInteger n

instance Marshal Rational PRational where
  marshal r = fromRational r

instance Marshal Bool PBool where
  marshal True = pcon PTrue
  marshal False = pcon PFalse

instance Marshal () PUnit where
  marshal () = pcon PUnit

-- GensForArgs f is the tuple of generators for the arguments of F
-- ie GensForArgs (a -> b -> c) ~ (Gen a,Gen b)
type family GensForArgs f :: Type where
  GensForArgs f = AsTuple (GensForArgs' f)

-- GensForArgs is the same as GenArgsFor except it uses tupples like (Gen a,(Gen b,()))
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
