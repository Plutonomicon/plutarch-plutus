{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Properties.Utils (
  Marshal (marshal),
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

-- TODO
-- Ideally the Marshal class would be replaced with or integrated with the PLift class
-- currentlly this doesn't seem to work
-- because PLift works with builtins instead of Scott encodings
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

class HaskellPlutarchEquivelence (e :: EquivalenceMethod) h (p :: PType) args | h -> p where
  runTest :: Proxy e -> h -> ClosedTerm p -> args -> PropertyT IO ()

{-
 - e is the EquivalenceMethod indicating which sort of equivalence to use
 - ha is the haskell level type while pa is analogous PType

 - when ha is a function args contains the generators for it's arguments
 - the first few instances start args as ()
 - and add generators to the left with tuples
 - when ha is
 - pa , (pb :--> pa) , (pc :--> pb :--> pa) , (pd :--> pc :--> pb :--> pa)
 - args is
 - () , (Gen hb,())   , (Gen hc,(Gen hb,()))   , (Gen hd,(Gen hc,(Gen hb,()))) ...
 -}

instance (PIsData p, Marshal h p) => HaskellPlutarchEquivelence 'ViaPData h p () where
  runTest Proxy ht pt () = testDataEq (marshal ht) pt

instance (PEq p, Marshal h p) => HaskellPlutarchEquivelence 'ViaPEq h p () where
  runTest Proxy ht pt () = testPEq (marshal ht) pt

instance
  HaskellPlutarchEquivelence e h p () =>
  HaskellPlutarchEquivelence ( 'Partial e) h p ()
  where
  runTest Proxy ht pt () = testPartial baseTest ht pt
    where
      baseTest :: h -> ClosedTerm p -> PropertyT IO ()
      baseTest h p = runTest (Proxy :: Proxy e) h p ()

instance
  ( HaskellPlutarchEquivelence l h p ()
  , HaskellPlutarchEquivelence r h p ()
  ) =>
  HaskellPlutarchEquivelence ( 'And l r) h p ()
  where
  runTest Proxy h p () = do
    runTest (Proxy :: Proxy l) h p ()
    runTest (Proxy :: Proxy r) h p ()

instance
  (Show ha, Marshal ha pa, HaskellPlutarchEquivelence e hb pb args) =>
  HaskellPlutarchEquivelence e (ha -> hb) (pa :--> pb) (Gen ha, args)
  where
  runTest e hf pf (g, args) = do
    x <- forAll g
    runTest e (hf x) (pf # marshal x) args

{-
 - the next few instances provide less cluttered options for args
 - when ha is
 - pa , (pb :--> pa) , (pc :--> pb :--> pa) , (pd :--> pc :--> pb :--> pa)
 - args can now be
 - () , Gen hb       , (Gen hc,Gen hb)      , (Gen hd,Gen hc,Gen hb)
 - and finally (Gen he,Gen hd,Gen hc,Gen hb)
 - if we encounter higher arity functions
 - it would be easy enough to add
 - instances for clean tuples for them too
 -}

instance
  HaskellPlutarchEquivelence e ha pa (Gen b, ()) =>
  HaskellPlutarchEquivelence e ha pa (Gen b)
  where
  runTest e hf pf b = runTest e hf pf (b, ())

instance
  HaskellPlutarchEquivelence e ha pa (Gen a, (b, c)) =>
  HaskellPlutarchEquivelence e ha pa (Gen a, b, c)
  where
  runTest e hf pf (a, b, c) = runTest e hf pf (a, (b, c))

instance
  HaskellPlutarchEquivelence e ha pa (Gen a, (b, c, d)) =>
  HaskellPlutarchEquivelence e ha pa (Gen a, b, c, d)
  where
  runTest e hf pf (a, b, c, d) = runTest e hf pf (a, (b, c, d))

{-
- the function haskPlutEquiv and the EquivalenceMethods viaData viaPEq etc
- are exposed in the module (runTest and the HaskellPlutarchEquivelence class are not)
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

instance Marshal ha pa => Marshal [ha] (PList pa) where
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
