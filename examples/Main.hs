{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main (main, iterateN, swap, fromScriptConstant) where

import Data.Function ((&))
import Test.Tasty
import Test.Tasty.HUnit
import Flat qualified as Flat

import Plutarch (ClosedTerm, POpaque, compile, printScript, printTerm, punsafeBuiltin, punsafeConstant, PlutusType (..))
import Plutarch.Bool (PBool (PTrue), pif, (#==))
import Plutarch.Builtin (PBuiltinList, PBuiltinPair)
import Plutarch.ByteString (phexByteStr)
import Plutarch.Either (PEither (PLeft, PRight))
import Plutarch.Evaluate (evaluateScript)
import Plutarch.Integer (PInteger)
import Plutarch.String (PString, pfromText)
import Plutarch.Unit (PUnit (..))

-- FIXME:remove
import Plutarch.Prelude
import qualified Plutus.V1.Ledger.Scripts as Scripts
import qualified PlutusCore as PLC
import UntypedPlutusCore.Core.Type qualified as UPLC
import PlutusTx qualified as PTX
import PlutusCore.Data ()
import PlutusCore.Constant qualified as PLC

main :: IO ()
main = defaultMain tests

add1 :: Term s (PInteger :--> PInteger :--> PInteger)
add1 = plam $ \x y -> x + y + 1

add1Hoisted :: Term s (PInteger :--> PInteger :--> PInteger)
add1Hoisted = phoistAcyclic $ plam $ \x y -> x + y + 1

example1 :: Term s PInteger
example1 = add1Hoisted # 12 # 32 + add1Hoisted # 5 # 4

example2 :: Term s (PEither PInteger PInteger :--> PInteger)
example2 = plam $ \x -> pmatch x $ \case
  PLeft n -> n + 1
  PRight n -> n - 1

fib :: Term s (PInteger :--> PInteger)
fib = phoistAcyclic $
  pfix #$ plam $ \self n ->
    pif
      (n #== 0)
      0
      $ pif
        (n #== 1)
        1
        $ self # (n - 1) + self # (n - 2)

iterateN :: Term s (PInteger :--> (a :--> a) :--> a :--> a)
iterateN = pfix # plam iterateN'
  where
    iterateN' :: 
      Term s (PInteger :--> (a :--> a) :--> a :--> a) -> 
      Term s PInteger ->
      Term s (a :--> a) ->
      Term s a ->
      Term s a
  
    iterateN' self n f x = 
      pif 
        (n #== 0) 
        x 
        (self # (n - 1) # f #$ f # x)

data AB (s :: k) = A | B
  
instance PlutusType AB where
  type PInner AB _ = PInteger
  
  pcon' A = 0
  pcon' B = 1

  pmatch' x f = 
    pif (x #== 0) (f A) (f B)

swap :: Term s AB -> Term s AB
swap x = pmatch x $ \case
 A -> pcon B
 B -> pcon A

uglyDouble :: Term s (PInteger :--> PInteger)
uglyDouble = plam $ \n -> plet n $ \n1 -> plet n1 $ \n2 -> n2 + n2

eval :: HasCallStack => ClosedTerm a -> IO Scripts.Script
eval x = case evaluateScript $ compile x of
  Left e -> assertFailure $ "Script evaluation failed: " <> show e
  Right (_, _, x') -> pure x'

fromScriptConstant :: 
  forall a.
  ( Flat.Flat a 
  , PLC.DefaultUni `PLC.Contains` a
  ) => Scripts.Script -> Maybe a
fromScriptConstant s = do
  (Scripts.unScript s)
    & UPLC.toTerm
    & \case
  where
    fromTerm 
      (UPLC.Constant _ 
        (PLC.Some (PLC.ValueOf termUni x))) = 
      let exUni = PLC.knownUni @_ @PLC.DefaultUni @a
      in
      case termUni `PLC.geq` exUni of
        (Just PLC.Refl) -> Just x
        _ -> Nothing

    fromTerm _ = Nothing
    


equal :: HasCallStack => ClosedTerm a -> ClosedTerm b -> Assertion
equal x y = do
  x' <- eval x
  y' <- eval y
  printScript x' @?= printScript y'

equal' :: HasCallStack => ClosedTerm a -> String -> Assertion
equal' x y = do
  x' <- eval x
  printScript x' @?= y


fails :: HasCallStack => ClosedTerm a -> Assertion
fails x =
  case evaluateScript $ compile x of
    Left (Scripts.EvaluationError _ _) -> mempty
    Left (Scripts.EvaluationException _ _) -> mempty
    Left e -> assertFailure $ "Script is malformed: " <> show e
    Right (_, _, s) -> assertFailure $ "Script didn't err: " <> printScript s

expect :: HasCallStack => ClosedTerm PBool -> Assertion
expect = equal (pcon PTrue :: Term s PBool)

-- FIXME: Make the below impossible using run-time checks.
-- loop :: Term (PInteger :--> PInteger)
-- loop = plam $ \x -> loop # x
-- loopHoisted :: Term (PInteger :--> PInteger)
-- loopHoisted = phoistAcyclic $ plam $ \x -> loop # x

-- FIXME: Use property tests
tests :: TestTree
tests =
  testGroup
    "unit tests"
    [ plutarchTests
    , uplcTests
    ]

plutarchTests :: TestTree
plutarchTests =
  testGroup
    "plutarch tests"
    [ testCase "add1" $ (printTerm add1) @?= "(program 1.0.0 (\\i0 -> \\i0 -> addInteger (addInteger i2 i1) 1))"
    , testCase "add1Hoisted" $ (printTerm add1Hoisted) @?= "(program 1.0.0 ((\\i0 -> i1) (\\i0 -> \\i0 -> addInteger (addInteger i2 i1) 1)))"
    , testCase "example1" $ (printTerm example1) @?= "(program 1.0.0 ((\\i0 -> addInteger (i1 12 32) (i1 5 4)) (\\i0 -> \\i0 -> addInteger (addInteger i2 i1) 1)))"
    , testCase "example2" $ (printTerm example2) @?= "(program 1.0.0 (\\i0 -> i1 (\\i0 -> addInteger i1 1) (\\i0 -> subtractInteger i1 1)))"
    , testCase "pfix" $ (printTerm pfix) @?= "(program 1.0.0 ((\\i0 -> i1) (\\i0 -> (\\i0 -> i2 (\\i0 -> i2 i2 i1)) (\\i0 -> i2 (\\i0 -> i2 i2 i1)))))"
    , testCase "fib" $ (printTerm fib) @?= "(program 1.0.0 ((\\i0 -> (\\i0 -> (\\i0 -> i1) (i1 (\\i0 -> \\i0 -> force (i4 (equalsInteger i1 0) (delay 0) (delay (force (i4 (equalsInteger i1 1) (delay 1) (delay (addInteger (i2 (subtractInteger i1 1)) (i2 (subtractInteger i1 2))))))))))) (\\i0 -> (\\i0 -> i2 (\\i0 -> i2 i2 i1)) (\\i0 -> i2 (\\i0 -> i2 i2 i1)))) (force ifThenElse)))"
    , testCase "fib 9 == 34" $ equal (fib # 9) (34 :: Term s PInteger)
    , testCase "uglyDouble" $ (printTerm uglyDouble) @?= "(program 1.0.0 (\\i0 -> addInteger i1 i1))"
    , testCase "1 + 2 == 3" $ equal (1 + 2 :: Term s PInteger) (3 :: Term s PInteger)
    , testCase "fails: perror" $ fails perror
    , testCase "() == ()" $ expect $ pmatch (pcon PUnit) (\case PUnit -> pcon PTrue)
    , testCase "0x02af == 0x02af" $ expect $ phexByteStr "02af" #== phexByteStr "02af"
    , testCase "\"foo\" == \"foo\"" $ expect $ "foo" #== ("foo" :: Term s PString)
    , testCase "PByteString :: mempty <> a == a <> mempty == a" $ do
        expect $ let a = phexByteStr "152a" in (mempty <> a) #== a
        expect $ let a = phexByteStr "4141" in (a <> mempty) #== a
    , testCase "PString :: mempty <> a == a <> mempty == a" $ do
        expect $ let a = "foo" :: Term s PString in (mempty <> a) #== a
        expect $ let a = "bar" :: Term s PString in (a <> mempty) #== a
    , testCase "PByteString :: 0x12 <> 0x34 == 0x1234" $
        expect $
          (phexByteStr "12" <> phexByteStr "34") #== phexByteStr "1234"
    , testCase "PString :: \"ab\" <> \"cd\" == \"abcd\"" $
        expect $
          ("ab" <> "cd") #== ("abcd" :: Term s PString)
    , testCase "PByteString mempty" $ expect $ mempty #== phexByteStr ""
    , testCase "PString mempty" $ expect $ mempty #== ("" :: Term s PString)
    , testCase "pfromText \"abc\" `equal` \"abc\"" $ equal (pfromText "abc") ("abc" :: Term s PString)
    ]

uplcTests :: TestTree
uplcTests =
  testGroup
    "uplc tests"
    [ testCase "2:[1]" $
        let l :: Term _ (PBuiltinList PInteger) =
              punsafeConstant . PLC.Some $
                PLC.ValueOf (PLC.DefaultUniApply PLC.DefaultUniProtoList PLC.DefaultUniInteger) [1]
            l' :: Term _ (PBuiltinList PInteger) =
              (pforce $ punsafeBuiltin PLC.MkCons) # (2 :: Term _ PInteger) # l
         in equal' l' "(program 1.0.0 [2,1])"
    , testCase "[2,1]" $
        let l :: Term _ (PBuiltinList PInteger) =
              punsafeConstant . PLC.Some $
                PLC.ValueOf (PLC.DefaultUniApply PLC.DefaultUniProtoList PLC.DefaultUniInteger) [1]
            l' :: Term _ (PBuiltinList PInteger) =
              (pforce $ punsafeBuiltin PLC.MkCons) # (2 :: Term _ PInteger) # l
         in equal' l' "(program 1.0.0 [2,1])"
    , testCase "fails: True:[1]" $
        let l :: Term _ (PBuiltinList POpaque) =
              punsafeConstant . PLC.Some $
                PLC.ValueOf (PLC.DefaultUniApply PLC.DefaultUniProtoList PLC.DefaultUniInteger) [1]
            l' :: Term _ (PBuiltinList POpaque) =
              (pforce $ punsafeBuiltin PLC.MkCons) # (pcon PTrue) # l
         in fails l'
    , testCase "(2,1)" $
        let p :: Term _ (PBuiltinPair PInteger PInteger) =
              punsafeConstant . PLC.Some $
                PLC.ValueOf
                  ( PLC.DefaultUniApply
                      (PLC.DefaultUniApply PLC.DefaultUniProtoPair PLC.DefaultUniInteger)
                      PLC.DefaultUniInteger
                  )
                  (1, 2)
         in equal' p "(program 1.0.0 (1, 2))"
    , testCase "fails: MkPair 1 2" $
        let p :: Term _ (PBuiltinPair PInteger PInteger) =
              (punsafeBuiltin PLC.MkPairData) # (1 :: Term _ PInteger) # (2 :: Term _ PInteger)
         in fails p
    ]
