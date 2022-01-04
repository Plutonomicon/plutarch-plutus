{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Free (Free,iterM,liftF)
import Control.Exception (SomeException, try)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.Maybe (fromJust)
import Plutarch (ClosedTerm, POpaque, compile, compileAndShrink, popaque, printScript, printTerm, punsafeBuiltin, punsafeCoerce, punsafeConstant)
import Plutarch.Bool (PBool (PFalse, PTrue), pif, pnot, (#&&), (#<), (#<=), (#==), (#||))
import Plutarch.Builtin (PBuiltinList, PBuiltinPair, PData, pdata, pdataLiteral)
import Plutarch.ByteString (pbyteStr, pconsBS, phexByteStr, pindexBS, plengthBS, psliceBS)
import Plutarch.Either (PEither (PLeft, PRight))
import Plutarch.Evaluate (evaluateScript)
import Plutarch.Integer (PInteger)
import Plutarch.Prelude
import Plutarch.ScriptContext (PScriptPurpose (PMinting))
import Plutarch.String (PString, pfromText)
import Plutarch.Unit (PUnit (..))
import qualified Plutus.V1.Ledger.Scripts as Scripts
import Plutus.V1.Ledger.Value (CurrencySymbol (CurrencySymbol))
import Plutus.V2.Ledger.Contexts (ScriptPurpose (Minting))
import qualified PlutusCore as PLC
import qualified PlutusTx
import PlutusTx.IsData.Class (toData)
import Shrink.Testing.Tactics (Similar((~=)))

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

uglyDouble :: Term s (PInteger :--> PInteger)
uglyDouble = plam $ \n -> plet n $ \n1 -> plet n1 $ \n2 -> n2 + n2

evalBy :: HasCallStack => (ClosedTerm a -> Scripts.Script) -> ClosedTerm a -> IO Scripts.Script
evalBy compiler x = case evaluateScript $ compiler x of
  Left e -> assertFailure $ "Script evaluation failed: " <> show e
  Right (_, _, x') -> pure x'

eval :: HasCallStack => ClosedTerm a -> IO Scripts.Script
eval = evalBy compile

shrinkEval :: HasCallStack => ClosedTerm a -> IO Scripts.Script
shrinkEval = evalBy compileAndShrink

pEqual :: HasCallStack => ClosedTerm a -> ClosedTerm b -> Assertion
pEqual x y = do
  x' <- eval x
  y' <- eval y
  printScript x' @?= printScript y'

shrinkEqual :: HasCallStack => ClosedTerm a -> ClosedTerm b -> Assertion
shrinkEqual x y = do
  x' <- eval x
  y' <- eval y
  xs <- shrinkEval x
  ys <- shrinkEval y
  printScript x' @?= printScript xs
  printScript y' @?= printScript ys
  printScript xs @?= printScript ys

equal' :: HasCallStack => ClosedTerm a -> String -> Assertion
equal' x y = do
  x' <- eval x
  printScript x' @?= y

shrinkDoesntBreak :: HasCallStack => ClosedTerm a -> Assertion
shrinkDoesntBreak x = case (evaluateScript $ compile x,evaluateScript $ compileAndShrink x) of
                        (Left _,Left _) -> mempty
                        (Right (_,_,x'),Right (_,_,xs')) -> assertBool ("shrink broke: " ++ printScript (compile x)) $ x' ~= xs' 
                        _ -> assertFailure $ "shrink changed rather script fails"

failsBy :: HasCallStack => (ClosedTerm a -> Scripts.Script) -> ClosedTerm a -> Assertion
failsBy compiler x = case evaluateScript $ compiler x of
    Left (Scripts.EvaluationError _ _) -> mempty
    Left (Scripts.EvaluationException _ _) -> mempty
    Left e -> assertFailure $ "Script is malformed: " <> show e
    Right (_, _, s) -> assertFailure $ "Script didn't err: " <> printScript s

pFails :: HasCallStack => ClosedTerm a -> Assertion
pFails = failsBy compile

shrinkFails :: HasCallStack => ClosedTerm a -> Assertion
shrinkFails = failsBy compileAndShrink

expect :: HasCallStack => ClosedTerm PBool -> Assertion
expect = pEqual (pcon PTrue :: Term s PBool)

shrinkExpect :: HasCallStack => ClosedTerm PBool -> Assertion
shrinkExpect = shrinkEqual (pcon PTrue :: Term s PBool)

throwsBy :: (ClosedTerm a -> Scripts.Script) -> ClosedTerm a -> Assertion
throwsBy compiler x =
  try @SomeException (putStrLn $ printScript $ compiler x) >>= \case
    Right _ -> assertFailure "Supposed to throw"
    Left _ -> pure ()

pThrows :: ClosedTerm a -> Assertion
pThrows = throwsBy compile

shrinkThrows :: ClosedTerm a -> Assertion
shrinkThrows = throwsBy compileAndShrink

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
    , shrinkerTests
    ]

data UnitTest = UnitTest TestName (TestStructM ())

data TestStruct where
  PrintsTo :: ClosedTerm a ->  String ->  TestStruct
  Equal :: ClosedTerm a -> ClosedTerm a -> TestStruct
  IsTrue :: ClosedTerm PBool -> TestStruct
  Fails :: ClosedTerm a -> TestStruct
  Throws :: ClosedTerm a -> TestStruct

type TestStructF = (,) TestStruct
type TestStructM = Free TestStructF

printsTo :: ClosedTerm a ->  String -> TestStructM ()
printsTo t s = liftF (PrintsTo t s,())

equal :: ClosedTerm a -> ClosedTerm a -> TestStructM ()
equal l r = liftF (Equal l r,())

isTrue :: ClosedTerm PBool -> TestStructM ()
isTrue t = liftF (IsTrue t,())

fails :: ClosedTerm a -> TestStructM ()
fails t = liftF (Fails t,())

throws :: ClosedTerm a -> TestStructM ()
throws t = liftF (Throws t,())

interpWith :: (TestStruct -> Assertion) -> TestStructM () -> Assertion
interpWith interpreter = iterM (\(testStruct,r) -> interpreter testStruct >> r )

plutarchInterp :: TestStruct -> Assertion
plutarchInterp = \case
  PrintsTo term str -> printTerm term @?= str
  Equal l r -> pEqual l r
  IsTrue t -> expect t
  Fails t -> pFails t
  Throws t -> pThrows t

shrinkInterp :: TestStruct -> Assertion
shrinkInterp = \case
  PrintsTo term _ -> shrinkDoesntBreak term
  Equal l r -> shrinkEqual l r
  IsTrue t -> shrinkExpect t
  Fails t -> shrinkFails t
  Throws t -> shrinkThrows t

runUnitTestWith :: (TestStruct -> Assertion) -> UnitTest -> TestTree
runUnitTestWith interpreter (UnitTest name testStruct) = testCase name $ interpWith interpreter testStruct

shrinkerTests :: TestTree
shrinkerTests =
  testGroup "shrinker tests" $
    runUnitTestWith shrinkInterp <$> mainTests

plutarchTests :: TestTree
plutarchTests =
  testGroup "plutarch tests" $
    runUnitTestWith plutarchInterp <$> mainTests

mainTests :: [UnitTest]
mainTests = 
  [ UnitTest "add1" $ add1 `printsTo` "(program 1.0.0 (\\i0 -> \\i0 -> addInteger (addInteger i2 i1) 1))"
  , UnitTest "add1Hoisted" $ add1Hoisted `printsTo` "(program 1.0.0 (\\i0 -> \\i0 -> addInteger (addInteger i2 i1) 1))"
  , UnitTest "example1" $ example1 `printsTo` "(program 1.0.0 ((\\i0 -> addInteger (i1 12 32) (i1 5 4)) (\\i0 -> \\i0 -> addInteger (addInteger i2 i1) 1)))"
  , UnitTest "example2" $ example2 `printsTo` "(program 1.0.0 (\\i0 -> i1 (\\i0 -> addInteger i1 1) (\\i0 -> subtractInteger i1 1)))"
  , UnitTest "pfix" $ pfix `printsTo` "(program 1.0.0 (\\i0 -> (\\i0 -> i2 (\\i0 -> i2 i2 i1)) (\\i0 -> i2 (\\i0 -> i2 i2 i1))))"
  , UnitTest "fib" $ fib `printsTo` "(program 1.0.0 ((\\i0 -> (\\i0 -> (\\i0 -> i2 (\\i0 -> i2 i2 i1)) (\\i0 -> i2 (\\i0 -> i2 i2 i1))) (\\i0 -> \\i0 -> force (i3 (equalsInteger i1 0) (delay 0) (delay (force (i3 (equalsInteger i1 1) (delay 1) (delay (addInteger (i2 (subtractInteger i1 1)) (i2 (subtractInteger i1 2)))))))))) (force ifThenElse)))"
  , UnitTest "fib 9 == 34" $ equal (fib # 9) (34 :: Term s PInteger)
  , UnitTest "uglyDouble" $ uglyDouble `printsTo` "(program 1.0.0 (\\i0 -> addInteger i1 i1))"
  , UnitTest "1 + 2 == 3" $ equal (1 + 2 :: Term s PInteger) (3 :: Term s PInteger)
  , UnitTest "fails: perror" $ fails perror
  , UnitTest "pnot" $ do 
     (pnot #$ pcon PTrue) `equal` pcon PFalse
     (pnot #$ pcon PFalse) `equal` pcon PTrue
  , UnitTest "() == ()" $ do 
      isTrue $ pmatch (pcon PUnit) (\case PUnit -> pcon PTrue)
      isTrue $ pcon PUnit #== pcon PUnit
      pcon PUnit `equal` pcon PUnit
  , UnitTest "() < () == False" $ do
      isTrue $ pnot #$ pcon PUnit #< pcon PUnit
  , UnitTest "() <= () == True" $ do
      isTrue $ pcon PUnit #<= pcon PUnit
  , UnitTest "0x02af == 0x02af" $ isTrue $ phexByteStr "02af" #== phexByteStr "02af"
  , UnitTest "\"foo\" == \"foo\"" $ isTrue $ "foo" #== ("foo" :: Term s PString)
  , UnitTest "PByteString :: mempty <> a == a <> mempty == a" $ do 
      isTrue $ let a = phexByteStr "152a" in (mempty <> a) #== a
      isTrue $ let a = phexByteStr "4141" in (a <> mempty) #== a
  , UnitTest "PString :: mempty <> a == a <> mempty == a" $ do 
      isTrue $ let a = "foo" :: Term s PString in (mempty <> a) #== a
      isTrue $ let a = "bar" :: Term s PString in (a <> mempty) #== a
  , UnitTest "PByteString :: 0x12 <> 0x34 == 0x1234" $
      isTrue $
        (phexByteStr "12" <> phexByteStr "34") #== phexByteStr "1234"
  , UnitTest "PString :: \"ab\" <> \"cd\" == \"abcd\"" $
      isTrue $
        ("ab" <> "cd") #== ("abcd" :: Term s PString)
  , UnitTest "PByteString mempty" $ isTrue $ mempty #== phexByteStr ""
  , UnitTest "pconsByteStr" $ do
      let xs = "5B1F"; b = "41"
      (pconsBS # fromInteger (readByte b) # phexByteStr xs) `equal` phexByteStr (b <> xs)
  , UnitTest "plengthByteStr" $ do
    (plengthBS # phexByteStr "012f") `equal` (2 :: Term s PInteger)
    isTrue $ (plengthBS # phexByteStr "012f") #== 2
    let xs = phexByteStr "48fCd1" 
    (plengthBS #$ pconsBS # 91 # xs)
      `equal` (1 + plengthBS # xs)
  , UnitTest "pindexByteStr" $
      (pindexBS # phexByteStr "4102af" # 1) `equal` (0x02 :: Term s PInteger)
  , UnitTest "psliceByteStr" $
      (psliceBS # 1 # 3 # phexByteStr "4102afde5b2a") `equal` phexByteStr "02afde"
  , UnitTest "pbyteStr - phexByteStr relation" $ do
      let a = ["42", "ab", "df", "c9"]
      pbyteStr (BS.pack $ map readByte a) `equal` phexByteStr (concat a)
  , UnitTest "PString mempty" $ isTrue $ mempty #== ("" :: Term s PString)
  , UnitTest "pfromText \"abc\" == \"abc\"" $ do
        pfromText "abc" `equal` ("abc" :: Term s PString)
        isTrue $ pfromText "foo" #== "foo"
  , UnitTest "#&& - boolean and; #|| - boolean or" $ do
      let ptrue = pcon PTrue
          pfalse = pcon PFalse
      -- AND tests
      isTrue $ ptrue #&& ptrue
      isTrue $ pnot #$ ptrue #&& pfalse
      isTrue $ pnot #$ pfalse #&& ptrue
      isTrue $ pnot #$ pfalse #&& pfalse
      -- OR tests
      isTrue $ ptrue #|| ptrue
      isTrue $ ptrue #|| pfalse
      isTrue $ pfalse #|| ptrue
      isTrue $ pnot #$ pfalse #|| pfalse
  , UnitTest "ScriptPurpose literal" $
      let d :: ScriptPurpose
          d = Minting dummyCurrency
          f :: Term s PData
          f = pdataLiteral $ toData d
       in f `printsTo` "(program 1.0.0 #d8799f58201111111111111111111111111111111111111111111111111111111111111111ff)"
  , UnitTest "decode ScriptPurpose" $
      let d :: ScriptPurpose
          d = Minting dummyCurrency
          d' :: Term s PScriptPurpose
          d' = punsafeCoerce $ pdataLiteral $ toData d
          f :: Term s POpaque
          f = pmatch d' $ \case
            PMinting c -> popaque c
            _ -> perror
       in f `printsTo` "(program 1.0.0 ((\\i0 -> (\\i0 -> (\\i0 -> force (force ifThenElse (equalsInteger 0 i2) (delay i1) (delay error))) (force (force sndPair) i2)) (force (force fstPair) i1)) (unConstrData #d8799f58201111111111111111111111111111111111111111111111111111111111111111ff)))"
  , UnitTest "error # 1 => error" $
      (perror # (1 :: Term s PInteger)) `printsTo` "(program 1.0.0 error)"
  , UnitTest "fib error => error" $
      (fib # perror) `printsTo` "(program 1.0.0 error)"
  , UnitTest "force (delay 0) => 0" $
      (pforce . pdelay $ (0 :: Term s PInteger)) `printsTo` "(program 1.0.0 0)"
  , UnitTest "delay (force (delay 0)) => delay 0" $
      (pdelay . pforce . pdelay $ (0 :: Term s PInteger)) `printsTo` "(program 1.0.0 (delay 0))"
  , UnitTest "id # 0 => 0" $
      ((plam $ \x -> x) # (0 :: Term s PInteger)) `printsTo` "(program 1.0.0 0)"
  , UnitTest "hoist id 0 => 0" $
      ((phoistAcyclic $ plam $ \x -> x) # (0 :: Term s PInteger)) `printsTo` "(program 1.0.0 0)"
  , UnitTest "hoist fstPair => fstPair" $
      phoistAcyclic (punsafeBuiltin PLC.FstPair) `printsTo` "(program 1.0.0 fstPair)"
  , UnitTest "throws: hoist error" $ throws $ phoistAcyclic perror
  , UnitTest "PData equality" $ do
      isTrue $ let dat = pdataLiteral (PlutusTx.List [PlutusTx.Constr 1 [PlutusTx.I 0]]) in dat #== dat
      isTrue $ pnot #$ pdataLiteral (PlutusTx.Constr 0 []) #== pdataLiteral (PlutusTx.I 42)
  , UnitTest "PAsData equality" $ do
      isTrue $ let dat = pdata @PInteger 42 in dat #== dat
      isTrue $ pnot #$ pdata (phexByteStr "12") #== pdata (phexByteStr "ab")
  , UnitTest "λx y -> addInteger x y => addInteger" $
      (plam $ \x y -> (x :: Term _ PInteger) + y) `printsTo` "(program 1.0.0 addInteger)"
  , UnitTest "λx y -> hoist (force mkCons) x y => force mkCons" $
      (plam $ \x y -> (pforce $ punsafeBuiltin PLC.MkCons) # x # y) `printsTo` "(program 1.0.0 (force mkCons))"
  , UnitTest "λx y -> hoist mkCons x y => mkCons x y" $
      (plam $ \x y -> (punsafeBuiltin PLC.MkCons) # x # y) `printsTo` "(program 1.0.0 (\\i0 -> \\i0 -> mkCons i2 i1))"
  , UnitTest "λx y -> hoist (λx y. x + y - y - x) x y => λx y. x + y - y - x" $
      (plam $ \x y -> (phoistAcyclic $ plam $ \(x :: Term _ PInteger) y -> x + y - y - x) # x # y) `printsTo` "(program 1.0.0 (\\i0 -> \\i0 -> subtractInteger (subtractInteger (addInteger i2 i1) i1) i2))"
  , UnitTest "λx y -> x + x" $
      (plam $ \(x :: Term _ PInteger) (_ :: Term _ PInteger) -> x + x) `printsTo` "(program 1.0.0 (\\i0 -> \\i0 -> addInteger i2 i2))"
  , UnitTest "let x = addInteger in x 1 1" $
      (plet (punsafeBuiltin PLC.AddInteger) $ \x -> x # (1 :: Term _ PInteger) # (1 :: Term _ PInteger)) `printsTo` "(program 1.0.0 (addInteger 1 1))"
  , UnitTest "let x = 0 in x => 0" $
      (plet 0 $ \(x :: Term _ PInteger) -> x) `printsTo` "(program 1.0.0 0)"
  , UnitTest "let x = hoist (\\x -> x + x) in 0 => 0" $
      (plet (phoistAcyclic $ plam $ \(x :: Term _ PInteger) -> x + x) $ \_ -> (0 :: Term _ PInteger)) `printsTo` "(program 1.0.0 0)"
  , UnitTest "let x = hoist (\\x -> x + x) in x" $
      (plet (phoistAcyclic $ plam $ \(x :: Term _ PInteger) -> x + x) $ \x -> x) `printsTo` "(program 1.0.0 (\\i0 -> addInteger i1 i1))"
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
              pforce (punsafeBuiltin PLC.MkCons) # (2 :: Term _ PInteger) # l
         in equal' l' "(program 1.0.0 [2,1])"
    , testCase "[2,1]" $
        let l :: Term _ (PBuiltinList PInteger) =
              punsafeConstant . PLC.Some $
                PLC.ValueOf (PLC.DefaultUniApply PLC.DefaultUniProtoList PLC.DefaultUniInteger) [1]
            l' :: Term _ (PBuiltinList PInteger) =
              pforce (punsafeBuiltin PLC.MkCons) # (2 :: Term _ PInteger) # l
         in equal' l' "(program 1.0.0 [2,1])"
    , testCase "fails: True:[1]" $
        let l :: Term _ (PBuiltinList POpaque) =
              punsafeConstant . PLC.Some $
                PLC.ValueOf (PLC.DefaultUniApply PLC.DefaultUniProtoList PLC.DefaultUniInteger) [1]
            l' :: Term _ (PBuiltinList POpaque) =
              pforce (punsafeBuiltin PLC.MkCons) # pcon PTrue # l
         in pFails l'
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
              punsafeBuiltin PLC.MkPairData # (1 :: Term _ PInteger) # (2 :: Term _ PInteger)
         in pFails p
    ]

{- | Interpret a byte.

>>> readByte "41"
65
-}
readByte :: Num a => String -> a
readByte a = fromInteger $ read $ "0x" <> a

dummyCurrency :: CurrencySymbol
dummyCurrency =
  CurrencySymbol . fromJust . Aeson.decode $
    "\"1111111111111111111111111111111111111111111111111111111111111111\""
