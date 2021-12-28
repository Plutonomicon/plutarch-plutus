{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Exception (SomeException, try)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.Maybe (fromJust)
import Plutarch (ClosedTerm, POpaque, compile, popaque, printScript, printTerm, punsafeBuiltin, punsafeCoerce, punsafeConstant)
import Plutarch.Bool (PBool (PFalse, PTrue), pif, pnot, (#&&), (#<), (#<=), (#==), (#||))
import Plutarch.Builtin (PBuiltinList, PBuiltinPair, PData, pdataLiteral)
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
import PlutusTx.IsData.Class (toData)

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

eval :: HasCallStack => ClosedTerm a -> IO Scripts.Script
eval x = case evaluateScript $ compile x of
  Left e -> assertFailure $ "Script evaluation failed: " <> show e
  Right (_, _, x') -> pure x'

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

throws :: ClosedTerm a -> Assertion
throws x =
  try @SomeException (putStrLn $ printScript $ compile x) >>= \case
    Right _ -> assertFailure "Supposed to throw"
    Left _ -> pure ()

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
    , testCase "add1Hoisted" $ (printTerm add1Hoisted) @?= "(program 1.0.0 (\\i0 -> \\i0 -> addInteger (addInteger i2 i1) 1))"
    , testCase "example1" $ (printTerm example1) @?= "(program 1.0.0 ((\\i0 -> addInteger (i1 12 32) (i1 5 4)) (\\i0 -> \\i0 -> addInteger (addInteger i2 i1) 1)))"
    , testCase "example2" $ (printTerm example2) @?= "(program 1.0.0 (\\i0 -> i1 (\\i0 -> addInteger i1 1) (\\i0 -> subtractInteger i1 1)))"
    , testCase "pfix" $ (printTerm pfix) @?= "(program 1.0.0 (\\i0 -> (\\i0 -> i2 (\\i0 -> i2 i2 i1)) (\\i0 -> i2 (\\i0 -> i2 i2 i1))))"
    , testCase "fib" $ (printTerm fib) @?= "(program 1.0.0 ((\\i0 -> (\\i0 -> (\\i0 -> i2 (\\i0 -> i2 i2 i1)) (\\i0 -> i2 (\\i0 -> i2 i2 i1))) (\\i0 -> \\i0 -> force (i3 (equalsInteger i1 0) (delay 0) (delay (force (i3 (equalsInteger i1 1) (delay 1) (delay (addInteger (i2 (subtractInteger i1 1)) (i2 (subtractInteger i1 2)))))))))) (force ifThenElse)))"
    , testCase "fib 9 == 34" $ equal (fib # 9) (34 :: Term s PInteger)
    , testCase "uglyDouble" $ (printTerm uglyDouble) @?= "(program 1.0.0 (\\i0 -> addInteger i1 i1))"
    , testCase "1 + 2 == 3" $ equal (1 + 2 :: Term s PInteger) (3 :: Term s PInteger)
    , testCase "fails: perror" $ fails perror
    , testCase "pnot" $ do
        (pnot #$ pcon PTrue) `equal` pcon PFalse
        (pnot #$ pcon PFalse) `equal` pcon PTrue
    , testCase "() == ()" $ do
        expect $ pmatch (pcon PUnit) (\case PUnit -> pcon PTrue)
        expect $ pcon PUnit #== pcon PUnit
        pcon PUnit `equal` pcon PUnit
    , testCase "() < () == False" $ do
        expect $ pnot #$ pcon PUnit #< pcon PUnit
    , testCase "() <= () == True" $ do
        expect $ pcon PUnit #<= pcon PUnit
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
    , testCase "pconsByteStr" $
        let xs = "5B1F"; b = "41"
         in (pconsBS # fromInteger (readByte b) # phexByteStr xs) `equal` phexByteStr (b <> xs)
    , testCase "plengthByteStr" $ do
        (plengthBS # phexByteStr "012f") `equal` (2 :: Term s PInteger)
        expect $ (plengthBS # phexByteStr "012f") #== 2
        let xs = phexByteStr "48fCd1"
        (plengthBS #$ pconsBS # 91 # xs)
          `equal` (1 + plengthBS # xs)
    , testCase "pindexByteStr" $
        (pindexBS # phexByteStr "4102af" # 1) `equal` (0x02 :: Term s PInteger)
    , testCase "psliceByteStr" $
        (psliceBS # 1 # 3 # phexByteStr "4102afde5b2a") `equal` phexByteStr "02afde"
    , testCase "pbyteStr - phexByteStr relation" $ do
        let a = ["42", "ab", "df", "c9"]
        pbyteStr (BS.pack $ map readByte a) `equal` phexByteStr (concat a)
    , testCase "PString mempty" $ expect $ mempty #== ("" :: Term s PString)
    , testCase "pfromText \"abc\" == \"abc\"" $ do
        pfromText "abc" `equal` ("abc" :: Term s PString)
        expect $ pfromText "foo" #== "foo"
    , testCase "#&& - boolean and; #|| - boolean or" $ do
        let ptrue = pcon PTrue
            pfalse = pcon PFalse
        -- AND tests
        expect $ ptrue #&& ptrue
        expect $ pnot #$ ptrue #&& pfalse
        expect $ pnot #$ pfalse #&& ptrue
        expect $ pnot #$ pfalse #&& pfalse
        -- OR tests
        expect $ ptrue #|| ptrue
        expect $ ptrue #|| pfalse
        expect $ pfalse #|| ptrue
        expect $ pnot #$ pfalse #|| pfalse
    , testCase "ScriptPurpose literal" $
        let d :: ScriptPurpose
            d = Minting dummyCurrency
            f :: Term s PData
            f = pdataLiteral $ toData d
         in printTerm f @?= "(program 1.0.0 #d8799f58201111111111111111111111111111111111111111111111111111111111111111ff)"
    , testCase "decode ScriptPurpose" $
        let d :: ScriptPurpose
            d = Minting dummyCurrency
            d' :: Term s PScriptPurpose
            d' = punsafeCoerce $ pdataLiteral $ toData d
            f :: Term s POpaque
            f = pmatch d' $ \case
              PMinting c -> popaque c
              _ -> perror
         in printTerm f @?= "(program 1.0.0 ((\\i0 -> (\\i0 -> (\\i0 -> force (force ifThenElse (equalsInteger 0 i2) (delay i1) (delay error))) (force (force sndPair) i2)) (force (force fstPair) i1)) (unConstrData #d8799f58201111111111111111111111111111111111111111111111111111111111111111ff)))"
    , testCase "error # 1 => error" $
        printTerm (perror # (1 :: Term s PInteger)) @?= "(program 1.0.0 error)"
    , testCase "fib error => error" $
        printTerm (fib # perror) @?= "(program 1.0.0 error)"
    , testCase "force (delay 0) => 0" $
        printTerm (pforce . pdelay $ (0 :: Term s PInteger)) @?= "(program 1.0.0 0)"
    , testCase "delay (force (delay 0)) => delay 0" $
        printTerm (pdelay . pforce . pdelay $ (0 :: Term s PInteger)) @?= "(program 1.0.0 (delay 0))"
    , testCase "id # 0 => 0" $
        printTerm ((plam $ \x -> x) # (0 :: Term s PInteger)) @?= "(program 1.0.0 0)"
    , testCase "hoist id 0 => 0" $
        printTerm ((phoistAcyclic $ plam $ \x -> x) # (0 :: Term s PInteger)) @?= "(program 1.0.0 0)"
    , testCase "hoist fstPair => fstPair" $
        printTerm (phoistAcyclic (punsafeBuiltin PLC.FstPair)) @?= "(program 1.0.0 fstPair)"
    , testCase "throws: hoist error" $ throws $ phoistAcyclic perror
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
              punsafeBuiltin PLC.MkPairData # (1 :: Term _ PInteger) # (2 :: Term _ PInteger)
         in fails p
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
