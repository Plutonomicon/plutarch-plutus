{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.Maybe (fromJust)
import qualified Examples.List as List
import Examples.Tracing (traceTests)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Plutarch (POpaque, popaque, printTerm)
import Plutarch.Api.V1 (PScriptPurpose (PMinting))
import Plutarch.Bool (pand, por)
import Plutarch.Internal (punsafeConstantInternal)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeBuiltin)
import Plutus.V1.Ledger.Value (CurrencySymbol (CurrencySymbol))
import Plutus.V2.Ledger.Contexts (ScriptPurpose (Minting))
import qualified PlutusCore as PLC
import qualified PlutusTx

import qualified Examples.Api as Api
import qualified Examples.ConstrData as ConstrData
import qualified Examples.Field as Field
import qualified Examples.LetRec as LetRec
import qualified Examples.Lift as Lift
import qualified Examples.PIsData as PIsData
import qualified Examples.PlutusType as PlutusType
import qualified Examples.Rationals as Rationals
import qualified Examples.Recursion as Recursion
import qualified Examples.Scripts as Scripts
import Utils

import Data.Text (Text)

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain $ testGroup "all tests" [standardTests] -- , shrinkTests ]

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

-- FIXME: Make the below impossible using run-time checks.
-- loop :: Term (PInteger :--> PInteger)
-- loop = plam $ \x -> loop # x
-- loopHoisted :: Term (PInteger :--> PInteger)
-- loopHoisted = phoistAcyclic $ plam $ \x -> loop # x

-- _shrinkTests :: TestTree
-- _shrinkTests = testGroup "shrink tests" [let ?tester = shrinkTester in tests]

standardTests :: TestTree
standardTests = testGroup "standard tests" [let ?tester = standardTester in tests]

tests :: HasTester => TestTree
tests =
  testGroup
    "unit tests"
    [ plutarchTests
    , uplcTests
    , PlutusType.tests
    , Recursion.tests
    , Api.tests
    , List.tests
    , Rationals.tests
    , LetRec.tests
    , PIsData.tests
    , Field.tests
    , ConstrData.tests
    , Lift.tests
    , Scripts.tests
    ]

plutarchTests :: HasTester => TestTree
plutarchTests =
  testGroup
    "plutarch tests"
    [ testCase "add1" $ (printTerm add1) @?= "(program 1.0.0 (\\i0 -> \\i0 -> addInteger (addInteger i2 i1) 1))"
    , testCase "add1Hoisted" $ (printTerm add1Hoisted) @?= "(program 1.0.0 (\\i0 -> \\i0 -> addInteger (addInteger i2 i1) 1))"
    , testCase "example1" $ (printTerm example1) @?= "(program 1.0.0 ((\\i0 -> addInteger (i1 12 32) (i1 5 4)) (\\i0 -> \\i0 -> addInteger (addInteger i2 i1) 1)))"
    , testCase "example2" $ (printTerm example2) @?= "(program 1.0.0 (\\i0 -> i1 (\\i0 -> addInteger i1 1) (\\i0 -> subtractInteger i1 1)))"
    , testCase "pfix" $ (printTerm pfix) @?= "(program 1.0.0 (\\i0 -> (\\i0 -> i2 (\\i0 -> i2 i2 i1)) (\\i0 -> i2 (\\i0 -> i2 i2 i1))))"
    , testCase "fib" $ (printTerm fib) @?= "(program 1.0.0 ((\\i0 -> (\\i0 -> (\\i0 -> i2 (\\i0 -> i2 i2 i1)) (\\i0 -> i2 (\\i0 -> i2 i2 i1))) (\\i0 -> \\i0 -> force (i3 (equalsInteger i1 0) (delay 0) (delay (force (i3 (equalsInteger i1 1) (delay 1) (delay (addInteger (i2 (subtractInteger i1 1)) (i2 (subtractInteger i1 2)))))))))) (force ifThenElse)))"
    , testCase "fib 9 == 34" $ equal (fib # 9) (pconstant @PInteger 34)
    , testCase "uglyDouble" $ (printTerm uglyDouble) @?= "(program 1.0.0 (\\i0 -> addInteger i1 i1))"
    , testCase "1 + 2 == 3" $ equal (pconstant @PInteger $ 1 + 2) (pconstant @PInteger 3)
    , testCase "fails: perror" $ fails perror
    , testGroup
        "PlutusType scott encoding "
        [ testCase "PMaybe" $ do
            let a = 42 :: Term s PInteger
            let x = pmatch (pcon $ PJust a) $ \case
                  PJust x -> x
                  -- We expect this perror not to be evaluated eagerly when mx
                  -- is a PJust.
                  PNothing -> perror
            printTerm x @?= "(program 1.0.0 ((\\i0 -> \\i0 -> i2 42) (\\i0 -> i1) (delay error)))"
        , testCase "PPair" $ do
            let a = 42 :: Term s PInteger
                b = "Universe" :: Term s PString
            let x = pmatch (pcon (PPair a b) :: Term s (PPair PInteger PString)) $ \(PPair _ y) -> y
            printTerm x @?= "(program 1.0.0 ((\\i0 -> i1 42 \"Universe\") (\\i0 -> \\i0 -> i1)))"
        ]
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
    , testCase "\"foo\" == \"foo\"" $ expect $ "foo" #== pconstant @PString "foo"
    , testCase "PByteString :: mempty <> a == a <> mempty == a" $ do
        expect $ let a = phexByteStr "152a" in (mempty <> a) #== a
        expect $ let a = phexByteStr "4141" in (a <> mempty) #== a
    , testCase "PString :: mempty <> a == a <> mempty == a" $ do
        expect $ let a = pconstant @PString "foo" in (mempty <> a) #== a
        expect $ let a = pconstant @PString "bar" in (a <> mempty) #== a
    , testCase "PByteString :: 0x12 <> 0x34 == 0x1234" $
        expect $
          (phexByteStr "12" <> phexByteStr "34") #== phexByteStr "1234"
    , testCase "PString :: \"ab\" <> \"cd\" == \"abcd\"" $
        expect $
          ("ab" <> "cd") #== (pconstant @PString "abcd")
    , testCase "PByteString mempty" $ expect $ mempty #== phexByteStr ""
    , testCase "pconsByteStr" $
        let xs = "5B1F"; b = "41"
         in (pconsBS # fromInteger (readByte b) # phexByteStr xs) `equal` phexByteStr (b <> xs)
    , testCase "plengthByteStr" $ do
        (plengthBS # phexByteStr "012f") `equal` pconstant @PInteger 2
        expect $ (plengthBS # phexByteStr "012f") #== 2
        let xs = phexByteStr "48fCd1"
        (plengthBS #$ pconsBS # 91 # xs)
          `equal` (1 + plengthBS # xs)
    , testCase "pindexByteStr" $
        (pindexBS # phexByteStr "4102af" # 1) `equal` pconstant @PInteger 0x02
    , testCase "psliceByteStr" $
        (psliceBS # 2 # 3 # phexByteStr "4102afde5b2a") `equal` phexByteStr "afde5b"
    , testCase "pconstant - phexByteStr relation" $ do
        let a = ["42", "ab", "df", "c9"]
        pconstant @PByteString (BS.pack $ map readByte a) `equal` phexByteStr (concat a)
    , testCase "PString mempty" $ expect $ mempty #== pconstant @PString ""
    , testCase "pconstant \"abc\" == \"abc\"" $ do
        pconstant @PString "abc" `equal` pconstant @PString "abc"
        expect $ pconstant @PString "foo" #== "foo"
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
            f :: Term s PScriptPurpose
            f = pconstant @PScriptPurpose d
         in printTerm f @?= "(program 1.0.0 #d8799f58201111111111111111111111111111111111111111111111111111111111111111ff)"
    , testCase "decode ScriptPurpose" $
        let d :: ScriptPurpose
            d = Minting dummyCurrency
            d' :: Term s PScriptPurpose
            d' = pconstant @PScriptPurpose d
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
    , testCase "PData equality" $ do
        expect $ let dat = pconstant @PData (PlutusTx.List [PlutusTx.Constr 1 [PlutusTx.I 0]]) in dat #== dat
        expect $ pnot #$ pconstant @PData (PlutusTx.Constr 0 []) #== pconstant @PData (PlutusTx.I 42)
    , testCase "PAsData equality" $ do
        expect $ let dat = pdata @PInteger 42 in dat #== dat
        expect $ pnot #$ pdata (phexByteStr "12") #== pdata (phexByteStr "ab")
    , testCase "Tracing" $ traceTests
    , testGroup
        "η-reduction optimisations"
        [ testCase "λx y. addInteger x y => addInteger" $
            printTerm (plam $ \x y -> (x :: Term _ PInteger) + y) @?= "(program 1.0.0 addInteger)"
        , testCase "λx y. hoist (force mkCons) x y => force mkCons" $
            printTerm (plam $ \x y -> (pforce $ punsafeBuiltin PLC.MkCons) # x # y) @?= "(program 1.0.0 (force mkCons))"
        , testCase "λx y. hoist mkCons x y => mkCons x y" $
            printTerm (plam $ \x y -> (punsafeBuiltin PLC.MkCons) # x # y) @?= "(program 1.0.0 (\\i0 -> \\i0 -> mkCons i2 i1))"
        , testCase "λx y. hoist (λx y. x + y - y - x) x y => λx y. x + y - y - x" $
            printTerm (plam $ \x y -> (phoistAcyclic $ plam $ \(x :: Term _ PInteger) y -> x + y - y - x) # x # y) @?= "(program 1.0.0 (\\i0 -> \\i0 -> subtractInteger (subtractInteger (addInteger i2 i1) i1) i2))"
        , testCase "λx y. x + x" $
            printTerm (plam $ \(x :: Term _ PInteger) (_ :: Term _ PInteger) -> x + x) @?= "(program 1.0.0 (\\i0 -> \\i0 -> addInteger i2 i2))"
        , testCase "let x = addInteger in x 1 1" $
            printTerm (plet (punsafeBuiltin PLC.AddInteger) $ \x -> x # (1 :: Term _ PInteger) # (1 :: Term _ PInteger)) @?= "(program 1.0.0 (addInteger 1 1))"
        , testCase "let x = 0 in x => 0" $
            printTerm (plet 0 $ \(x :: Term _ PInteger) -> x) @?= "(program 1.0.0 0)"
        , testCase "let x = hoist (λx. x + x) in 0 => 0" $
            printTerm (plet (phoistAcyclic $ plam $ \(x :: Term _ PInteger) -> x + x) $ \_ -> (0 :: Term _ PInteger)) @?= "(program 1.0.0 0)"
        , testCase "let x = hoist (λx. x + x) in x" $
            printTerm (plet (phoistAcyclic $ plam $ \(x :: Term _ PInteger) -> x + x) $ \x -> x) @?= "(program 1.0.0 (\\i0 -> addInteger i1 i1))"
        , testCase "λx y. sha2_256 x y =>!" $
            printTerm ((plam $ \x y -> punsafeBuiltin PLC.Sha2_256 # x # y)) @?= "(program 1.0.0 (\\i0 -> \\i0 -> sha2_256 i2 i1))"
        , testCase "let f = hoist (λx. x) in λx y. f x y => λx y. x y" $
            printTerm ((plam $ \x y -> (phoistAcyclic $ plam $ \x -> x) # x # y)) @?= "(program 1.0.0 (\\i0 -> \\i0 -> i2 i1))"
        , testCase "let f = hoist (λx. x True) in λx y. f x y => λx y. (λz. z True) x y" $
            printTerm ((plam $ \x y -> ((phoistAcyclic $ plam $ \x -> x # pcon PTrue)) # x # y)) @?= "(program 1.0.0 (\\i0 -> \\i0 -> i2 True i1))"
        , testCase "λy. (λx. x + x) y" $
            printTerm (plam $ \y -> (plam $ \(x :: Term _ PInteger) -> x + x) # y) @?= "(program 1.0.0 (\\i0 -> addInteger i1 i1))"
        ]
    , testGroup
        "Lifting of constants"
        [ testCase "plift on primitive types" $ do
            plift (pcon PTrue) @?= True
            plift (pcon PFalse) @?= False
        , testCase "pconstant on primitive types" $ do
            plift (pconstant @PBool False) @?= False
            plift (pconstant @PBool True) @?= True
        , testCase "plift on list and pair" $ do
            plift (pconstant ([1, 2, 3] :: [Integer])) @?= [1, 2, 3]
            plift (pconstant ("IOHK" :: Text, 42 :: Integer)) @?= ("IOHK", 42)
        , testCase "plift on data" $ do
            let d :: PlutusTx.Data
                d = PlutusTx.toData @(Either Bool Bool) $ Right False
            plift (pconstant d) @?= d
        , testCase "plift on nested containers" $ do
            -- List of pairs
            let v1 = [("IOHK", 42), ("Plutus", 31)] :: [(Text, Integer)]
            plift (pconstant v1) @?= v1
            -- List of pair of lists
            let v2 = [("IOHK", [1, 2, 3]), ("Plutus", [9, 8, 7])] :: [(Text, [Integer])]
            plift (pconstant v2) @?= v2
        ]
    , testGroup
        "Boolean operations"
        [ testCase "True && False ≡ False" $ equal (pcon PTrue #&& pcon PFalse) (pcon PFalse)
        , testCase "False && True ≡ False" $ equal (pcon PFalse #&& pcon PTrue) (pcon PFalse)
        , testCase "False && perror ≡ False" $ equal (pcon PFalse #&& perror) (pcon PFalse)
        , testCase "fails: pand False perror" $ fails $ pand # pcon PFalse # perror
        , testCase "pand False (pdelay perror) ≡ False" $ equal (pand # pcon PFalse # pdelay perror) (pdelay $ pcon PFalse)
        , testCase "True || False ≡ True" $ equal (pcon PTrue #|| pcon PFalse) (pcon PTrue)
        , testCase "False || True ≡ True" $ equal (pcon PFalse #|| pcon PTrue) (pcon PTrue)
        , testCase "True || perror ≡ True" $ equal (pcon PTrue #|| perror) (pcon PTrue)
        , testCase "fails: por True perror" $ fails $ por # pcon PFalse # perror
        , testCase "por True (pdelay perror) ≡ True" $ equal (por # pcon PTrue # pdelay perror) (pdelay $ pcon PTrue)
        ]
    , testGroup
        "plam"
        [ testCase "flip const" $ printTerm (plam $ \_ y -> y) @?= "(program 1.0.0 (\\i0 -> \\i0 -> i1))"
        , testCase "id" $ printTerm (plam $ \x -> x) @?= "(program 1.0.0 (\\i0 -> i1))"
        , testCase "plet" $ printTerm (plam $ \x _ -> plet x $ \_ -> perror) @?= "(program 1.0.0 (\\i0 -> \\i0 -> error))"
        ]
    ]

-- | Tests for the behaviour of UPLC itself.
uplcTests :: HasTester => TestTree
uplcTests =
  testGroup
    "uplc tests"
    [ testCase "2:[1]" $
        let l :: Term _ (PBuiltinList PInteger) =
              punsafeConstantInternal . PLC.Some $
                PLC.ValueOf (PLC.DefaultUniApply PLC.DefaultUniProtoList PLC.DefaultUniInteger) [1]
            l' :: Term _ (PBuiltinList PInteger) =
              pforce (punsafeBuiltin PLC.MkCons) # (2 :: Term _ PInteger) # l
         in equal' l' "(program 1.0.0 [2,1])"
    , testCase "fails: True:[1]" $
        let l :: Term _ (PBuiltinList POpaque) =
              punsafeConstantInternal . PLC.Some $
                PLC.ValueOf (PLC.DefaultUniApply PLC.DefaultUniProtoList PLC.DefaultUniInteger) [1]
            l' :: Term _ (PBuiltinList POpaque) =
              pforce (punsafeBuiltin PLC.MkCons) # pcon PTrue # l
         in fails l'
    , testCase "(2,1)" $
        let p :: Term _ (PBuiltinPair PInteger PInteger) =
              punsafeConstantInternal . PLC.Some $
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
