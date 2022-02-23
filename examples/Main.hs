{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Plutarch (POpaque, popaque, printTerm)
import Plutarch.Api.V1 (PScriptPurpose (PMinting))
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeBuiltin)
import Plutus.V1.Ledger.Value (CurrencySymbol, currencySymbol)
import Plutus.V2.Ledger.Contexts (ScriptPurpose (Minting))
import qualified PlutusCore as PLC
import qualified PlutusTx

import Utils

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain $ testGroup "all tests" [standardTests] -- , shrinkTests ]

standardTests :: TestTree
standardTests = testGroup "standard tests" [let ?tester = standardTester in tests]

tests :: HasTester => TestTree
tests =
  testGroup
    "unit tests"
    [ plutarchTests
    ]

plutarchTests :: HasTester => TestTree
plutarchTests =
  testGroup
    "plutarch tests"
    [ testCase "1 + 2 == 3" $ equal (pconstant @PInteger $ 1 + 2) (pconstant @PInteger 3)
    , testCase "fails: perror" $ fails perror
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
    , -- TODO: Port this to pluatrch-test
      -- , testCase "fib error => error" $
      --    printTerm (fib # perror) @?= "(program 1.0.0 error)"
      testCase "force (delay 0) => 0" $
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
    ]

dummyCurrency :: CurrencySymbol
dummyCurrency = currencySymbol "\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11\x11"
