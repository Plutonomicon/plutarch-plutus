{-# LANGUAGE OverloadedLists #-}

module Main (main) where

import Control.Monad.Except (runExceptT)
import Control.Monad.RWS.CPS (runRWS)
import Data.Kind (Type)
import Data.Vector.NonEmpty qualified as NEVector
import Plutarch.Backend.ANF (
  ANF (ANF),
  ANFBind (ANFLam),
  analyzeDemand,
  fromHashedAST,
 )
import Plutarch.Backend.AST (
  fromRawTerm,
 )
import Plutarch.Backend.Compile (toUPLCTerm)
import Plutarch.Backend.RawTerm (RawTerm (RLamAbs))
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (
  Term (asRawTerm),
  TermEnv (TermEnv),
  TermError,
  papp,
  pcompiled,
  pcompose,
  pdelay,
  perror,
  pforce,
  plam',
  punsafeCase,
  punsafeConstant,
  punsafeConstr,
  toSomeTerm,
 )
import Plutarch.Backend.UPLC (UPLCTerm (UPLCTerm))
import Plutarch.Backend.VarMap (VarMap, vmEmpty)
import Plutarch.Primitive.Bool (PBool, pif, pnot, por)
import Plutarch.Primitive.Function ((:-->))
import Plutarch.Primitive.List (PBList)
import Plutarch.Primitive.Numeric (
  PInteger,
  paddInteger,
  pmultiplyInteger,
  psubtractInteger,
 )
import Plutarch.Primitive.Pair
import PlutusCore qualified as PLC
import PlutusCore.Pretty (prettyPlcReadable)
import Prettyprinter (
  Pretty (pretty),
  defaultLayoutOptions,
  layoutSmart,
 )
import Prettyprinter.Render.String (renderString)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (
  assertBool,
  assertEqual,
  assertFailure,
  testCaseSteps,
 )
import Text.Show.Pretty (ppShow)

main :: IO ()
main =
  defaultMain . testGroup "Term" $
    [ testCaseSteps "Case 1" $ \step -> do
        step "Case: \\x -> (\\y -> y) ((\\z -> z) x)"
        step "1. Does Case 1 compile?"
        let compiled = compileTerm case1
        case compiled of
          Left err -> assertFailure $ "Compile error: " <> show err
          Right (vm, t) -> do
            step "Successfully compiled!"
            step "2. Is our VarMap empty?"
            assertBool "VarMap is not empty" (vm == vmEmpty)
            step "VarMap is empty!"
            step "3. Is the top node RLamAbs?"
            case t of
              RLamAbs {} -> do
                step $ "Top node: \n" <> ppShow t
                step "Converting to AST"
                let asAST = fromRawTerm t
                step $ "AST: \n" <> ppShow asAST
                step "Converting to ANF"
                let anf = fromHashedAST asAST
                step $ toPrettyString anf
                step "Demand analysis"
                let anf' = analyzeDemand anf
                step $ toPrettyString anf'
                step "Converting to UPLC"
                let (UPLCTerm t) = toUPLCTerm anf'
                step $ "UPLC:\n" <> (renderString . layoutSmart defaultLayoutOptions . prettyPlcReadable $ t)
                pure ()
              _ -> assertFailure $ "Unexpected top node: \n" <> ppShow t
    , testCaseSteps "Case 2" $ \step -> do
        step "Case: \\x -> force (delay x)"
        step "1. Does Case 2 compile?"
        let compiled = compileTerm case2
        case compiled of
          Left err -> assertFailure $ "Compile error: " <> show err
          Right (_, t) -> do
            step "Successfully compiled!"
            let asAST = fromRawTerm t
            step $ "AST: \n" <> ppShow asAST
            let anf@(ANF _ binds) = fromHashedAST asAST
            step $ toPrettyString anf
            step "2. Is there one bind exactly?"
            assertBool "Too many binds" (NEVector.length binds == 1)
            step "Exactly one bind!"
            step "3. Is that bind an ANFLam?"
            let soleBind = binds NEVector.! 0
            case soleBind of
              ANFLam {} -> step "The bind is ANFLam!"
              _ -> assertFailure $ "Unexpected bind: " <> ppShow soleBind
    , testCaseSteps "Case 3" $ \step -> do
        step "Case: \\x y -> or (not x) y"
        step "1. Does Case 3 compile?"
        let compiled = compileTerm case3
        case compiled of
          Left err -> assertFailure $ "Compile error: " <> show err
          Right (_, t) -> do
            step "Successfully compiled!"
            let asAST = fromRawTerm t
            step $ "AST:\n" <> ppShow asAST
            let anf@(ANF _ binds) = fromHashedAST asAST
            step $ toPrettyString anf
            step "2. Are there 5 binds?"
            let len = NEVector.length binds
            assertBool ("Too many binds: " <> show len) (len == 5)
            step "5 binds exactly!"
            step "Demand analysis"
            let anf' = analyzeDemand anf
            step $ toPrettyString anf'
            let (UPLCTerm t) = toUPLCTerm anf'
            step $ "UPLC:\n" <> (renderString . layoutSmart defaultLayoutOptions . prettyPlcReadable $ t)
            pure ()
    , testCaseSteps "Case 4" $ \step -> do
        step "Case: \\x y -> addInteger x y"
        step "1. Does Case 4 compile?"
        let compiled = compileTerm case4
        case compiled of
          Left err -> assertFailure $ "Compile error: " <> show err
          Right (_, t) -> do
            step "Successfully compiled!"
            let asAST = fromRawTerm t
            step $ "AST:\n" <> ppShow asAST
            let anf = fromHashedAST asAST
            step $ toPrettyString anf
            let anf' = analyzeDemand anf
            step $ toPrettyString anf'
            let (UPLCTerm t) = toUPLCTerm anf'
            step $ "UPLC:\n" <> (renderString . layoutSmart defaultLayoutOptions . prettyPlcReadable $ t)
            pure ()
    , testCaseSteps "Case 5" $ \step -> do
        step "Case: \\cond ifT ifF -> (compiled (\\cond' ifT' ifF' -> if cond' ifT' ifF') cond ifT ifF"
        step "1. Does Case 5 compile?"
        let compiled = compileTerm case5
        case compiled of
          Left err -> assertFailure $ "Compile error: " <> show err
          Right (_, t) -> do
            step "Successfully compiled!"
            let asAST = fromRawTerm t
            step $ "AST:\n" <> ppShow asAST
            let anf = fromHashedAST asAST
            step $ toPrettyString anf
            let anf' = analyzeDemand anf
            step $ toPrettyString anf'
            let (UPLCTerm t) = toUPLCTerm anf'
            step $ "UPLC:\n" <> (renderString . layoutSmart defaultLayoutOptions . prettyPlcReadable $ t)
            pure ()
    , testCaseSteps "Case 6" $ \step -> do
        step "Case: \\x y -> addInteger (multiplyInteger x x) (multiplyInteger y y)"
        step "1. Does Case 6 compile?"
        let compiled = compileTerm case6
        case compiled of
          Left err -> assertFailure $ "Compile error: " <> show err
          Right (_, t) -> do
            step "Successfully compiled!"
            let asAST = fromRawTerm t
            let anf = fromHashedAST asAST
            step $ toPrettyString anf
            let anf' = analyzeDemand anf
            step $ toPrettyString anf'
            let (UPLCTerm t) = toUPLCTerm anf'
            step $ "UPLC:\n" <> (renderString . layoutSmart defaultLayoutOptions . prettyPlcReadable $ t)
            pure ()
    , testCaseSteps "Case 7" $ \step -> do
        step "Case: \\x -> addInteger error (addInteger x error)"
        step "1. Does Case 7 compile?"
        let compiled = compileTerm case7
        case compiled of
          Left err -> assertFailure $ "Compile error: " <> show err
          Right (_, t) -> do
            step "Successfully compiled!"
            let asAST = fromRawTerm t
            let anf = fromHashedAST asAST
            step $ toPrettyString anf
            let anf' = analyzeDemand anf
            step $ toPrettyString anf'
            let (UPLCTerm t) = toUPLCTerm anf'
            step $ "UPLC:\n" <> (renderString . layoutSmart defaultLayoutOptions . prettyPlcReadable $ t)
            pure ()
    , testCaseSteps "Case 8" $ \step -> do
        step "Case: \\x -> constr 0 [x, error]"
        step "1. Does Case 8 compile?"
        let compiled = compileTerm case8
        case compiled of
          Left err -> assertFailure $ "Compile error: " <> show err
          Right (_, t) -> do
            step "Successfully compiled!"
            step $ "AST:\n" <> ppShow t
            let asAST = fromRawTerm t
            let anf = fromHashedAST asAST
            step $ toPrettyString anf
            let anf' = analyzeDemand anf
            step $ toPrettyString anf'
            let (UPLCTerm t) = toUPLCTerm anf'
            step $ "UPLC:\n" <> (renderString . layoutSmart defaultLayoutOptions . prettyPlcReadable $ t)
            pure ()
    , testCaseSteps "Case 9" $ \step -> do
        step "Case: \\x -> case error of [x]"
        step "1. Does Case 9 compile?"
        let compiled = compileTerm case9
        case compiled of
          Left err -> assertFailure $ "Compile error: " <> show err
          Right (_, t) -> do
            step "Successfully compiled!"
            step $ "RawTerm:\n" <> ppShow t
            let asAST = fromRawTerm t
            let anf = fromHashedAST asAST
            step $ toPrettyString anf
            let anf' = analyzeDemand anf
            step $ toPrettyString anf'
            let (UPLCTerm t) = toUPLCTerm anf'
            step $ "UPLC:\n" <> (renderString . layoutSmart defaultLayoutOptions . prettyPlcReadable $ t)
            pure ()
    , testCaseSteps "Cases 10 and 11" $ \step -> do
        step "Case: \\x -> (compose [\\y -> y + 2, \\z -> x * z, \\z1 -> z1 - 5]) x"
        step "1. Do both associativities compile?"
        let compiledLA = compileTerm case10
        let compiledRA = compileTerm case11
        case (compiledLA, compiledRA) of
          (Left err, _) -> assertFailure $ "Compile error: " <> show err
          (_, Left err) -> assertFailure $ "Compile error: " <> show err
          (Right (_, tla), Right (_, tra)) -> do
            step "Successfully compiled!"
            step $ "RawTerm (left associative):\n" <> ppShow tla
            step $ "RawTerm (right associative):\n" <> ppShow tra
            step "2. Are both RawTerms the same?"
            assertEqual "RawTerms differ" tla tra
            step "RawTerms are the same!"
            let tlaAST = fromRawTerm tla
            let traAST = fromRawTerm tra
            step $ "AST (left associative):\n" <> ppShow tlaAST
            step $ "AST (right associative):\n" <> ppShow traAST
            step "3. Are both ASTs the same?"
            assertEqual "ASTs differ" tlaAST traAST
            step "ASTs are the same!"
            let anfLA@(ANF tlaBM tlaANF) = fromHashedAST tlaAST
            let anfRA@(ANF traBM traANF) = fromHashedAST traAST
            step $ "ANF (left associative):\n" <> toPrettyString anfLA
            step $ "ANF (right associative):\n" <> toPrettyString anfRA
            step "4. Are both ANFs the same?"
            assertEqual "ANF bimaps differ" tlaBM traBM
            assertEqual "ANF binds differ" tlaANF traANF
            step "ANFs are the same!"
            let (UPLCTerm ttla) = toUPLCTerm . analyzeDemand $ anfLA
            let (UPLCTerm ttra) = toUPLCTerm . analyzeDemand $ anfRA
            step $ "UPLC (left associative):\n" <> (renderString . layoutSmart defaultLayoutOptions . prettyPlcReadable $ ttla)
            step $ "UPLC (right associative):\n" <> (renderString . layoutSmart defaultLayoutOptions . prettyPlcReadable $ ttra)
            pure ()
    , testCaseSteps "Case 12" $ \step -> do
        step "Case: \\x -> (compose [\\y -> y + 2, \\z -> z + 2, \\z1 -> z1 + 2]) x"
        step "1. Does Case 12 compile?"
        let compiled = compileTerm case12
        case compiled of
          Left err -> assertFailure $ "Compile error: " <> show err
          Right (_, t) -> do
            step "Successfully compiled!"
            step $ "RawTerm:\n" <> ppShow t
            let asAST = fromRawTerm t
            let anf = fromHashedAST asAST
            step $ toPrettyString anf
            let anf' = analyzeDemand anf
            step $ toPrettyString anf'
            let (UPLCTerm t) = toUPLCTerm anf'
            step $ "UPLC:\n" <> (renderString . layoutSmart defaultLayoutOptions . prettyPlcReadable $ t)
            pure ()
    , testCaseSteps "Case 13" $ \step -> do
        step "Case: \\x -> (compose [\\y -> y * y, \\z -> z + 2]) x"
        step "1. Does Case 13 compile?"
        let compiled = compileTerm case13
        case compiled of
          Left err -> assertFailure $ "Compile error: " <> show err
          Right (_, t) -> do
            step "Successfully compiled!"
            step $ "RawTerm:\n" <> ppShow t
            let asAST = fromRawTerm t
            let anf = fromHashedAST asAST
            step $ toPrettyString anf
            let anf' = analyzeDemand anf
            step $ toPrettyString anf'
            let (UPLCTerm t) = toUPLCTerm anf'
            step $ "UPLC:\n" <> (renderString . layoutSmart defaultLayoutOptions . prettyPlcReadable $ t)
            pure ()
    , testCaseSteps "Case 14" $ \step -> do
        step "Case: [[2]]"
        step "1. Does Case 14 compile?"
        let compiled = compileTerm case14
        case compiled of
          Left err -> assertFailure $ "Compile error: " <> show err
          Right (_, t) -> do
            step "Successfully compiled!"
            step $ "RawTerm:\n" <> ppShow t
            let asAST = fromRawTerm t
            let anf = fromHashedAST asAST
            step $ toPrettyString anf
            pure ()
    ]

-- Cases

-- Case 1: \x -> (\y -> y) ((\z -> z) x)
case1 :: forall (a :: S -> Type) (s :: S). Term s (a :--> a)
case1 = plam' $ \x -> papp (plam' id) (papp (plam' id) x)

-- Case 2: \x -> force (delay x)
case2 :: forall (a :: S -> Type) (s :: S). Term s (a :--> a)
case2 = plam' $ \x -> pforce (pdelay x)

-- Case 3: \x y -> por (pnot x) y
case3 :: forall (s :: S). Term s (PBool :--> PBool :--> PBool)
case3 = plam' $ \x -> plam' $ \y -> por (pnot x) y

-- Case 4: \x y -> addInteger x y
case4 :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
case4 = plam' $ \x -> plam' $ papp (papp paddInteger x)

-- Case 5: \cond ifT ifF -> (compiled (\cond' ifT' ifF' -> pif cond' ifT' ifF') cond ifT ifF
case5 ::
  forall (a :: S -> Type) (s :: S).
  Term s (PBool :--> a :--> a :--> a)
case5 = plam' $ \cond -> plam' $ \ifT -> plam' $ \ifF ->
  papp (papp (papp (pcompiled go) cond) ifT) ifF
  where
    go ::
      forall (s' :: S).
      Term s' (PBool :--> a :--> a :--> a)
    go = plam' $ \cond' -> plam' $ \ifT' -> plam' $ \ifF' -> pif cond' ifT' ifF'

-- Case 6: \x y -> addInteger (multiplyInteger x x) (multiplyInteger y y)
case6 :: forall (s :: S). Term s (PInteger :--> PInteger :--> PInteger)
case6 = plam' $ \x -> plam' $ \y ->
  papp (papp paddInteger (papp (papp pmultiplyInteger x) x)) (papp (papp pmultiplyInteger y) y)

-- Case 7: \x -> addInteger error (addInteger x error)
case7 :: forall (s :: S). Term s (PInteger :--> PInteger)
case7 = plam' $ \x -> papp (papp paddInteger perror) (papp (papp paddInteger x) perror)

-- Case 8: \x -> constr 0 [x, error]
case8 :: forall (a :: S -> Type) (b :: S -> Type) (s :: S). Term s (a :--> b)
case8 = plam' $ \x -> punsafeConstr 0 [toSomeTerm x, toSomeTerm perror]

-- Case 9: \x -> case error of [x]
case9 :: forall (a :: S -> Type) (s :: S). Term s (a :--> a)
case9 = plam' $ \x -> punsafeCase perror . NEVector.singleton . toSomeTerm $ x

-- Case 10: \x -> (compose [\y -> y + 2, \z -> x * z, \z1 -> z1 - 5]) x
--
-- Constructed left associatively
case10 :: forall (s :: S). Term s (PInteger :--> PInteger)
case10 =
  let fun1 = plam' $ \y -> papp (papp paddInteger y) (punsafeConstant $ PLC.someValue @Integer 2)
      fun3 = plam' $ \z1 -> papp (papp psubtractInteger z1) (punsafeConstant $ PLC.someValue @Integer 5)
   in plam' $ \x ->
        let fun2 = plam' $ \z -> papp (papp pmultiplyInteger x) z
         in papp (pcompose (pcompose fun1 fun2) fun3) x

-- Case 11: \x -> (compose [\y -> y + 2, \z -> x * z, \z1 -> z1 - 5]) x
--
-- Constructed right associatively
case11 :: forall (s :: S). Term s (PInteger :--> PInteger)
case11 =
  let fun1 = plam' $ \y -> papp (papp paddInteger y) (punsafeConstant $ PLC.someValue @Integer 2)
      fun3 = plam' $ \z1 -> papp (papp psubtractInteger z1) (punsafeConstant $ PLC.someValue @Integer 5)
   in plam' $ \x ->
        let fun2 = plam' $ \z -> papp (papp pmultiplyInteger x) z
         in papp (pcompose fun1 . pcompose fun2 $ fun3) x

-- Case 12: \x -> (compose [\y -> y + 2, \z -> z + 2, \z1 -> z1 + 2]) x
case12 :: forall (s :: S). Term s (PInteger :--> PInteger)
case12 =
  let fun = plam' $ \y -> papp (papp paddInteger y) (punsafeConstant $ PLC.someValue @Integer 2)
   in plam' $ \x -> papp (pcompose fun . pcompose fun $ fun) x

-- Case 13: \x -> (compose [\y -> y * y, \z -> z + 2]) x
case13 :: forall (s :: S). Term s (PInteger :--> PInteger)
case13 =
  let f = plam' $ \y -> papp (papp pmultiplyInteger y) y
      g = plam' $ \z -> papp (papp paddInteger z) (punsafeConstant $ PLC.someValue @Integer 2)
   in plam' $ \x -> papp (pcompose f g) x

-- [[2]] (for pretty printing)
case14 :: forall (s :: S). Term s (PBList (PBList (PBPair PInteger PInteger)))
case14 = punsafeConstant $ PLC.someValue @[[(Integer, Integer)]] [[(2, 3)]]

-- Helpers

compileTerm ::
  forall (a :: S -> Type) (s :: S).
  Term s a -> Either TermError (VarMap, RawTerm ())
compileTerm t = case runRWS (runExceptT (asRawTerm t)) TermEnv 0 of
  (res, _, _) -> res

toPrettyString :: forall a. Pretty a => a -> String
toPrettyString = renderString . layoutSmart defaultLayoutOptions . pretty
