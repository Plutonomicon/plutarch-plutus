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
  pdelay,
  perror,
  pforce,
  plam',
  punsafeCase,
  punsafeConstr,
  toSomeTerm,
 )
import Plutarch.Backend.UPLC (UPLCTerm (UPLCTerm))
import Plutarch.Backend.VarMap (VarMap, vmEmpty)
import Plutarch.Primitive.Bool (PBool, pif, pnot, por)
import Plutarch.Primitive.Function ((:-->))
import Plutarch.Primitive.Numeric (
  PInteger,
  paddInteger,
  pmultiplyInteger,
 )
import PlutusCore.Pretty (prettyPlcReadable)
import Prettyprinter (defaultLayoutOptions, layoutSmart)
import Prettyprinter.Render.String (renderString)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCaseSteps)
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
                let anf@(ANF bm binds) = fromHashedAST asAST
                step $ "ANF bimap:\n" <> ppShow bm
                step $ "ANF binds:\n" <> ppShow binds
                step "Demand analysis"
                let anf'@(ANF bm' binds') = analyzeDemand anf
                step $ "ANF bimap:\n" <> ppShow bm'
                step $ "ANF binds:\n" <> ppShow binds'
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
            let ANF bm binds = fromHashedAST asAST
            step $ "ANF bimap:\n" <> ppShow bm
            step $ "ANF binds:\n" <> ppShow binds
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
            let anf@(ANF bm binds) = fromHashedAST asAST
            step $ "ANF bimap:\n" <> ppShow bm
            step $ "ANF binds:\n" <> ppShow binds
            step "2. Are there 5 binds?"
            let len = NEVector.length binds
            assertBool ("Too many binds: " <> show len) (len == 5)
            step "5 binds exactly!"
            step "Demand analysis"
            let anf'@(ANF bm' binds') = analyzeDemand anf
            step $ "ANF bimap:\n" <> ppShow bm'
            step $ "ANF binds:\n" <> ppShow binds'
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
            let anf@(ANF bm binds) = fromHashedAST asAST
            step $ "ANF bimap:\n" <> ppShow bm
            step $ "ANF binds:\n" <> ppShow binds
            let anf'@(ANF bm' binds') = analyzeDemand anf
            step $ "ANF bimap:\n" <> ppShow bm'
            step $ "ANF binds:\n" <> ppShow binds'
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
            let anf@(ANF bm binds) = fromHashedAST asAST
            step $ "ANF bimap:\n" <> ppShow bm
            step $ "ANF binds:\n" <> ppShow binds
            let anf'@(ANF bm' binds') = analyzeDemand anf
            step $ "ANF bimap:\n" <> ppShow bm'
            step $ "ANF binds:\n" <> ppShow binds'
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
            let anf@(ANF bm binds) = fromHashedAST asAST
            step $ "ANF bimap:\n" <> ppShow bm
            step $ "ANF binds:\n" <> ppShow binds
            let anf'@(ANF bm' binds') = analyzeDemand anf
            step $ "ANF bimap:\n" <> ppShow bm'
            step $ "ANF binds:\n" <> ppShow binds'
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
            let anf@(ANF bm binds) = fromHashedAST asAST
            step $ "ANF bimap:\n" <> ppShow bm
            step $ "ANF binds:\n" <> ppShow binds
            let anf'@(ANF bm' binds') = analyzeDemand anf
            step $ "ANF bimap:\n" <> ppShow bm'
            step $ "ANF binds:\n" <> ppShow binds'
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
            let anf@(ANF bm binds) = fromHashedAST asAST
            step $ "ANF bimap:\n" <> ppShow bm
            step $ "ANF binds:\n" <> ppShow binds
            let anf'@(ANF bm' binds') = analyzeDemand anf
            step $ "ANF bimap:\n" <> ppShow bm'
            step $ "ANF binds:\n" <> ppShow binds'
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
            step $ "AST:\n" <> ppShow t
            let asAST = fromRawTerm t
            let anf@(ANF bm binds) = fromHashedAST asAST
            step $ "ANF bimap:\n" <> ppShow bm
            step $ "ANF binds:\n" <> ppShow binds
            let anf'@(ANF bm' binds') = analyzeDemand anf
            step $ "ANF bimap:\n" <> ppShow bm'
            step $ "ANF binds:\n" <> ppShow binds'
            let (UPLCTerm t) = toUPLCTerm anf'
            step $ "UPLC:\n" <> (renderString . layoutSmart defaultLayoutOptions . prettyPlcReadable $ t)
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

-- Helpers

compileTerm ::
  forall (a :: S -> Type) (s :: S).
  Term s a -> Either TermError (VarMap, RawTerm ())
compileTerm t = case runRWS (runExceptT (asRawTerm t)) TermEnv 0 of
  (res, _, _) -> res
