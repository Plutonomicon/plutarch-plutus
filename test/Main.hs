module Main (main) where

import Control.Monad.Except (runExceptT)
import Control.Monad.RWS.CPS (runRWS)
import Data.Kind (Type)
import Plutarch.Backend.AST (
  ANF (ANF),
  fromHashedAST,
  fromRawTerm,
  toUPLCTerm,
 )
import Plutarch.Backend.RawTerm (RawTerm (RLamAbs))
import Plutarch.Backend.Term (
  S,
  Term (asRawTerm),
  TermEnv (TermEnv),
  TermError,
  papp,
  plam',
  (:-->),
 )
import Plutarch.Backend.UPLC (UPLCTerm (UPLCTerm))
import Plutarch.Backend.VarMap (VarMap, vmEmpty)
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
                let anf@(ANF bm bindings) = fromHashedAST asAST
                step $ "ANF bimap:\n" <> ppShow bm
                step $ "ANF bindings:\n" <> ppShow bindings
                step "Converting to UPLC"
                let (UPLCTerm t) = toUPLCTerm anf
                step $ "UPLC:\n" <> (renderString . layoutSmart defaultLayoutOptions . prettyPlcReadable $ t)
                pure ()
              _ -> assertFailure $ "Unexpected top node: \n" <> ppShow t
    ]

-- Cases

-- Case 1

-- \x -> (\y -> y) ((\z -> z) x)
case1 :: forall (a :: S -> Type) (s :: S). Term s (a :--> a)
case1 = plam' $ \x -> papp (plam' id) (papp (plam' id) x)

-- Helpers

compileTerm ::
  forall (a :: S -> Type) (s :: S).
  Term s a -> Either TermError (VarMap, RawTerm ())
compileTerm t = case runRWS (runExceptT (asRawTerm t)) TermEnv 0 of
  (res, _, _) -> res
