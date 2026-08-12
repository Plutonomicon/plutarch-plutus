{-# LANGUAGE NoPartialTypeSignatures #-}

module Plutarch.Test.Golden (
  plutarchGolden,
  plutarchGoldenWith,
  plutarchGoldenEval,
  plutarchGoldenEvalWith,
  plutarchGoldenAll,
  plutarchGoldenAllWith,
) where

import Control.Exception (Exception, throwIO)
import Data.ByteString.Lazy qualified as Lazy
import Data.Char (isSpace, isUpperCase, toLower)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Plutarch.Backend.ANF (analyzeDemand, fromHashedAST)
import Plutarch.Backend.AST (fromRawTerm)
import Plutarch.Backend.Compile (toUPLCTerm)
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (
  Term,
  TermEnv,
  TermError,
  debugTermEnv,
  releaseTermEnv,
 )
import Plutarch.Backend.UPLC (UPLCTerm, uplcConstant)
import Plutarch.Helpers.Compile (compileTerm, termToUPLC)
import Plutarch.Helpers.Evaluate (evalUPLC, maxBudget)
import PlutusCore qualified as PLC
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget)
import Prettyprinter (Pretty (pretty), defaultLayoutOptions, layoutSmart)
import Prettyprinter.Render.Text (renderLazy)
import System.FilePath ((<.>), (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import UntypedPlutusCore.Evaluation.Machine.Cek qualified as Cek

{- | Constructs golden files based on the given closed 'Term'. Specifically,
will generate golden files of each of the following:

* The 'Term' itself;
* Its 'AST';
* Its 'ANF', both before, and after, demand analysis; and
* The resulting UPLC.

All but the first of these will produce an error if the 'Term' fails to
compile. This uses 'debugTermEnv' for clarity.

= Important note

Ensure that the test name is unique to all tests in your suite. This will be
used to determine the name of the folder where the golden files will be
placed: if there is a name collision, this will produce weird failures.

@since wip
-}
plutarchGolden ::
  forall (a :: S -> Type).
  {- | A description for the test. This is what you will see when you run the
  golden test.
  -}
  String ->
  -- | A name for the test, which should be unique (as described above).
  String ->
  -- | A closed 'Term'.
  (forall (s :: S). Term s a) ->
  TestTree
plutarchGolden = plutarchGoldenWith debugTermEnv

{- | As 'plutarchGolden', but allows specifying the 'TermEnv' to compile with.

@since wip
-}
plutarchGoldenWith ::
  forall (a :: S -> Type).
  TermEnv ->
  String ->
  String ->
  (forall (s :: S). Term s a) ->
  TestTree
plutarchGoldenWith env testDescription testName t =
  let folderName = toFolderName testName
      goldenFolderFP = "golden" </> folderName
      termGoldenFP = goldenFolderFP </> "term" <.> "golden"
      compiled = compileTerm env t
      asAST = fromRawTerm <$> compiled
      astGoldenFP = goldenFolderFP </> "ast" <.> "golden"
      asANF = fromHashedAST <$> asAST
      anfGoldenFP = goldenFolderFP </> "anf" <.> "golden"
      withDemand = analyzeDemand <$> asANF
      demandGoldenFP = goldenFolderFP </> "anf-demand" <.> "golden"
      asUPLC = toUPLCTerm <$> withDemand
      uplcGoldenFP = goldenFolderFP </> "uplc" <.> "golden"
   in testGroup
        (testName <> ": " <> testDescription)
        [ goldenVsString "Term" termGoldenFP (pure . toLazyBS $ t)
        , goldenVsString "AST" astGoldenFP (toLazyBSOrErr asAST)
        , goldenVsString "ANF" anfGoldenFP (toLazyBSOrErr asANF)
        , goldenVsString "ANF with demand analysis" demandGoldenFP (toLazyBSOrErr withDemand)
        , goldenVsString "UPLC" uplcGoldenFP (toLazyBSOrErr asUPLC)
        ]

{- | Constructs golden files based on the given closed 'Term' and its
evaluation. Specifically, will generate golden files of each of the
following:

* The 'Term' itself;
* The UPLC resulting from compiling the 'Term'; and
* The UPLC resulting from evaluating the compiled 'Term'.

All but the first will produce an error if the 'Term' fails to compile, and
the last will produce an error if the compiled 'Term' fails to evaluate.

This uses 'releaseTermEnv', as this produces the best possible code.

= Important note

The caveats regarding naming given for 'plutarchGolden' also apply to this
function.

@since wip
-}
plutarchGoldenEval ::
  forall (a :: S -> Type).
  -- | A description for the test. This is what you will see when the test runs.
  String ->
  -- | A name for the test, which should be unique (as described above).
  String ->
  -- | A closed 'Term'.
  (forall (s :: S). Term s a) ->
  TestTree
plutarchGoldenEval = plutarchGoldenEvalWith releaseTermEnv

{- | As 'plutarchGoldenEval', but allows specifying the 'TermEnv' to compile
with.

@since wip
-}
plutarchGoldenEvalWith ::
  forall (a :: S -> Type).
  TermEnv ->
  String ->
  String ->
  (forall (s :: S). Term s a) ->
  TestTree
plutarchGoldenEvalWith env testDescription testName t =
  let folderName = toFolderName testName
      goldenFolderFP = "golden" </> folderName
      termGoldenFP = goldenFolderFP </> "term" <.> "golden"
      compiled = termToUPLC env t
      uplcGoldenFP = goldenFolderFP </> "uplc" <.> "golden"
      evaluated = evalUPLC maxBudget <$> compiled
      uplcEvalGoldenFP = goldenFolderFP </> "uplc-eval" <.> "golden"
   in testGroup
        (testName <> ": " <> testDescription <> " (eval)")
        [ goldenVsString "Term" termGoldenFP (pure . toLazyBS $ t)
        , goldenVsString "UPLC" uplcGoldenFP (toLazyBSOrErr compiled)
        , goldenVsString "UPLC (evaluated)" uplcEvalGoldenFP (toLazyBSEvaluated evaluated)
        ]

{- | A combination of all the tests from both 'plutarchGolden' and
'plutarchGoldenEval'. Uses the same configuration 'plutarchGolden' would use.

@since wip
-}
plutarchGoldenAll ::
  forall (a :: S -> Type).
  -- | A description for the test. This is what you will see when the test runs.
  String ->
  -- | A name for the test, which should be unique (as described above).
  String ->
  -- | A closed 'Term'.
  (forall (s :: S). Term s a) ->
  TestTree
plutarchGoldenAll = plutarchGoldenAllWith debugTermEnv

{- | As 'plutarchGoldenAll', but allows specifying the 'TermEnv' to compile
with.

@since wip
-}
plutarchGoldenAllWith ::
  forall (a :: S -> Type).
  TermEnv ->
  String ->
  String ->
  (forall (s :: S). Term s a) ->
  TestTree
plutarchGoldenAllWith env testDescription testName t =
  let folderName = toFolderName testName
      goldenFolderFP = "golden" </> folderName
      termGoldenFP = goldenFolderFP </> "term" <.> "golden"
      compiled = compileTerm env t
      asAST = fromRawTerm <$> compiled
      astGoldenFP = goldenFolderFP </> "ast" <.> "golden"
      asANF = fromHashedAST <$> asAST
      anfGoldenFP = goldenFolderFP </> "anf" <.> "golden"
      withDemand = analyzeDemand <$> asANF
      demandGoldenFP = goldenFolderFP </> "anf-demand" <.> "golden"
      asUPLC = toUPLCTerm <$> withDemand
      uplcGoldenFP = goldenFolderFP </> "uplc" <.> "golden"
      evaluated = evalUPLC maxBudget <$> asUPLC
      uplcEvalGoldenFP = goldenFolderFP </> "uplc-eval" <.> "golden"
   in testGroup
        (testName <> ": " <> testDescription)
        [ goldenVsString "Term" termGoldenFP (pure . toLazyBS $ t)
        , goldenVsString "AST" astGoldenFP (toLazyBSOrErr asAST)
        , goldenVsString "ANF" anfGoldenFP (toLazyBSOrErr asANF)
        , goldenVsString "ANF with demand analysis" demandGoldenFP (toLazyBSOrErr withDemand)
        , goldenVsString "UPLC" uplcGoldenFP (toLazyBSOrErr asUPLC)
        , goldenVsString "UPLC (evaluated)" uplcEvalGoldenFP (toLazyBSEvaluated evaluated)
        ]

-- Helpers

-- Replace whitespace with dash, downcase everything
toFolderName :: String -> FilePath
toFolderName s = go <$> s
  where
    go :: Char -> Char
    go c
      | isSpace c = '-'
      | isUpperCase c = toLower c
      | otherwise = c

toLazyBS :: forall (a :: Type). Pretty a => a -> Lazy.ByteString
toLazyBS = encodeUtf8 . renderLazy . layoutSmart defaultLayoutOptions . pretty

toLazyBSOrErr ::
  forall (a :: Type).
  Pretty a =>
  Either TermError a ->
  IO Lazy.ByteString
toLazyBSOrErr = \case
  Left err -> throwIO . DidNotCompileException $ err
  Right res -> pure . toLazyBS $ res

toLazyBSEvaluated ::
  Either
    TermError
    ( Either
        (Cek.CekEvaluationException PLC.Name PLC.DefaultUni PLC.DefaultFun)
        (Either (PLC.Some (PLC.ValueOf PLC.DefaultUni)) UPLCTerm)
    , ExBudget
    , [Text]
    ) ->
  IO Lazy.ByteString
toLazyBSEvaluated = \case
  Left err -> throwIO . DidNotCompileException $ err
  Right (Left err, _, _) -> throwIO . DidNotEvaluateException $ err
  Right (Right (Left c), _, _) -> pure . toLazyBS . uplcConstant $ c
  Right (Right (Right t), _, _) -> pure . toLazyBS $ t

newtype DidNotCompileException = DidNotCompileException TermError
  deriving stock (Show)

instance Exception DidNotCompileException

newtype DidNotEvaluateException
  = DidNotEvaluateException (Cek.CekEvaluationException PLC.Name PLC.DefaultUni PLC.DefaultFun)
  deriving stock (Show)

instance Exception DidNotEvaluateException
