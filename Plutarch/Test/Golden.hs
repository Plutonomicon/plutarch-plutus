{-# LANGUAGE NoPartialTypeSignatures #-}

module Plutarch.Test.Golden (
  plutarchGolden,
) where

import Control.Exception (Exception, throwIO)
import Control.Monad.Except (runExceptT)
import Control.Monad.RWS.CPS (runRWS)
import Data.ByteString.Lazy qualified as Lazy
import Data.Char (isSpace, isUpperCase, toLower)
import Data.Kind (Type)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Plutarch.Backend.ANF (analyzeDemand, fromHashedAST)
import Plutarch.Backend.AST (fromRawTerm)
import Plutarch.Backend.Compile (toUPLCTerm)
import Plutarch.Backend.RawTerm (RawTerm)
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (
  Term (Term),
  TermEnv (TermEnv),
  TermError,
 )
import Prettyprinter (Pretty (pretty), defaultLayoutOptions, layoutSmart)
import Prettyprinter.Render.Text (renderLazy)
import System.FilePath ((<.>), (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

plutarchGolden ::
  forall (a :: S -> Type).
  String ->
  String ->
  (forall (s :: S). Term s a) ->
  TestTree
plutarchGolden testDescription testName t =
  let folderName = toFolderName testName
      goldenFolderFP = "golden" </> folderName
      termGoldenFP = goldenFolderFP </> "term" <.> "golden"
      compiled = compileTerm t
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

compileTerm ::
  forall (a :: S -> Type).
  (forall (s :: S). Term s a) ->
  Either TermError (RawTerm ())
compileTerm (Term comp) = (\(x, _, _) -> fmap snd x) . runRWS (runExceptT comp) TermEnv $ 0

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

newtype DidNotCompileException = DidNotCompileException TermError
  deriving stock (Show)

instance Exception DidNotCompileException
