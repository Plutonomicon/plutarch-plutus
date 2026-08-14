module Plutarch.Test.Codegen (
  identicalCode,
) where

import Data.Diff.Myers (diffTexts)
import Data.Kind (Type)
import Data.Tagged (Tagged (Tagged))
import Data.Text (Text)
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (Term, TermError, releaseTermEnv)
import Plutarch.Helpers.Compile (termToUPLC)
import Prettyprinter (defaultLayoutOptions, layoutSmart, pretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Tasty (TestTree)
import Test.Tasty.Providers (
  IsTest (run, testOptions),
  singleTest,
  testFailed,
  testPassed,
 )
import Type.Reflection (Typeable)

{- | Checks that the two given closed 'Term's compile to identical UPLC. If they
do not, produces a diff between them as the error message. If either 'Term'
does not compile, the test will instead fail and display the error.

This uses 'releaseTermEnv' as this gives the most optimized code.

@since wip
-}
identicalCode ::
  forall (a :: S -> Type).
  Typeable a =>
  -- | Test name
  String ->
  (forall (s :: S). Term s a) ->
  (forall (s :: S). Term s a) ->
  TestTree
identicalCode testName t1 t2 = singleTest testName (IC t1 t2)

-- Helpers

data IdenticalCode (a :: S -> Type)
  = IC
      (forall (s :: S). Term s a)
      (forall (s :: S). Term s a)

instance Typeable a => IsTest (IdenticalCode a) where
  run _ (IC t1 t2) _ = do
    let code1 = toPrettyUPLC t1
    let code2 = toPrettyUPLC t2
    case code1 of
      Left err -> pure . testFailed $ "First Term compilation errored: " <> show err
      Right code1' -> case code2 of
        Left err -> pure . testFailed $ "Second Term compilation errored: " <> show err
        Right code2' ->
          if code1' == code2'
            then pure $ testPassed "Code identical"
            else do
              let diff = diffTexts code1' code2'
              pure . testFailed $ "Code differs: \n" <> show diff
  testOptions = Tagged []

toPrettyUPLC ::
  forall (a :: S -> Type).
  (forall (s :: S). Term s a) ->
  Either TermError Text
toPrettyUPLC t =
  renderStrict
    . layoutSmart defaultLayoutOptions
    . pretty
    <$> termToUPLC releaseTermEnv t
