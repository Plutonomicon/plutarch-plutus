module Main (main) where

import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.Char (ord)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Exts (fromList)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Plutarch (
  Config (NoTracing, Tracing),
  LogLevel (LogDebug, LogInfo),
  TracingMode (DoTracing),
 )
import Plutarch.ByteString (pallBS)
import Plutarch.Evaluate (evalTerm)
import Plutarch.Lift (PLifted, PUnsafeLiftDecl)
import Plutarch.List (pcheckSorted, preverse)
import Plutarch.Prelude (
  PBool,
  PUnit,
  S,
  Term,
  pconstant,
  plam,
  plift,
  pnot,
  ptraceDebug,
  ptraceDebugError,
  ptraceDebugIfFalse,
  ptraceDebugIfTrue,
  ptraceDebugShowId,
  ptraceInfo,
  ptraceInfoError,
  ptraceInfoIfFalse,
  ptraceInfoIfTrue,
  ptraceInfoShowId,
  (#),
  (#$),
  (#==),
 )
import Plutarch.String (pisHexDigit)
import Test.Tasty (
  DependencyType (AllSucceed),
  after,
  defaultMain,
  testGroup,
 )
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

main :: IO ()
main = do
  setLocaleEncoding utf8
  defaultMain . testGroup "Plutarch" $
    [ testGroup
        "PByteString"
        [ testGroup
            "pallBS"
            [ testCase "predicate matching all entries works" . isTrue $
                (pallBS # plam (#== pconstant (toInteger . ord $ 'a')) # pconstant "aaaaaaaaaa")
            , testCase "predicate missing one case fails" . isFalse $
                (pallBS # plam (#== pconstant (toInteger . ord $ 'a')) # pconstant "aaaaaaaaab")
            ]
        , after AllSucceed "pallBS" . testGroup "pisHexDigit" $
            [ testCase "numbers are hex digits" . isTrue $
                (pallBS # pisHexDigit # pconstant "0123456789")
            , testCase "A-F are hex digits" . isTrue $
                (pallBS # pisHexDigit # pconstant "ABCDEF")
            , testCase "a-f are hex digits" . isTrue $
                (pallBS # pisHexDigit # pconstant "abcdef")
            , testCase "no other ASCII code is a hex digit" . isTrue $
                (pallBS # plam (\x -> pnot #$ pisHexDigit # x) # pconstant nonHexAscii)
            ]
        ]
    , testGroup
        "List utilities"
        [ testGroup
            "preverse"
            [ testCase "reversing an empty list is identity" . matches (pconstant ([] :: [Integer])) $
                (preverse # pconstant ([] :: [Integer]))
            , testCase "reversing a singleton is identity" . matches (pconstant ([1] :: [Integer])) $
                (preverse # pconstant ([1] :: [Integer]))
            , testCase "reversing a non-singleton" . matches (pconstant ([2, 1] :: [Integer])) $
                (preverse # pconstant ([1, 2] :: [Integer]))
            ]
        , testGroup
            "pcheckSorted"
            [ testCase "empty lists are sorted" . isTrue $
                (pcheckSorted # pconstant ([] :: [Integer]))
            , testCase "singleton lists are sorted" . isTrue $
                (pcheckSorted # pconstant ([1] :: [Integer]))
            , testCase "two items in the right order are sorted" . isTrue $
                (pcheckSorted # pconstant ([1, 2] :: [Integer]))
            , testCase "two items in the wrong order are not sorted" . isFalse $
                (pcheckSorted # pconstant ([2, 1] :: [Integer]))
            ]
        ]
    , testGroup
        "tracing"
        [ testCase "ptraceInfo traces at info level"
            . traces LogInfo (ptraceInfo "foo" (pconstant ()))
            $ ["foo"]
        , testCase "ptraceInfo traces at debug level"
            . traces LogDebug (ptraceInfo "foo" (pconstant ()))
            $ ["foo"]
        , testCase "ptraceInfoShowId traces at info level"
            . traces LogInfo (ptraceInfoShowId (pconstant ()))
            $ ["()"]
        , testCase "ptraceInfoShowId traces at debug level"
            . traces LogDebug (ptraceInfoShowId (pconstant ()))
            $ ["()"]
        , testCase "ptraceInfoError traces at info level"
            . traces LogInfo (ptraceInfoError @PUnit "foo")
            $ ["foo"]
        , testCase "ptraceInfoError traces at debug level"
            . traces LogDebug (ptraceInfoError @PUnit "foo")
            $ ["foo"]
        , testCase "ptraceInfoIfTrue traces at info level if true"
            . traces LogInfo (ptraceInfoIfTrue "foo" (pconstant True))
            $ ["foo"]
        , testCase "ptraceInfoIfTrue does not trace at info level if false"
            . traces LogInfo (ptraceInfoIfTrue "foo" (pconstant False))
            $ []
        , testCase "ptraceInfoIfTrue traces at debug level if true"
            . traces LogDebug (ptraceInfoIfTrue "foo" (pconstant True))
            $ ["foo"]
        , testCase "ptraceInfoIfTrue does not trace at debug level if false"
            . traces LogDebug (ptraceInfoIfTrue "foo" (pconstant False))
            $ []
        , testCase "ptraceInfoIfFalse does not trace at info level if true"
            . traces LogInfo (ptraceInfoIfFalse "foo" (pconstant True))
            $ []
        , testCase "ptraceInfoIfFalse traces at info level if false"
            . traces LogInfo (ptraceInfoIfFalse "foo" (pconstant False))
            $ ["foo"]
        , testCase "ptraceInfoIfFalse does not trace at debug level if true"
            . traces LogDebug (ptraceInfoIfFalse "foo" (pconstant True))
            $ []
        , testCase "ptraceInfoIfFalse traces at debug level if false"
            . traces LogDebug (ptraceInfoIfFalse "foo" (pconstant False))
            $ ["foo"]
        , testCase "ptraceDebug does not trace at info level"
            . traces LogInfo (ptraceDebug "foo" (pconstant ()))
            $ []
        , testCase "ptraceDebug traces at debug level"
            . traces LogDebug (ptraceDebug "foo" (pconstant ()))
            $ ["foo"]
        , testCase "ptraceDebugShowId does not trace at info level"
            . traces LogInfo (ptraceDebugShowId (pconstant ()))
            $ []
        , testCase "ptraceDebugShowId traces at debug level"
            . traces LogDebug (ptraceDebugShowId (pconstant ()))
            $ ["()"]
        , testCase "ptraceDebugError does not trace at info level"
            . traces LogInfo (ptraceDebugError @PUnit "foo")
            $ []
        , testCase "ptraceDebugError traces at debug level"
            . traces LogDebug (ptraceDebugError @PUnit "foo")
            $ ["foo"]
        , testCase "ptraceDebugIfTrue does not trace at info level if true"
            . traces LogInfo (ptraceDebugIfTrue "foo" (pconstant True))
            $ []
        , testCase "ptraceDebugIfTrue does not trace at info level if false"
            . traces LogInfo (ptraceDebugIfTrue "foo" (pconstant False))
            $ []
        , testCase "ptraceDebugIfTrue traces at debug level if true"
            . traces LogDebug (ptraceDebugIfTrue "foo" (pconstant True))
            $ ["foo"]
        , testCase "ptraceDebugIfTrue does not trace at debug level if false"
            . traces LogDebug (ptraceDebugIfTrue "foo" (pconstant False))
            $ []
        , testCase "ptraceDebugIfFalse does not trace at info level if true"
            . traces LogInfo (ptraceDebugIfFalse "foo" (pconstant True))
            $ []
        , testCase "ptraceDebugIfFalse does not trace at info level if false"
            . traces LogInfo (ptraceDebugIfFalse "foo" (pconstant False))
            $ []
        , testCase "ptraceDebugIfFalse does not trace at debug level if true"
            . traces LogDebug (ptraceDebugIfFalse "foo" (pconstant True))
            $ []
        , testCase "ptraceDebugIfFalse traces at debug level if false"
            . traces LogDebug (ptraceDebugIfFalse "foo" (pconstant False))
            $ ["foo"]
        ]
    ]

-- Helpers

traces ::
  forall (a :: S -> Type).
  LogLevel ->
  (forall (s :: S). Term s a) ->
  [Text] ->
  IO ()
traces ll comp expected = case evalTerm (Tracing ll DoTracing) comp of
  Left err -> assertFailure $ "Did not compile: " <> Text.unpack err
  Right (_, _, logs) -> assertEqual "" expected logs

isTrue ::
  (forall (s :: S). Term s PBool) ->
  IO ()
isTrue comp = case evalTerm NoTracing comp of
  Left err -> assertFailure $ "Did not compile: " <> Text.unpack err
  Right (res, _, _) -> case res of
    Left err -> assertFailure $ "Execution errored: " <> show err
    Right t -> unless (plift t) (assertFailure "is false")

isFalse ::
  (forall (s :: S). Term s PBool) ->
  IO ()
isFalse comp = case evalTerm NoTracing comp of
  Left err -> assertFailure $ "Did not compile: " <> Text.unpack err
  Right (res, _, _) -> case res of
    Left err -> assertFailure $ "Execution errored: " <> show err
    Right t -> when (plift t) (assertFailure "is true")

matches ::
  forall (a :: S -> Type).
  (PUnsafeLiftDecl a, Eq (PLifted a)) =>
  -- expected
  (forall (s :: S). Term s a) ->
  -- actual
  (forall (s :: S). Term s a) ->
  IO ()
matches expected actual = case evalTerm NoTracing expected of
  Left err -> assertFailure $ "Expected case failed to compile: " <> Text.unpack err
  Right (res, _, _) -> case res of
    Left err -> assertFailure $ "Expected case execution errored: " <> show err
    Right expected' -> case evalTerm NoTracing actual of
      Left err -> assertFailure $ "Case did not compile: " <> Text.unpack err
      Right (res', _, _) -> case res' of
        Left err -> assertFailure $ "Case execution errored: " <> show err
        Right actual' -> unless (plift expected' == plift actual') (assertFailure "cases do not match")

nonHexAscii :: ByteString
nonHexAscii =
  -- All codes up to, but not including, the first digit
  fromList [0, 1 .. 47]
    <>
    -- Between digits to upper-case
    fromList [58, 59 .. 64]
    <>
    -- Between upper-case and lower-case
    fromList [71, 72 .. 96]
    <>
    -- After lower-case
    fromList [103 .. 127]
