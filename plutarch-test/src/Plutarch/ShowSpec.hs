module Plutarch.ShowSpec (spec) where

import Control.Monad (forM_)
import Data.String (IsString (fromString))
import qualified Data.Text as T
import Test.Syd

import Plutarch.ListSpec (integerList)
import Plutarch.Prelude
import Plutarch.Test

spec :: Spec
spec = do
  describe "show" . pgoldenSpec $ do
    let str x = pconstant @PString x
    "unit" @| pshow (pcon PUnit) @== str "()"
    "bool" @\ do
      "true" @| pshow (pcon PTrue) @== str "PTrue"
      "false" @| pshow (pcon PFalse) @== str "PFalse"
    "int" @\ do
      "0" @| pshow (pconstant @PInteger 0) @== str "0"
      forM_ [5, 10, 14, 102] $ \n -> do
        (fromString $ show n)
          @| pshow (pconstant @PInteger n)
          @== pconstant (T.pack $ show n)
        (fromString $ show (-n))
          @| pshow (pconstant @PInteger (-n))
          @== pconstant (T.pack $ show (-n))
    "maybe" @\ do
      "nothing"
        @| pshow @(PMaybe PInteger) (pcon PNothing)
        @== str "PNothing"
      "just"
        @| pshow @(PMaybe PInteger) (pcon $ PJust $ pconstant @PInteger 42)
        @== str "PJust 42"
    "either" @\ do
      "right"
        @| pshow (pcon @(PEither PUnit PInteger) $ PRight 42)
        @== str "PRight 42"
    -- Test automatic injection of `(..)`.
    "maybe.either"
      @| pshow (pcon $ PJust $ pcon @(PEither PInteger PUnit) $ PLeft 42)
      @== str "PJust (PLeft 42)"
    "list" @\ do
      "nil" @| pshow (integerList []) @== str "[]"
      "1" @| pshow (integerList [1]) @== str "[1]"
      "1,2,3" @| pshow (integerList [1, 2, 3]) @== str "[1, 2, 3]"
    "builtinlist" @\ do
      let xs3 = pconstant @(PBuiltinList PInteger) [1, 2, 3]
          xs0 = pconstant @(PBuiltinList PInteger) []
      "nil" @| pshow xs0 @== str "[]"
      "1,2,3" @| pshow xs3 @== str "[1, 2, 3]"
    "pair" @\ do
      "int-str"
        @| pshow (pcon @(PPair PInteger PString) $ PPair 42 "hello")
        @== str "PPair 42 hello"
      "int-list"
        @| pshow (pcon @(PPair PInteger (PBuiltinList PInteger)) $ PPair 42 $ pconstant [1, 2, 3])
        @== str "PPair 42 [1, 2, 3]"
    "rational" @\ do
      let rat :: Term s PRational -> Term s PRational
          rat = id
      "1/2"
        @| pshow (rat $ 1 / 2)
        @== str "1/2"
