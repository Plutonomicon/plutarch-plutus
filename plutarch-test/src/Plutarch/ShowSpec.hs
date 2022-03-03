module Plutarch.ShowSpec (spec) where

import Control.Monad (forM_)
import Data.String (IsString (fromString))
import qualified Data.Text as T
import Test.Syd

import Plutarch.Prelude
import Plutarch.Show
import Plutarch.Test

spec :: Spec
spec = do
  describe "show" . pgoldenSpec $ do
    "int" @\ do
      "0" @| pshow (pconstant @PInteger 0) @== pconstant @PString "0"
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
        @== pconstant @PString "PNothing"
      "just"
        @| pshow @(PMaybe PInteger) (pcon $ PJust $ pconstant @PInteger 42)
        @== pconstant @PString "PJust 42"
    "either" @\ do
      "right"
        @| pshow (pcon @(PEither PUnit PInteger) $ PRight 42)
        @== pconstant @PString "PRight 42"
    -- Test automatic injection of `(..)`.
    "maybe.either"
      @| pshow (pcon $ PJust $ pcon @(PEither PInteger PUnit) $ PLeft 42)
      @== pconstant @PString "PJust (PLeft 42)"
