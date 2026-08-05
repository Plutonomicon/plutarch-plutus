module Main (main) where

import Array qualified
import Bool qualified
import Compiled qualified
import Compose qualified
import Derivation qualified
import Error qualified
import Numeric qualified
import Pretty qualified
import Term qualified
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain . testGroup "Goldens" $
    [ Term.goldens
    , Bool.goldens
    , Numeric.goldens
    , Compiled.goldens
    , Error.goldens
    , Compose.goldens
    , Pretty.goldens
    , Derivation.goldens
    , Array.goldens
    ]
