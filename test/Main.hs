module Main (main) where

import Bool qualified
import Compiled qualified
import Compose qualified
import Debug qualified
import Derivation qualified
import Error qualified
import Numeric qualified
import Pretty qualified
import Term qualified
import Test.Tasty (defaultMain, testGroup)
import Trace qualified

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
    , Trace.goldens
    , Debug.goldens
    ]
