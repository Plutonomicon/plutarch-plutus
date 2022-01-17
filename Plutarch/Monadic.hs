module Plutarch.Monadic ((>>=), (>>), fail) where

import Prelude hiding (fail, (>>), (>>=))

import Data.String (fromString)
import Plutarch.Prelude
import Plutarch.Trace (ptraceError)

(>>=) :: a -> a
(>>=) = id

(>>) :: a -> a
(>>) = id

fail :: String -> Term s a
fail msg = ptraceError (fromString msg)
