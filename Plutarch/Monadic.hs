module Plutarch.Monadic ((>>=), (>>), fail) where

import Prelude hiding (fail, (>>), (>>=))

import Data.String (fromString)
import Plutarch.Prelude
import Plutarch.Trace (ptrace)

(>>=) :: a -> a
(>>=) = id

(>>) :: a -> a
(>>) = id

fail :: String -> Term s a
fail msg = ptrace (fromString msg) perror
