module Plutarch.Monadic ((>>=), fail) where

import Prelude hiding (fail, (>>=))

import Data.String (fromString)
import Plutarch (PMatch)
import Plutarch.Prelude
import Plutarch.Trace (ptrace)

(>>=) :: PMatch a => Term s a -> (a s -> Term s b) -> Term s b
(>>=) = pmatch

fail :: String -> Term s a
fail msg = ptrace (fromString msg) perror
