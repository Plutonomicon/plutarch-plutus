module Plutarch.Monadic ((>>=), (>>), fail) where

import Prelude hiding (fail, (>>), (>>=))

import Data.String (fromString)
import Plutarch.Prelude
import Plutarch.Trace (ptraceError)

-- | @P.do { y <- x ; z }@ is equivalent to @x $ \y -> z@.
(>>=) :: (x -> Term s a) -> x -> Term s a
(>>=) = id

-- | @P.do { x ; y }@ is equivalent to @x y@.
(>>) :: (x -> Term s a) -> x -> Term s a
(>>) = id

-- | On a partial pattern match, `ptraceError` is called automatically.
fail :: String -> Term s a
fail msg = ptraceError (fromString msg)
