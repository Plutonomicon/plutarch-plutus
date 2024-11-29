{- Justification: default implementation of PIsData requires PlutusType
-}

module Plutarch.Internal.IsData where

import Data.Kind
import Plutarch.Builtin.ByteString
import Plutarch.Builtin.Data
import Plutarch.Internal.Term

class PIsData (a :: S -> Type)

-- Thanks IOG
instance PIsData PData
instance PIsData PByteString

pfromData :: PIsData a => Term s (PAsData a) -> Term s a
pdata :: PIsData a => Term s a -> Term s (PAsData a)
pforgetData :: forall s a. Term s (PAsData a) -> Term s PData
