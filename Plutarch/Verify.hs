module Verify ( PTryFrom (ptryFrom) ) where 

import Plutarch.ByteString (PByteString)
import Plutarch.Integer (PInteger)
import Plutarch.Internal.Other 
  ( POpaque
  , type (:-->)
  , Term 
  , plam
  )

class PTryFrom a where 
  ptryFrom :: Term s (POpaque :--> a)

instance PTryFrom PInteger where 
  ptryFrom = plam $ _

instance PTryFrom PByteString where 
  ptryFrom = plam $ _

{- TODO: 
    -> implement for
      - PBuiltinList 
      - PBuiltinPair 
    -> derive for 
      - PBuiltinMap
-}
