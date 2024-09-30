module Plutarch.Extra.ByteString (pisHexDigit) where

import Plutarch.Prelude

pisHexDigit :: Term s (PInteger :--> PBool)
pisHexDigit = phoistAcyclic $
  plam $ \chr ->
    (chr #<= 57 #&& 48 #<= chr)
      #|| (chr #<= 70 #&& 65 #<= chr)
      #|| (chr #<= 102 #&& 97 #<= chr)
