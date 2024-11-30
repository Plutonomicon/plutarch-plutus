{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.String (
  -- * Type
  PString,
  -- Functions
  pisHexDigit,
  pencodeUtf8,
  pdecodeUtf8,
) where

import Plutarch.Builtin.String (
  PString,
  pdecodeUtf8,
  pencodeUtf8,
 )

import Plutarch.Builtin.Bool (PBool, (#&&), (#||))
import Plutarch.Builtin.Integer (PInteger)
import Plutarch.Internal.Numeric ()
import Plutarch.Internal.Ord ((#<=))
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.Term (
  S,
  Term,
  phoistAcyclic,
  (:-->),
 )

{- | Verify if the given argument is the ASCII encoding of a hex digit. This
includes specifically the following ASCII ranges (inclusively):

* 48-54 (digits 0 through 9)
* 65-70 (upper-case A through upper-case F)
* 97-102 (lower-case a through lower-case f)

@since WIP
-}
pisHexDigit :: forall (s :: S). Term s (PInteger :--> PBool)
pisHexDigit = phoistAcyclic $ plam $ \c ->
  (c #<= 57 #&& 48 #<= c)
    #|| (c #<= 70 #&& 65 #<= c)
    #|| (c #<= 102 #&& 97 #<= c)
