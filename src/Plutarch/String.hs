{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.String (
  -- Functions
  pisHexDigit,
  pencodeUtf8,
  pdecodeUtf8,
) where

import Data.Text qualified as Text
import Plutarch.Bool ((#<=))
import Plutarch.ByteString (PByteString)
import Plutarch.Integer ()
import Plutarch.Internal.Builtin (
  PBool,
  PInteger,
  PString,
  plam,
  (#&&),
  (#||),
 )
import Plutarch.Internal.Term (S, Term, phoistAcyclic, (#), (:-->))
import Plutarch.Lift (pconstant)
import Plutarch.Unsafe (punsafeBuiltin)
import PlutusCore qualified as PLC

instance Semigroup (Term s PString) where
  x <> y = punsafeBuiltin PLC.AppendString # x # y

instance Monoid (Term s PString) where
  mempty = pconstant Text.empty

-- | Encode a 'PString' using UTF-8.
pencodeUtf8 :: Term s (PString :--> PByteString)
pencodeUtf8 = punsafeBuiltin PLC.EncodeUtf8

-- | Decode a 'PByteString' using UTF-8.
pdecodeUtf8 :: Term s (PByteString :--> PString)
pdecodeUtf8 = punsafeBuiltin PLC.DecodeUtf8

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
