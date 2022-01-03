module Plutarch.String (PString, pfromText, pencodeUtf8, pdecodeUtf8) where

import Data.String (IsString, fromString)
import qualified Data.Text as Txt
import Plutarch (punsafeBuiltin, punsafeConstant)
import Plutarch.Bool (PEq, (#==))
import Plutarch.ByteString (PByteString)
import Plutarch.Prelude
import qualified PlutusCore as PLC

-- | Plutus 'BuiltinString' values
data PString s

-- | Create a PString from 'Text'
pfromText :: Txt.Text -> Term s PString
pfromText = punsafeConstant . PLC.Some . PLC.ValueOf PLC.DefaultUniString

instance IsString (Term s PString) where
  fromString = punsafeConstant . PLC.Some . PLC.ValueOf PLC.DefaultUniString . Txt.pack

instance PEq PString where
  x #== y = punsafeBuiltin PLC.EqualsString # x # y

instance Semigroup (Term s PString) where
  x <> y = punsafeBuiltin PLC.AppendString # x # y

instance Monoid (Term s PString) where
  mempty = punsafeConstant . PLC.Some $ PLC.ValueOf PLC.DefaultUniString Txt.empty

-- | Encode a 'PString' using UTF-8.
pencodeUtf8 :: Term s (PString :--> PByteString)
pencodeUtf8 = punsafeBuiltin PLC.EncodeUtf8

-- | Decode a 'PByteString' using UTF-8.
pdecodeUtf8 :: Term s (PByteString :--> PString)
pdecodeUtf8 = punsafeBuiltin PLC.DecodeUtf8
