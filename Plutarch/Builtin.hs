module Plutarch.Builtin (PData (..), pfstBuiltin, psndBuiltin, pasConstr, phexByteStr, PBuiltinPair, PBuiltinList, PBuiltinByteString, PBuiltinString, pfromText) where

import qualified Data.ByteString as BS
import Data.Char (toLower)
import Data.String (IsString (..))
import qualified Data.Text as Txt
import Data.Word (Word8)
import GHC.Stack (HasCallStack)
import Plutarch (punsafeBuiltin, punsafeConstant)
import Plutarch.Bool (PEq (..), POrd (..))
import Plutarch.Integer (PInteger)
import Plutarch.Prelude
import qualified PlutusCore as PLC

data PBuiltinPair (a :: k -> Type) (b :: k -> Type) (s :: k)

data PBuiltinList (a :: k -> Type) (s :: k)

data PBuiltinByteString s

instance PEq PBuiltinByteString where
  x £== y = punsafeBuiltin PLC.EqualsByteString £ x £ y

instance POrd PBuiltinByteString where
  x £<= y = punsafeBuiltin PLC.LessThanEqualsByteString £ x £ y
  x £< y = punsafeBuiltin PLC.LessThanByteString £ x £ y

instance Semigroup (Term s PBuiltinByteString) where
  x <> y = punsafeBuiltin PLC.AppendByteString £ x £ y

instance Monoid (Term s PBuiltinByteString) where
  mempty = punsafeConstant . PLC.Some $ PLC.ValueOf PLC.DefaultUniByteString BS.empty

-- | Interpret a hex string as a PBuiltinByteString.
phexByteStr :: HasCallStack => String -> Term s PBuiltinByteString
phexByteStr = punsafeConstant . PLC.Some . PLC.ValueOf PLC.DefaultUniByteString . BS.pack . f
  where
    f "" = []
    f [_] = error "UnevenLength"
    f (x : y : rest) = (hexDigitToWord8 x * 16 + hexDigitToWord8 y) : f rest

data PBuiltinString s

pfromText :: Txt.Text -> Term s PBuiltinString
pfromText = punsafeConstant . PLC.Some . PLC.ValueOf PLC.DefaultUniString

instance IsString (Term s PBuiltinString) where
  fromString = punsafeConstant . PLC.Some . PLC.ValueOf PLC.DefaultUniString . Txt.pack

instance PEq PBuiltinString where
  x £== y = punsafeBuiltin PLC.EqualsString £ x £ y

instance Semigroup (Term s PBuiltinString) where
  x <> y = punsafeBuiltin PLC.AppendString £ x £ y

instance Monoid (Term s PBuiltinString) where
  mempty = punsafeConstant . PLC.Some $ PLC.ValueOf PLC.DefaultUniString Txt.empty

data PData s
  = PDataConstr (Term s (PBuiltinPair PInteger (PBuiltinList PData)))
  | PDataMap (Term s (PBuiltinList (PBuiltinPair PData PData)))
  | PDataList (Term s (PBuiltinList PData))
  | PDataInteger (Term s PInteger)
  | PDataByteString (Term s PBuiltinByteString)

pfstBuiltin :: Term s (PBuiltinPair a b :--> a)
pfstBuiltin = phoistAcyclic $ pforce . pforce . punsafeBuiltin $ PLC.FstPair

psndBuiltin :: Term s (PBuiltinPair a b :--> b)
psndBuiltin = phoistAcyclic $ pforce . pforce . punsafeBuiltin $ PLC.SndPair

pasConstr :: Term s (PData :--> PBuiltinPair PInteger (PBuiltinList PData))
pasConstr = punsafeBuiltin PLC.UnConstrData

hexDigitToWord8 :: HasCallStack => Char -> Word8
hexDigitToWord8 = f . toLower
  where
    f '0' = 0
    f '1' = 1
    f '2' = 2
    f '3' = 3
    f '4' = 4
    f '5' = 5
    f '6' = 6
    f '7' = 7
    f '8' = 8
    f '9' = 9
    f 'a' = 10
    f 'b' = 11
    f 'c' = 12
    f 'd' = 13
    f 'e' = 14
    f 'f' = 15
    f c = error $ "InvalidHexDigit " ++ [c]
