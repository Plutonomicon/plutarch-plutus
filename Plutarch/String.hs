module Plutarch.String (PString, pfromText) where

import Data.String (IsString, fromString)
import qualified Data.Text as Txt
import Plutarch (punsafeBuiltin, punsafeConstant)
import Plutarch.Bool (PEq (..))
import Plutarch.Prelude
import qualified PlutusCore as PLC

data PString s

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
