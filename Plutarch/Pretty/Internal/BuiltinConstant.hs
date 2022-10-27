module Plutarch.Pretty.Internal.BuiltinConstant (prettyConstant) where

import Data.ByteString (ByteString)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text.Encoding qualified as TxtEnc

import Prettyprinter ((<+>))
import Prettyprinter qualified as PP

import PlutusCore qualified as PLC
import PlutusLedgerApi.V1 qualified as Plutus
import UntypedPlutusCore (DefaultUni)

import Plutarch.Pretty.Internal.Config (indentWidth)

prettyConstant :: PLC.Some (PLC.ValueOf DefaultUni) -> PP.Doc ()
prettyConstant (PLC.Some (PLC.ValueOf PLC.DefaultUniInteger n)) = PP.pretty n
prettyConstant (PLC.Some (PLC.ValueOf PLC.DefaultUniByteString b)) = PP.pretty $ encodeHex b
prettyConstant (PLC.Some (PLC.ValueOf PLC.DefaultUniString s)) =
  -- Have to `show` first to get a quoted string.
  PP.pretty $ show s
prettyConstant (PLC.Some (PLC.ValueOf PLC.DefaultUniUnit _)) = "()"
prettyConstant (PLC.Some (PLC.ValueOf PLC.DefaultUniBool b)) = PP.pretty b
prettyConstant (PLC.Some (PLC.ValueOf (PLC.DefaultUniList a) l)) =
  PP.list $
    fmap (prettyConstant . PLC.Some . PLC.ValueOf a) l
prettyConstant (PLC.Some (PLC.ValueOf (PLC.DefaultUniPair a b) ~(x, y))) =
  PP.tupled
    [prettyConstant . PLC.Some $ PLC.ValueOf a x, prettyConstant . PLC.Some $ PLC.ValueOf b y]
prettyConstant (PLC.Some (PLC.ValueOf PLC.DefaultUniData (Plutus.Constr ix dl))) =
  "Σ"
    <> PP.pretty ix
    <> "."
    <> PP.list (prettyConstant . PLC.Some . PLC.ValueOf PLC.DefaultUniData <$> dl)
prettyConstant (PLC.Some (PLC.ValueOf PLC.DefaultUniData (Plutus.Map ascList))) =
  PP.group
    . PP.encloseSep (PP.flatAlt "{ " "{") (PP.flatAlt " }" "}") ", "
    $ fmap
      ( \(a, b) ->
          PP.hang indentWidth $
            PP.sep
              [ prettyConstant (PLC.Some (PLC.ValueOf PLC.DefaultUniData a)) <+> "="
              , prettyConstant $ PLC.Some $ PLC.ValueOf PLC.DefaultUniData b
              ]
      )
      ascList
prettyConstant (PLC.Some (PLC.ValueOf PLC.DefaultUniData (Plutus.List l))) =
  "#" <> PP.list (prettyConstant . PLC.Some . PLC.ValueOf PLC.DefaultUniData <$> l)
prettyConstant (PLC.Some (PLC.ValueOf PLC.DefaultUniData (Plutus.B b))) =
  "#" <> prettyConstant (PLC.Some $ PLC.ValueOf PLC.DefaultUniByteString b)
prettyConstant (PLC.Some (PLC.ValueOf PLC.DefaultUniData (Plutus.I i))) =
  "#" <> prettyConstant (PLC.Some $ PLC.ValueOf PLC.DefaultUniInteger i)
prettyConstant (PLC.Some (PLC.ValueOf uni _)) =
  error $ "prettyConstant(impossible): " <> show uni

encodeHex :: ByteString -> Text
encodeHex = ("0x" <>) . TxtEnc.decodeUtf8 . LBS.toStrict . BSB.toLazyByteString . BSB.byteStringHex
