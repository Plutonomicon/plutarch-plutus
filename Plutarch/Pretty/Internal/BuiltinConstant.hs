module Plutarch.Pretty.Internal.BuiltinConstant (prettyConstant) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text.Encoding as TxtEnc

import Prettyprinter ((<+>))
import qualified Prettyprinter as PP

import qualified Plutus.V1.Ledger.Api as Plutus
import qualified PlutusCore as PLC
import UntypedPlutusCore (DefaultUni)

import Plutarch.Pretty.Internal.Config (indentWidth)

prettyConstant :: PLC.Some (PLC.ValueOf DefaultUni) -> PP.Doc ()
prettyConstant (PLC.Some (PLC.ValueOf PLC.DefaultUniInteger n)) = PP.pretty n
prettyConstant (PLC.Some (PLC.ValueOf PLC.DefaultUniByteString b)) = PP.pretty $ fromHex b
prettyConstant (PLC.Some (PLC.ValueOf PLC.DefaultUniString s)) = PP.pretty $ show s
prettyConstant (PLC.Some (PLC.ValueOf PLC.DefaultUniUnit _)) = "()"
prettyConstant (PLC.Some (PLC.ValueOf PLC.DefaultUniBool b)) = PP.pretty b
prettyConstant (PLC.Some (PLC.ValueOf (PLC.DefaultUniList a) l)) =
  PP.list $
    map (prettyConstant . PLC.Some . PLC.ValueOf a) l
prettyConstant (PLC.Some (PLC.ValueOf (PLC.DefaultUniPair a b) ~(x, y))) =
  PP.tupled
    [prettyConstant . PLC.Some $ PLC.ValueOf a x, prettyConstant . PLC.Some $ PLC.ValueOf b y]
prettyConstant (PLC.Some (PLC.ValueOf PLC.DefaultUniData (Plutus.Constr ix dl))) =
  "Σ" <> PP.pretty ix <> "."
    <> PP.list (prettyConstant . PLC.Some . PLC.ValueOf PLC.DefaultUniData <$> dl)
prettyConstant (PLC.Some (PLC.ValueOf PLC.DefaultUniData (Plutus.Map ascList))) =
  PP.group
    . PP.encloseSep (PP.flatAlt "{ " "{") (PP.flatAlt " }" "}") ", "
    $ map
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

fromHex :: ByteString -> Text
fromHex = ("0x" <>) . TxtEnc.decodeUtf8 . LBS.toStrict . BSB.toLazyByteString . BSB.byteStringHex
