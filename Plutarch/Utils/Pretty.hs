-- | Miscellaneous helpers for the various pretty printers.
module Plutarch.Utils.Pretty (
  prettyValueOf,
  compactReadableVar,
  (<:=>),
) where

import Data.ByteString ()
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as Vector
import Data.Vector.Strict qualified as SV
import PlutusCore.Default (DefaultUni (..), Esc)
import Prettyprinter (
  Doc,
  Pretty (pretty),
  brackets,
  list,
  parens,
  tupled,
  viaShow,
  (<+>),
 )

-- We can do better than the Plutus Pretty instance.
prettyValueOf :: forall (a :: Type) ann. DefaultUni (Esc a) -> a -> Doc ann
prettyValueOf uni x = case prettyUni uni of
  (uniDoc, prettyX) -> prettyX x <+> "::" <+> uniDoc
  where
    prettyUni :: forall (b :: Type) ann. DefaultUni (Esc b) -> (Doc ann, b -> Doc ann)
    prettyUni = \case
      DefaultUniInteger -> ("Integer", pretty)
      DefaultUniByteString -> ("Bytestring", viaShow)
      DefaultUniString -> ("String", pretty)
      DefaultUniUnit -> ("()", pretty)
      DefaultUniBool -> ("Bool", pretty)
      (DefaultUniProtoPair `DefaultUniApply` tX `DefaultUniApply` tY) ->
        let (innerX, fX) = prettyUni tX
            (innerY, fY) = prettyUni tY

            fXY (x, y) = tupled [fX x, fY y]
         in (tupled [innerX, innerY], fXY)
      DefaultUniApply DefaultUniProtoList uniA ->
        let (inner, f) = prettyUni uniA
         in (brackets inner, list . fmap f)
      DefaultUniApply DefaultUniProtoArray uniA ->
        let (inner, f) = prettyUni uniA
            f' = list . SV.toList . fmap f
         in case uniA of
              DefaultUniApply _ _ -> ("Array" <+> parens inner, f')
              _ -> ("Array" <+> inner, f')
      DefaultUniData -> ("Data", pretty)
      DefaultUniBLS12_381_G1_Element -> ("Bls12_381_G1_element", pretty)
      DefaultUniBLS12_381_G2_Element -> ("Bls12_381_G2_element", pretty)
      DefaultUniBLS12_381_MlResult -> ("Bls12_381_mlresult", pretty)
      DefaultUniValue -> ("Value", pretty)
      DefaultUniApply apF _ ->
        error $
          "Error: Could not prettify a DefaultUniApply because the LHS is: "
            <> show apF
            <> ", which "
            <> "is neither a DefaultUniProtoPair, DefaultUniProtoList, or DefaultUniProtoArray. "
            <> "The most likely cause of this is that a new polymorphic type was added to the "
            <> "default universe but `prettyValueOf` was not updated to support it."
            <> "\n"
            <> "If you are a Plutarch user, please open an issue or contact the maintainers."

{- The hashes are usually pretty large Ints, and would clutter up the prettified output
   even in hex, so we do something even more compact.

   The basic idea is that we do a first "round" of conversion in base 26 to get a lowercase letter,
   and then convert the remainder into base 61 using every alphanum char (except `N`, which
   we tack onto the end of negative values to disambiguate them from their positive absolute value)
-}
compactReadableVar :: Integer -> Text
compactReadableVar n
  | n < 0 = compactReadableVar (abs n) <> "N"
  | dn == 0 = T.singleton (lowers Vector.! fromIntegral mn)
  | otherwise = T.singleton (lowers Vector.! fromIntegral mn) <> go dn
  where
    (dn, mn) = n `divMod` 26

    go :: Integer -> Text
    go x = case x `divMod` 61 of
      (dx, mx) ->
        if dx == 0
          then T.singleton (allChars Vector.! fromIntegral mx)
          else
            T.singleton (allChars Vector.! fromIntegral mx)
              <> go dx

    lowers :: Vector.Vector Char
    lowers = Vector.fromList ['a' .. 'z']

    -- it would break uniqueness if we allowed 'N' to be part of the alphabet
    -- because we use it to indicate negatives
    allChars :: Vector.Vector Char
    allChars = lowers <> Vector.fromList (['A' .. 'M'] <> ['O' .. 'Z'] <> ['0' .. '9'])

(<:=>) :: forall (ann :: Type). Doc ann -> Doc ann -> Doc ann
d1 <:=> d2 = d1 <+> ":=" <+> d2
