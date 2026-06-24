{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
-- I don't want to put all of this in the ANF module.
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Backend.Pretty (
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

-- We can do better than the UPLC Pretty instance.
-- REVIEW(@Koz): I get an incomplete cases warning in the helper here even though I think this
--               should cover all of the cases that can actually exist? I *think* it's
--               the exhaustiveness checker imploding (probably the kinds make it fail to identify
--               inaccessible branches, I'd guess?) but do check if you have a sec.
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
      other -> error ("I was wrong in `prettyValueOf`` and I missed a case that covers: " <> show other)

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

(<:=>) :: forall ann. Doc ann -> Doc ann -> Doc ann
d1 <:=> d2 = d1 <+> ":=" <+> d2
