-- I don't want to put all of this in the ANF module.
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.Backend.PrettyANF (
  prettyShow,
  prettyANFAnnotations,
  prettyANFBinds,
  prettyANFHashes,
) where

import Plutarch.Backend.ANF
import Plutarch.Backend.AST (Hash (Hash), Multiplicity (MultiplicityMany, MultiplicityOne))
import Plutarch.Backend.UPLC (UPLCTerm (UPLCTerm))

import PlutusCore (Some (Some))
import Prettyprinter

import Data.Vector qualified as Vector
import Data.Vector.NonEmpty qualified as NEV

import Data.Text (Text)
import Data.Text qualified as T

import Data.Bimap (toAscList)

-- NOTE: Instance does not print the annotations, use `prettyANFAnnotations`
--       if you wish to see them.
--
--       It does print the hashes.
instance Pretty (ANF ann) where
  pretty anf =
    "ANF Hashes:"
      <> hardline
      <> indent 1 (align $ prettyANFHashes anf)
      <> hardline
      <> hardline
      <> "ANF Binds:"
      <> hardline
      <> indent 1 (align $ prettyANFBinds anf)
      <> hardline

-- I am probably going to reuse these two elsewhere so I might as well give them instances
-- (for hashes at least it's important they render the same since we aren't printing the Ints directly)
instance Pretty Id where
  pretty = prettyId

instance Pretty Hash where
  pretty = prettyHash

-- DOES THIS REALLY NOT EXIST SOMEWHERE?!
prettyShow :: forall a. Pretty a => a -> String
prettyShow = show . pretty

-- Just renders the binds, not the Bimap or the annotations.
-- NOTE: There isn't really a good way of formatting things so that it prints (arbitrary)
--       annotations bundled with the binds in a sufficiently pretty manner - if we don't know what the
--       annotations are supposed to look like we don't know where to put them or how to constrain them
--       in the layout. So if you want to see the annotations, use `prettyANFAnnotations`,
--       which prints them separately.
prettyANFBinds :: forall ann1 ann2. ANF ann1 -> Doc ann2
prettyANFBinds (ANF _ binds) = vcat . NEV.toList $ NEV.imap (\(Id -> i) b -> mkBind i b) binds
  where
    mkBind :: Id -> ANFBind ann1 -> Doc ann2
    mkBind i b = align . group $ prettyId i <:=> align (group $ prettyBind b)

prettyANFAnnotations :: forall ann1 ann2. Pretty ann1 => ANF ann1 -> Doc ann2
prettyANFAnnotations (ANF _ binds) = vcat . NEV.toList $ NEV.imap (\(Id -> i) b -> mkAnn i b) binds
  where
    mkAnn :: Id -> ANFBind ann1 -> Doc ann2
    mkAnn i b = align . group $ prettyId i <:=> align (group . pretty $ getANFBindAnn b)

prettyANFHashes :: forall ann1 ann2. ANF ann1 -> Doc ann2
prettyANFHashes (ANF hashes _) = vcat . map (\(i, h) -> prettyId i <:=> prettyHash h) . toAscList $ hashes

prettyBind :: forall ann1 ann2. ANFBind ann1 -> Doc ann2
prettyBind = \case
  ANFLeaf l -> prettyLeaf l
  ANFForce _ ref -> "!" <> prettyRef ref
  ANFDelay _ ref -> angles (prettyRef ref)
  ANFLam _ args body -> "\\" <> mkArgs args <+> "->" <+> prettyRef body
  ANFFix _ mult body -> "FIX" <> brackets (prettyMultiplicity mult) <+> prettyRef body
  ANFApply _ fnRef args -> hsep . punctuate " #" . fmap prettyRef $ (fnRef : NEV.toList args)
  ANFConstr _ cix args -> "CTOR" <+> viaShow cix <+> list (prettyRef <$> Vector.toList args)
  ANFCase _ scrut handlers -> "case" <+> prettyRef scrut <+> list (prettyRef <$> NEV.toList handlers)
  ANFCompose _ args -> hsep . punctuate " ." . fmap prettyRef . NEV.toList $ args
  where
    mkArgs :: NEV.NonEmptyVector (Maybe Multiplicity) -> Doc ann
    mkArgs (NEV.toList -> xs) =
      hsep
        . fmap (\case Nothing -> "_"; Just m -> prettyMultiplicity m)
        $ xs

prettyLeaf :: forall ann1 ann2. Leaf ann1 -> Doc ann2
prettyLeaf = \case
  LConstant _ (Some plcVal) ->
    -- the default instance can be ugly, TODO prettify it later
    pretty plcVal
  LBuiltin _ fun -> viaShow fun
  LCompiled _ (UPLCTerm uplc) -> pretty uplc
  LError _ -> "ERROR"

prettyRef :: forall ann. Ref -> Doc ann
prettyRef = \case
  AVar h -> prettyHash h
  AnId i -> prettyId i

prettyMultiplicity :: forall ann. Multiplicity -> Doc ann
prettyMultiplicity = \case
  MultiplicityOne h -> prettyHash h
  MultiplicityMany h -> prettyHash h

-- Hashing will give huge ints which are hard to read
-- so we turn them into something readable
prettyHash :: forall ann. Hash -> Doc ann
prettyHash (Hash h) = pretty . compactReadableVar . fromIntegral $ h

-- IDs should usually be small enough to easily read
prettyId :: Id -> Doc ann
prettyId (Id i) = "Id" <> brackets (viaShow i)

-- REVIEW: I am pretty sure that this actually gives us something unique for every Int,
-- because Integers should all have a unique `divMod` result, and it shouldn't really matter that
-- we're switching from (kind of) base 26 (to ensure it starts w/ a lowercase) to base 61 after the first
-- round of divMod.
--
-- But that's not a proof so I'm not absolutely certain.
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
