-- | Miscellaneous helpers for the various pretty printers.
module Plutarch.Utils.Pretty (
  prettyValueOf,
  compactReadableVar,
  (<:=>),
  taggedNode,
  prettyAnnotated,
  blockList,
  customList,
  prettyUPLC,
  oneLineList,
  blockParens,
  lambdaTemplate,
  appTemplate,
  caseTemplate,
  ctorTemplate,
  composeTemplate,
  letTemplate,
  PrintMode (PrintDefault, PrintAtomic),
) where

import Control.Lens.Plated
import Data.ByteString ()
import Data.Foldable (toList)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as Vector
import Data.Vector.NonEmpty (NonEmptyVector)
import Data.Vector.NonEmpty qualified as NEVector
import Data.Vector.Strict qualified as SV
import Data.Word (Word64)
import PlutusCore.Default (DefaultUni (..), Esc, Some (Some), ValueOf (ValueOf))
import Prettyprinter (
  Doc,
  Pretty (pretty),
  align,
  angles,
  brackets,
  encloseSep,
  flatAlt,
  group,
  hardline,
  hcat,
  hsep,
  indent,
  list,
  parens,
  punctuate,
  tupled,
  vcat,
  viaShow,
  vsep,
  (<+>),
 )
import UntypedPlutusCore (DefaultFun, Name (Name), Unique (Unique))
import UntypedPlutusCore.Core.Type (Term (Apply, Builtin, Case, Constant, Constr, Delay, Error, Force, LamAbs, Var))

-- We can do better than the Plutus Pretty instance.
-- If we use (_,_) for pairs and [] for list types then
-- we only need to care about parens for nested arrays
-- because everything else is fully disambiguated by default.
-- NOTE: If they ever add a new polymorphic default universe type
--       someone will have to do something more clever here.
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
              DefaultUniApply DefaultUniProtoArray _ -> ("Array" <+> parens inner, f')
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

_customLine :: Doc ann
_customLine = flatAlt hardline ""

-- arg is a "tag" for the AST node type, next two two args are the left and right block separator
block' :: forall (ann :: Type). Doc ann -> Doc ann -> Doc ann -> Doc ann -> Doc ann
block' lbl l' r d = group $ flatAlt multiLine oneLine
  where
    l :: Doc ann
    l = l' <> lbl

    multiLine :: Doc ann
    multiLine = align $ l <> hardline <> indent 2 (align . group $ d) <> hardline <> r

    oneLine :: Doc ann
    oneLine = align $ l <> align d <> r

block :: forall (ann :: Type). Doc ann -> Doc ann -> Doc ann -> Doc ann
block = block' ""

-- This aligns the `[` and `]` at the same indentation level to make this easier to read
blockList :: forall (ann :: Type). [Doc ann] -> Doc ann
blockList = block "[" "]" . vcat . punctuate ", "

customList :: forall (ann :: Type). [Doc ann] -> Doc ann
customList = \case
  [] -> "[]"
  (x : xs) -> group $ flatAlt (go x xs) (oneLineList (x : xs))
  where
    go :: Doc ann -> [Doc ann] -> Doc ann
    go headEl rest = align . group . vcat $ ["[" <+> headEl] <> map ("," <+>) rest <> ["]"]

oneLineList :: forall (ann :: Type). [Doc ann] -> Doc ann
oneLineList = \case
  [] -> "[]"
  xs -> "[" <> hcat (punctuate ", " xs) <> "]"

blockParens :: forall (ann :: Type). Doc ann -> Doc ann
blockParens = block "(" ")"

taggedNode :: forall (ann :: Type). Doc ann -> Doc ann -> Doc ann -> Doc ann
taggedNode lbl ann node = block' lbl "<" ">" node <> "@" <> ann

prettyAnnotated ::
  forall f a ann.
  (Plated (f a), Pretty a) =>
  (f a -> a) ->
  (Doc ann -> Doc ann -> Doc ann) -> -- ann doc is first arg
  (forall x. f x -> [Doc ann] -> Doc ann) ->
  f a ->
  Doc ann
prettyAnnotated getAnn annHandler nodePrinter x = annHandler (pretty a) $ nodePrinter x childNodesPretty
  where
    childNodesPretty = prettyAnnotated getAnn annHandler nodePrinter <$> childNodes
    a = getAnn x
    childNodes = children x

data PrintMode = PrintAtomic | PrintDefault

lambdaTemplate :: forall (ann :: Type). PrintMode -> [Doc ann] -> Doc ann -> Doc ann
lambdaTemplate mode vars body = case mode of
  PrintAtomic -> align . group $ flatAlt (mkMultiline "(" ")") (parens oneLineNoParens)
  PrintDefault -> align . group $ flatAlt (mkMultiline "" "") oneLineNoParens
  where
    myLine = case mode of PrintAtomic -> hardline; _ -> ""
    cxt = "\\" <> hsep vars <+> "->"
    mkMultiline l r =
      align . group $
        l
          <> cxt
          <> hardline
          <> indent 2 body
          <> myLine
          <> r
    oneLineNoParens = "\\" <> hsep vars <+> "->" <+> body

-- | Don't pass an empty list into this
appLike :: forall (ann :: Type). Doc ann -> PrintMode -> Bool -> NonEmptyVector (Doc ann) -> Doc ann
appLike op mode funIsSmall funList@(NEVector.uncons -> (fun, args)) = case mode of
  PrintAtomic -> align . group $ flatAlt (mkMultiline "(" ")") (parens oneLineNoParens)
  PrintDefault -> align . group $ flatAlt (mkMultiline "" "") oneLineNoParens
  where
    myLine = case mode of PrintAtomic -> hardline; _ -> ""
    -- this is the "small" variant
    mkMultiline l r =
      if funIsSmall
        then
          align . group $
            l
              <+> fun
              <> hardline
              <> indent 2 (vcat (map (op <+>) $ toList args))
              <> myLine
              <> r
        else
          align . group $
            l
              <+> myLine
              <> indent 2 (align . encloseSep "" "" (op <> " ") $ toList funList)
              <> myLine
              <> r
    oneLineNoParens = fun <> hcat (map ((" " <> op <> " ") <>) $ toList args)

appTemplate :: PrintMode -> Bool -> NonEmptyVector (Doc ann) -> Doc ann
appTemplate = appLike "#"

composeTemplate :: PrintMode -> NonEmptyVector (Doc ann) -> Doc ann
composeTemplate mode = appLike "." mode False

-- Var -> Binding -> Body -> Result
letTemplate :: forall (ann :: Type). PrintMode -> Doc ann -> Doc ann -> Doc ann -> Doc ann
letTemplate mode var bind body = case mode of
  PrintDefault -> align . group $ flatAlt (mkMultiline "" "") oneLineNoParens
  PrintAtomic -> align . group $ flatAlt (mkMultiline "(" ")") (parens oneLineNoParens)
  where
    myLine :: Doc ann
    myLine = case mode of PrintAtomic -> hardline; _ -> ""

    mkMultiline :: Doc ann -> Doc ann -> Doc ann
    mkMultiline l r =
      align . group $
        l
          <> "let"
          <+> var
          <+> "="
          <+> align (group bind)
          <> hardline
          <> "in"
          <+> align (group body)
          <> myLine
          <> r

    oneLineNoParens :: Doc ann
    oneLineNoParens = "let" <+> var <+> "=" <+> bind <+> "in" <+> body

caseTemplate :: forall (ann :: Type). PrintMode -> Bool -> Doc ann -> [Doc ann] -> Doc ann
caseTemplate mode scrutIsSmall scrut handlers = case mode of
  PrintAtomic -> align . group $ flatAlt (mkMultiline "(" ")") (parens oneLineNoParens)
  PrintDefault -> align . group $ flatAlt (mkMultiline "" "") oneLineNoParens
  where
    myLine :: Doc ann
    myLine = case mode of PrintAtomic -> hardline; _ -> ""

    mkMultiline :: Doc ann -> Doc ann -> Doc ann
    mkMultiline l r =
      if scrutIsSmall
        then
          align . group $
            l
              <> "case"
              <+> scrut
              <> hardline
              <> indent 2 (customList handlers)
              <> myLine
              <> r
        else
          align . group $
            l
              <> "case"
              <+> hardline
              <> indent 2 (align . vcat $ [scrut, customList handlers])
              <> myLine
              <> r

    oneLineNoParens :: Doc ann
    oneLineNoParens = "case" <+> scrut <+> oneLineList handlers

ctorTemplate :: forall (ann :: Type). PrintMode -> Word64 -> [Doc ann] -> Doc ann
ctorTemplate mode cix args = case mode of
  PrintDefault -> align . group $ flatAlt (mkMultiline "" "") oneLineNoParens
  PrintAtomic -> align . group $ flatAlt (mkMultiline "(" ")") (parens oneLineNoParens)
  where
    myLine :: Doc ann
    myLine = case mode of PrintAtomic -> hardline; _ -> ""

    mkMultiline :: Doc ann -> Doc ann -> Doc ann
    mkMultiline l r =
      align $
        l
          <> "constr"
          <+> pretty cix
          <> hardline
          <> indent 2 (customList args)
          <> myLine
          <> r

    oneLineNoParens :: Doc ann
    oneLineNoParens = "constr" <+> pretty cix <+> oneLineList args

-- Yes there are probably a bunch of superfluous `align`s, not worth the trouble to sort out which are safe to remove tho
prettyUPLC :: forall ann. Term Name DefaultUni DefaultFun () -> Doc ann
prettyUPLC pt = case takeBindable ([], topLevelBody) of
  ([], rest) -> case topLevelArgs of
    [] -> prettyNoBind rest
    _ -> lambdaTemplate PrintDefault topLevelArgs $ prettyNoBind rest
  (letBinds, rest) ->
    let pRest = "in" <+> prettyNoBind rest
        body = align . vsep . reverse $ (pRest : letBinds)
     in lambdaTemplate PrintDefault topLevelArgs body
  where
    (topLevelArgs, topLevelBody) = takeLamArgs ([], pt)
    -- if it's `arg` it came from Plutarch code and we just use the "compact pretty hash" as the visible name directly,
    -- but if it's something else then it came from a blob of compiled UPLC OR has a semantically meaningful text part
    -- so we need to make the text part visible
    prettyName :: Name -> Doc ann
    prettyName (Name txt (Unique u))
      | txt == "arg" = pretty . compactReadableVar . fromIntegral $ u
      | otherwise = pretty txt <> "_" <> pretty (compactReadableVar . fromIntegral $ u)

    takeBindable :: ([Doc ann], Term Name DefaultUni DefaultFun ()) -> ([Doc ann], Term Name DefaultUni DefaultFun ())
    takeBindable (acc, t) = case t of
      Apply () (LamAbs () nm body) arg ->
        let here = "let" <+> prettyName nm <+> "=" <+> align (prettyNoBind arg)
         in takeBindable (here : acc, body)
      other -> (acc, other)

    takeLamArgs :: ([Doc ann], Term Name DefaultUni DefaultFun ()) -> ([Doc ann], Term Name DefaultUni DefaultFun ())
    takeLamArgs (varAcc, next) = case next of
      LamAbs () nm body -> takeLamArgs (prettyName nm : varAcc, body)
      _ -> (reverse varAcc, next)

    go :: PrintMode -> Term Name DefaultUni DefaultFun () -> Doc ann
    go mode = \case
      Var () nm -> prettyName nm
      LamAbs () nm _body ->
        let (vars, body) = takeLamArgs ([prettyName nm], _body)
         in lambdaTemplate mode vars (prettyNoBind body)
      Apply () f arg ->
        let funList = prettyAtomic <$> (analyzeApp f <> NEVector.singleton arg)
         in align . group $ appTemplate mode (isAtom f) funList
      Force () inner -> "!" <> prettyAtomic inner
      Delay () inner -> angles $ prettyNoBind inner
      Constant _ (Some (ValueOf uni x)) -> parens (prettyValueOf uni x)
      Builtin _ b -> viaShow b
      Error {} -> "ERROR"
      Constr () cix args -> ctorTemplate mode cix (prettyNoBind <$> args)
      Case () scrut handlers ->
        caseTemplate mode (isAtom scrut) (prettyAtomic scrut) (prettyNoBind <$> Vector.toList handlers)

    prettyAtomic :: Term Name DefaultUni DefaultFun () -> Doc ann
    prettyAtomic = go PrintAtomic

    prettyNoBind :: Term Name DefaultUni DefaultFun () -> Doc ann
    prettyNoBind = go PrintDefault

    isAtom :: Term Name DefaultUni DefaultFun () -> Bool
    isAtom = \case
      Var {} -> True
      Constant {} -> True
      Error {} -> True
      Delay _ inner -> isAtom inner
      Force _ inner -> isAtom inner
      Builtin {} -> True
      _ -> False

    analyzeApp :: Term Name DefaultUni DefaultFun () -> NonEmptyVector (Term Name DefaultUni DefaultFun ())
    analyzeApp = \case
      Apply () f arg -> analyzeApp f <> NEVector.singleton arg
      other -> NEVector.singleton other
