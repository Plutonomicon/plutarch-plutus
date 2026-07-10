-- | Miscellaneous helpers for the various pretty printers.
module Plutarch.Utils.Pretty (
  prettyValueOf,
  compactReadableVar,
  (<:=>),
  taggedNode,
  prettyAnnotated,
  blockList,
  prettyUPLC,
) where

import Control.Lens.Plated
import Data.ByteString ()
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as Vector
import Data.Vector.Strict qualified as SV
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
  (x : xs) -> flatAlt (go x xs) (list (x : xs))
  where
    go :: Doc ann -> [Doc ann] -> Doc ann
    go headEl rest = align . group . vcat $ ["[" <+> headEl] <> map ("," <+>) rest <> ["]"]

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

prettyUPLC :: forall ann. Term Name DefaultUni DefaultFun () -> Doc ann
prettyUPLC pt = case takeBindable ([], pt) of
  ([], rest) -> prettyNoBind rest
  (letBinds, rest) ->
    let pRest = "in" <+> prettyNoBind rest
     in align . vsep . reverse $ (pRest : letBinds)
  where
    -- if it's `arg` it came from Plutarch code and we just use the "compact pretty hash" as the visible name directly,
    -- but if it's something else then it came from a blob of compiled UPLC and we need to make the text part visible
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

    prettyNoBind :: Term Name DefaultUni DefaultFun () -> Doc ann
    prettyNoBind = \case
      Var () nm -> prettyName nm
      LamAbs () nm _body ->
        let (vars, body) = takeLamArgs ([prettyName nm], _body)
            oneLine = "\\" <> hsep vars <+> "->" <+> prettyNoBind body
            multiLine = "\\" <> hsep vars <+> "->" <> hardline <> indent 2 (prettyNoBind body)
         in align . group $ flatAlt multiLine oneLine
      Apply () f arg ->
        let fs = prettyAtomic <$> analyzeApp f
            allArgs = tail fs <> [prettyAtomic arg]
            funPart = head fs
            oneLine = parens $ funPart <> hcat (map (" # " <>) allArgs)
            multiLine = funPart <> hardline <> vcat (map ("# " <>) allArgs)
         in align . group $ flatAlt multiLine oneLine
      Force () inner -> "!" <> prettyAtomic inner
      Delay () inner -> angles $ prettyAtomic inner
      Constant _ (Some (ValueOf uni x)) -> parens (prettyValueOf uni x)
      Builtin _ b -> viaShow b
      Error {} -> "ERROR"
      Constr () cix args -> "constr" <+> pretty cix <+> align (group $ customList (prettyNoBind <$> args))
      Case () scrut handlers ->
        align . group $
          "case"
            <+> prettyAtomic scrut
            <+> hardline
            <> align
              ( group
                  (indent 2 . customList . fmap prettyNoBind . Vector.toList $ handlers)
              )

    isAtom :: Term Name DefaultUni DefaultFun () -> Bool
    isAtom = \case
      Var {} -> True
      Constant {} -> True
      Error {} -> True
      Delay {} -> True
      Force {} -> True
      Builtin {} -> True
      _ -> False

    prettyAtomic :: Term Name DefaultUni DefaultFun () -> Doc ann
    prettyAtomic = \case
      v@Var {} -> prettyNoBind v
      c@Constant {} -> prettyNoBind c
      e@Error {} -> prettyNoBind e
      d@Delay {} -> prettyNoBind d
      f@Force {} -> prettyNoBind f
      b@Builtin {} -> prettyNoBind b
      LamAbs () nm _body ->
        let (vars, body) = takeLamArgs ([prettyName nm], _body)
            cxt = "\\" <> hsep vars <+> "->"
            oneLine = align . parens $ cxt <+> prettyNoBind body
            multiLine = align . vcat $ ["(" <> cxt, indent 2 (prettyNoBind body), ")"]
         in group $ flatAlt multiLine oneLine
      Apply () f arg -> prettyAtomicApp f arg
      other -> blockParens . prettyNoBind $ other

    -- This is annoying
    prettyAtomicApp :: Term Name DefaultUni DefaultFun () -> Term Name DefaultUni DefaultFun () -> Doc ann
    prettyAtomicApp f arg
      | isAtom funPart = align . group $ flatAlt atomicMultiline defOneline
      | otherwise = align . group $ flatAlt defMultiline defOneline
      where
        funList = analyzeApp f <> [arg]
        pfunList = prettyAtomic <$> funList
        funPart = head funList
        argsPart = tail funList
        pfunPart = prettyAtomic funPart
        pArgs = prettyAtomic <$> argsPart
        defOneline = parens $ pfunPart <> hcat (map (" # " <>) pArgs)
        defMultiline = align . group $ "(" <+> hardline <+> indent 2 (align $ encloseSep "" "" "# " pfunList) <> hardline <> ")"
        atomicMultiline = align $ "(" <+> pfunPart <> hardline <> indent 2 (vcat (map ("# " <>) pArgs)) <> hardline <> ")"

    analyzeApp :: Term Name DefaultUni DefaultFun () -> [Term Name DefaultUni DefaultFun ()]
    analyzeApp = \case
      Apply () f arg -> analyzeApp f <> [arg]
      other -> [other]
