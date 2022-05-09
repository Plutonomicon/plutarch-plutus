{-# LANGUAGE PatternSynonyms #-}

module Plutarch.Pretty (prettyTerm, prettyScript, prettyConstant) where

import Control.Monad ((<=<))
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as LBS
import Data.List (find, foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Txt
import qualified Data.Text.Encoding as TxtEnc

import System.Random (
  Random (randomR),
  RandomGen,
  mkStdGen,
  uniformR,
 )

import Prettyprinter ((<+>))
import qualified Prettyprinter as PP

import Plutarch.Internal.Other (ClosedTerm, compile)
import Plutus.V1.Ledger.Scripts (Script (unScript))
import qualified PlutusCore as PLC
import qualified PlutusCore.Data as Plutus
import UntypedPlutusCore (
  DeBruijn (DeBruijn),
  DefaultFun,
  DefaultUni,
  Index,
  Program (_progTerm),
  Term (Apply, Builtin, Constant, Delay, Error, Force, LamAbs, Var),
 )

prettyScript :: Script -> PP.Doc ()
prettyScript = prettyUPLC . _progTerm . unScript

prettyTerm :: ClosedTerm a -> PP.Doc ()
prettyTerm x = prettyScript $ compile x

data PrettyState = Normal | Applying | AppliedOver | UnaryArg
  deriving stock (Bounded, Enum, Eq, Show)

{- This isn't suitable for pretty printing UPLC from any source. It's primarily suited for Plutarch output.

Practically speaking though, it should work with any _idiomatic_ UPLC.
-}
prettyUPLC :: Term DeBruijn DefaultUni DefaultFun () -> PP.Doc ()
prettyUPLC = go Normal mempty $ mkStdGen 42
  where
    go :: RandomGen g => PrettyState -> Map Index Text -> g -> Term DeBruijn DefaultUni DefaultFun () -> PP.Doc ()
    go _ _ _ (Constant _ c) = prettyConstant c
    go _ _ _ (Builtin _ b) = PP.pretty b
    go _ _ _ (Error _) = "ERROR"
    go _ nameMap _ (Var _ (DeBruijn x)) = case nameOfRef x nameMap of
      Just nm -> PP.pretty nm
      Nothing -> error "impossible: free variable"
    go fl nameMap g (IfThenElseLikeAST (Force () (Builtin () PLC.IfThenElse)) cond trueBranch falseBranch) =
      (if fl `elem` [Applying, AppliedOver, UnaryArg] then PP.parens else id) $
        prettyIfThenElse (go Normal nameMap g) cond trueBranch falseBranch
    go
      fl
      nameMap
      g
      ( IfThenElseLikeAST
          (Var () (DeBruijn (builtinFunAtRef nameMap -> Just PLC.IfThenElse)))
          cond
          trueBranch
          falseBranch
        ) =
        (if fl `elem` [Applying, AppliedOver, UnaryArg] then PP.parens else id) $
          prettyIfThenElse (go Normal nameMap g) cond trueBranch falseBranch
    go _ nameMap g (Force _ t@Apply {}) = "!" <> PP.parens (go Normal nameMap g t)
    go _ nameMap g (Force _ t@LamAbs {}) = "!" <> PP.parens (go Normal nameMap g t)
    go _ nameMap g (Force _ t) = "!" <> go UnaryArg nameMap g t
    go _ nameMap g (Delay _ t@Apply {}) = "~" <> PP.parens (go Normal nameMap g t)
    go _ nameMap g (Delay _ t@LamAbs {}) = "~" <> PP.parens (go Normal nameMap g t)
    go _ nameMap g (Delay _ t) = "~" <> go UnaryArg nameMap g t
    go _ nameMap g (LamAbs _ _ t') =
      PP.parens $
        let (depth, t) = unwrapLamAbs 0 t'
            (names, _, finalG) =
              foldl'
                ( \(l, existingNames, g) _ ->
                    let (newName, newG) = freshVarName existingNames g
                     in (newName : l, Set.insert newName existingNames, newG)
                )
                ([], Set.fromList $ Map.elems nameMap, g)
                [0 .. depth]
         in PP.hang indentWidth $
              PP.sep
                [ "\\" <> PP.hsep (reverse $ map PP.pretty names) <+> "->"
                , go Normal (Map.mapKeys (+ (depth + 1)) nameMap <> Map.fromList (zip [0 .. depth] names)) finalG t
                ]
    go _ nameMap g (Apply _ (LamAbs _ _ t) firstArg) =
      let (restArgs, coreF) = unwrapBindings [] t
          (firstName, nextG) = smartName nameMap g firstArg
          nextMap = Map.mapKeys (+ 1) nameMap <> Map.singleton 0 firstName
          (finalDoc, finalMap, finalG) =
            foldl'
              ( \(docAcc, mp, currG) argExpr ->
                  let (newName, newG) = smartName mp currG argExpr
                      newDoc = docAcc <> PP.flatAlt PP.hardline "; " <> helper mp (newName, argExpr)
                   in (newDoc, Map.mapKeys (+ 1) mp <> Map.singleton 0 newName, newG)
              )
              (helper nextMap (firstName, firstArg), nextMap, nextG)
              $ reverse restArgs
          helper mp (name, expr) =
            PP.hang indentWidth $
              PP.sep
                [ PP.pretty name <+> "="
                , go Normal mp nextG expr
                ]
       in PP.align $
            PP.vsep
              [ "let" <+> PP.align finalDoc
              , "in" <+> go Normal finalMap finalG coreF
              ]
    go fl nameMap g (Apply _ t arg) =
      (if fl == AppliedOver then PP.parens else id) $
        let (l, f) = unwrapApply [] t
            args = l <> [arg]
         in PP.hang indentWidth $ PP.sep $ go Applying nameMap g f : (go AppliedOver nameMap g <$> args)

prettyIfThenElse :: (t -> PP.Doc ann) -> t -> t -> t -> PP.Doc ann
prettyIfThenElse cont cond trueBranch falseBranch =
  PP.hang indentWidth $ PP.vsep ["if" <+> cont cond, "then" <+> cont trueBranch, "else" <+> cont falseBranch]

smartName :: RandomGen g => Map Index Text -> g -> Term DeBruijn uni DefaultFun () -> (Text, g)
smartName nameMap g = \case
  Force _ (Force _ (Builtin _ b)) -> (forcedPrefix <> showText b, g)
  Force _ (Builtin _ b) -> (forcedPrefix <> showText b, g)
  PFixAst -> ("fix", g)
  ComposeAST
    (Builtin () PLC.SndPair)
    (Builtin () PLC.UnConstrData) -> ("unDataSum", g)
  ComposeAST
    (Var () (DeBruijn (builtinFunAtRef nameMap -> Just PLC.SndPair)))
    (Builtin () PLC.UnConstrData) -> ("unDataSum", g)
  _ -> freshVarName (Set.fromList $ Map.elems nameMap) g

showText :: Show a => a -> Text
showText = Txt.pack . show

builtinFunAtRef :: Map Index Text -> Index -> Maybe DefaultFun
builtinFunAtRef nameMap = builtinFunFromName <=< flip nameOfRef nameMap

nameOfRef :: Index -> Map Index Text -> Maybe Text
nameOfRef ix = Map.lookup (ix - 1)

builtinFunFromName :: Text -> Maybe DefaultFun
builtinFunFromName res =
  if Txt.take prefixLen res == forcedPrefix
    then helper $ Txt.drop prefixLen res
    else helper res
  where
    prefixLen = Txt.length forcedPrefix
    helper s = find (\e -> showText e == s) builtinFunNames
    builtinFunNames = [minBound .. maxBound :: PLC.DefaultFun]

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

forcedPrefix :: Text
forcedPrefix = "fr"

freshVarName :: RandomGen g => Set Text -> g -> (Text, g)
freshVarName s' g' = if Set.member res s then freshVarName s' finalG else r
  where
    r@(res, finalG) =
      first Txt.pack $
        foldl'
          ( \(l, g) _ ->
              let (res, newG) = uniformR (1, Txt.length chars - 1) g
               in (Txt.index chars res : l, newG)
          )
          ([], g)
          [1 .. nameLen]
    (nameLen, g) = randomR (1 :: Int, 7) g'
    chars :: Text
    chars = "abcdefghijkmnprstuvwxyz"
    s = Set.union s' keywords

keywords :: Set Text
keywords =
  Set.fromList $
    ["let", "in"]
      <> map (fromString . show . PP.pretty) [(minBound @PLC.DefaultFun) .. maxBound]

indentWidth :: Int
indentWidth = 2

unwrapLamAbs :: Index -> Term name uni fun ann -> (Index, Term name uni fun ann)
unwrapLamAbs d (LamAbs _ _ t) = unwrapLamAbs (d + 1) t
unwrapLamAbs d a = (d, a)

unwrapBindings :: [Term name uni fun ann] -> Term name uni fun ann -> ([Term name uni fun ann], Term name uni fun ann)
unwrapBindings l (Apply _ (LamAbs _ _ t) arg) = unwrapBindings (arg : l) t
unwrapBindings l a = (l, a)

unwrapApply ::
  [Term name uni fun ann] ->
  Term name uni fun ann ->
  ([Term name uni fun ann], Term name uni fun ann)
unwrapApply l (Apply _ t arg) = unwrapApply (arg : l) t
unwrapApply l arg = (l, arg)

-- AST resulting from `pfix`. This is always constant.
pattern PFixAst :: Term name uni fun ()
pattern PFixAst <-
  LamAbs
    ()
    _
    ( Apply
        ()
        ( LamAbs
            ()
            _
            ( Apply
                ()
                (Var () _)
                ( LamAbs
                    ()
                    _
                    ( Apply
                        ()
                        ( Apply
                            ()
                            (Var () _)
                            (Var () _)
                          )
                        (Var () _)
                      )
                  )
              )
          )
        ( LamAbs
            ()
            _
            ( Apply
                ()
                (Var () _)
                ( LamAbs
                    ()
                    _
                    ( Apply
                        ()
                        ( Apply
                            ()
                            (Var () _)
                            (Var () _)
                          )
                        (Var () _)
                      )
                  )
              )
          )
      )

-- If `f` and `g` are Var references, their indices are incremented once since they are within a lambda.
pattern ComposeAST :: Term DeBruijn uni fun () -> Term DeBruijn uni fun () -> Term DeBruijn uni fun ()
pattern ComposeAST f g <- LamAbs () _ (Apply () (incrVar -> f) (Apply () (incrVar -> g) (Var () (DeBruijn 1))))

{- This AST represents a typical if/then/else usage if and only if 'ifThenElseMaybe' is either the
builtin IfThenElse (forced once), or a reference to such.
-}
pattern IfThenElseLikeAST ::
  Term name uni fun () ->
  Term name uni fun () ->
  Term name uni fun () ->
  Term name uni fun () ->
  Term name uni fun ()
pattern IfThenElseLikeAST ifThenElseMaybe cond trueBranch falseBranch <-
  Force
    ()
    ( Apply
        ()
        ( Apply
            ()
            ( Apply
                ()
                ifThenElseMaybe
                cond
              )
            (Delay () trueBranch)
          )
        (Delay () falseBranch)
      )

-- | Increment the debruijn index of a 'Var', leave any other AST node unchanged.
incrVar :: Term DeBruijn uni fun () -> Term DeBruijn uni fun ()
incrVar (Var () (DeBruijn n)) = Var () . DeBruijn $ n - 1
incrVar n = n
