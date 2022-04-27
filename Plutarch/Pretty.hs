{-# LANGUAGE PatternSynonyms #-}

module Plutarch.Pretty (prettyTerm) where

import Data.Bifunctor (first)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Txt

import System.Random (
  Random (randomR),
  RandomGen,
  mkStdGen,
  uniformR,
 )

import Prettyprinter ((<+>))
import qualified Prettyprinter as PP

import Plutus.V1.Ledger.Scripts (Script (unScript))
import qualified PlutusCore as PLC
import UntypedPlutusCore (
  DeBruijn (DeBruijn),
  DefaultFun,
  DefaultUni,
  Index,
  Program (_progTerm),
  Term (Apply, Builtin, Constant, Delay, Error, Force, LamAbs, Var),
 )
import qualified UntypedPlutusCore as UPLC (Term)

import Plutarch.Internal.Other (ClosedTerm, compile)

prettyTerm :: ClosedTerm a -> PP.Doc ()
prettyTerm x = prettyUPLC . _progTerm . unScript $ compile x

{- This isn't suitable for pretty printing UPLC from any source. It's primarily suited for Plutarch output.

Practically speaking though, it should work with any _idiomatic_ UPLC.
-}
prettyUPLC :: UPLC.Term DeBruijn DefaultUni DefaultFun () -> PP.Doc ()
prettyUPLC = go False mempty $ mkStdGen 42
  where
    go :: RandomGen g => Bool -> Map Index Text -> g -> UPLC.Term DeBruijn DefaultUni DefaultFun () -> PP.Doc ()
    go _ _ _ (Constant _ c) = PP.pretty c
    go _ _ _ (Builtin _ b) = PP.pretty b
    go _ _ _ (Error _) = "ERROR"
    go _ nameMap _ (Var _ (DeBruijn x)) = case Map.lookup (x - 1) nameMap of
      Just nm -> PP.pretty nm
      Nothing -> error "impossible: free variable"
    go _ nameMap g (Force _ t@Apply {}) = "!" <> PP.parens (go False nameMap g t)
    go _ nameMap g (Force _ t@LamAbs {}) = "!" <> PP.parens (go False nameMap g t)
    go _ nameMap g (Force _ t) = "!" <> go False nameMap g t
    go _ nameMap g (Delay _ t@Apply {}) = "~" <> PP.parens (go False nameMap g t)
    go _ nameMap g (Delay _ t@LamAbs {}) = "~" <> PP.parens (go False nameMap g t)
    go _ nameMap g (Delay _ t) = "~" <> go False nameMap g t
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
                , go False (Map.mapKeys (+ (depth + 1)) nameMap <> Map.fromList (zip [0 .. depth] names)) finalG t
                ]
    go _ nameMap g (Apply _ (LamAbs _ _ t) arg) =
      let (l, ft) = unwrapBindings [] t
          args = arg : reverse l
          (names, _, finalG) =
            foldr
              ( \expr (l, existingNames, g) ->
                  let (newName, newG) = case expr of
                        Force _ (Force _ (Builtin _ b)) -> (fromString $ forcedPrefix <> show b, g)
                        Force _ (Builtin _ b) -> (fromString $ forcedPrefix <> show b, g)
                        PFixAst -> ("fix", g)
                        ComposeAST (Builtin () PLC.SndPair) (Builtin () PLC.UnConstrData) -> ("unDataSum", g)
                        ComposeAST (Var () (DeBruijn ix)) (Builtin () PLC.UnConstrData)
                          | ix == 2 -> ("unDataSum", g)
                        _ -> freshVarName existingNames g
                   in (newName : l, Set.insert newName existingNames, newG)
              )
              ([], Set.fromList $ Map.elems nameMap, g)
              args
          bindings = zip names args
          helper mp (name, expr) =
            PP.hang indentWidth $
              PP.sep
                [ PP.pretty name <+> "="
                , go False mp finalG expr
                ]
          (finalDoc, finalMap) =
            foldl'
              ( \(doc, mp) (name, expr) ->
                  let newDoc = doc <> PP.flatAlt PP.hardline "; " <> helper mp (name, expr)
                   in (newDoc, Map.mapKeys (+ 1) mp <> Map.singleton 0 name)
              )
              (helper nameMap (head bindings), Map.mapKeys (+ 1) nameMap <> Map.singleton 0 (fst $ head bindings))
              $ tail bindings
       in PP.align $
            PP.vsep
              [ "let" <+> PP.align finalDoc
              , "in" <+> go False finalMap finalG ft
              ]
    go fl nameMap g (Apply _ t arg) =
      (if fl then PP.parens else id) $
        let (l, f) = unwrapApply [] t
            args = l <> [arg]
         in PP.hang indentWidth $ PP.sep $ go False nameMap g f : (go True nameMap g <$> args)

forcedPrefix :: String
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

unwrapLamAbs :: Index -> UPLC.Term name uni fun ann -> (Index, UPLC.Term name uni fun ann)
unwrapLamAbs d (LamAbs _ _ t) = unwrapLamAbs (d + 1) t
unwrapLamAbs d a = (d, a)

unwrapBindings :: [UPLC.Term name uni fun ann] -> UPLC.Term name uni fun ann -> ([UPLC.Term name uni fun ann], UPLC.Term name uni fun ann)
unwrapBindings l (Apply _ (LamAbs _ _ t) arg) = unwrapBindings (arg : l) t
unwrapBindings l a = (l, a)

unwrapApply ::
  [UPLC.Term name uni fun ann] ->
  UPLC.Term name uni fun ann ->
  ([UPLC.Term name uni fun ann], UPLC.Term name uni fun ann)
unwrapApply l (Apply _ t arg) = unwrapApply (arg : l) t
unwrapApply l arg = (l, arg)

pattern PFixAst :: UPLC.Term name uni fun ()
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

pattern ComposeAST :: UPLC.Term DeBruijn uni fun () -> UPLC.Term DeBruijn uni fun () -> UPLC.Term DeBruijn uni fun ()
pattern ComposeAST f g <- LamAbs () _ (Apply () f (Apply () g (Var () (DeBruijn 1))))
