{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoPartialTypeSignatures #-}

module Plutarch.TH.DataPlutus (
  deriveDataPlutus,
) where

import Control.Monad (replicateM)
import Data.Foldable (foldl', foldrM, for_, traverse_)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.NonEmpty qualified as NEVector
import Language.Haskell.TH (
  Bang,
  BndrVis,
  Con (InfixC, NormalC, RecC),
  Dec,
  Exp (AppE, ConE, LamE, VarE),
  Name,
  Pat (VarP),
  Q,
  TyVarBndr,
  Type (AppT, ConT),
  newName,
 )
import Plutarch.Backend.Term (plam')
import Plutarch.Primitive.Apply (PlutarchType (PRepresentation))
import Plutarch.Primitive.BuiltinFun (pheadList)
import Plutarch.Primitive.CanData (PCanData)
import Plutarch.Primitive.Data (PAsData, PData)
import Plutarch.Primitive.Eq (PEq (peq))
import Plutarch.Primitive.Match (PMatch (pmatch'))
import Plutarch.Primitive.Pair (PBPair (PBPair))
import Plutarch.TH.Helpers (conToName, fullTypeName, getArity, mkContextOf)

-- | @since wip
deriveDataPlutus :: Vector (TyVarBndr BndrVis) -> Name -> Vector Con -> Q [Dec]
deriveDataPlutus tvbs name constructors = case Vector.unsnoc constructors of
  Nothing -> fail "DataPlutus derivation is not possible for nullary types."
  Just (cs, c) -> do
    traverse_ checkFieldIsWrapped constructors
    plutarchTypeDec <- derivePlutarchType tvbs name
    pmatchDec <- derivePMatch tvbs name cs c
    peqDec <- derivePEq tvbs name
    pure $ plutarchTypeDec <> pmatchDec <> peqDec

-- Helpers

derivePlutarchType :: Vector (TyVarBndr BndrVis) -> Name -> Q [Dec]
derivePlutarchType tyVars tyName =
  [d|
    instance $ctx => PlutarchType $name where
      type PRepresentation $name = PData
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf ''PCanData $ tyVars

derivePMatch :: Vector (TyVarBndr BndrVis) -> Name -> Vector Con -> Con -> Q [Dec]
derivePMatch tyVars tyName cs c =
  [d|
    instance $ctx => PMatch $name where
      pmatch' x f = pmatch (punConstrData # x) $ \(PBPair tag fields) ->
        $(mkMatchBody 'f 'tag 'fields)
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf ''PCanData $ tyVars
    mkMatchBody :: Name -> Name -> Name -> Q Exp
    mkMatchBody contName tagName fieldsName = do
      let arityLast = getArity c
      nameLast <- conToName c
      let aritiesRest = fmap getArity cs
      namesRest <- traverse conToName cs
      handlerLast <- mkHandler fieldsName contName (nameLast, arityLast)
      handlersRest <- traverse (mkHandler fieldsName contName) (Vector.zip namesRest aritiesRest)
      start <- [e|NEVector.singleton (toSomeTerm $(pure handlerLast))|]
      handlers <- foldrM (\e acc -> [e|NEVector.cons (toSomeTerm $(pure e)) $(pure acc)|]) start handlersRest
      [e|punsafeCase $(pure (VarE tagName)) $(pure handlers)|]
    mkHandler :: Name -> Name -> (Name, Word) -> Q Exp
    mkHandler fieldsName contName (conName, arity) = case arity of
      0 -> [e|$(pure (VarE contName)) $(pure (ConE conName))|]
      1 -> [e|$(pure (VarE contName)) ($(pure (ConE conName)) (punsafeCoerce $(pure (VarE fieldsName))))|]
      _ -> do
        headTails <- replicateM (fromIntegral $ arity - 1) ((,) <$> newName "h" <*> newName "t")
        argEs <- toArgEs headTails
        let final = AppE (VarE contName) . foldl' AppE (ConE conName) $ argEs
        go final fieldsName headTails
    toArgEs :: [(Name, Name)] -> Q [Exp]
    toArgEs = \case
      [] -> pure []
      [(h, t)] -> (:) <$> [e|punsafeCoerce $(pure (VarE h))|] <*> ((: []) <$> [e|punsafeCoerce (pheadList @PData # $(pure (VarE t)))|])
      (h, _) : rest -> (:) <$> [e|punsafeCoerce $(pure (VarE h))|] <*> toArgEs rest
    go :: Exp -> Name -> [(Name, Name)] -> Q Exp
    go acc remaining = \case
      [] -> pure acc
      (h, t) : rest -> do
        let plams =
              AppE (VarE 'plam')
                . LamE [VarP h]
                . AppE (VarE 'plam')
                . LamE [VarP t]
                $ acc
        acc' <- [e|punsafeCase $(pure (VarE remaining)) (NEVector.singleton (toSomeTerm $(pure plams)))|]
        go acc' t rest

derivePEq :: Vector (TyVarBndr BndrVis) -> Name -> Q [Dec]
derivePEq tyVars tyName =
  [d|
    instance $ctx => PEq $name where
      peq = plam' $ \x -> plam' $ \y -> pequalsData # pcoerce x # pcoerce y
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf ''PCanData $ tyVars

checkFieldIsWrapped :: Con -> Q ()
checkFieldIsWrapped = \case
  NormalC name fields -> for_ fields (go name)
  RecC name fields -> for_ fields (goNamed name)
  InfixC lhs name rhs -> do
    go name lhs
    go name rhs
  _ -> fail "Unexpected constructor type found. If you see this message, report as a bug."
  where
    go :: Name -> (Bang, Type) -> Q ()
    go conName (_, t) =
      let errMsg =
            "Constructor "
              <> show conName
              <> "has a field whose type is not wrapped in 'PAsData'."
       in dig errMsg t
    goNamed :: Name -> (Name, Bang, Type) -> Q ()
    goNamed conName (fieldName, _, t) =
      let errMsg =
            "Constructor "
              <> show conName
              <> "has a field whose type is not wrapped in 'PAsData', specifically "
              <> show fieldName
              <> "."
       in dig errMsg t
    dig :: String -> Type -> Q ()
    dig errMsg = \case
      AppT _ (AppT (ConT t) _) ->
        if t == ''PAsData
          then pure ()
          else fail errMsg
      _ -> fail errMsg
