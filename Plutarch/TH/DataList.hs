{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoPartialTypeSignatures #-}

module Plutarch.TH.DataList (
  deriveDataList,
) where

import Data.Foldable (foldrM, traverse_)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Language.Haskell.TH (
  BndrVis,
  Body (NormalB),
  Con,
  Dec,
  Exp (AppE, CaseE, ConE, VarE),
  Match (Match),
  Name,
  Pat (ConP, VarP),
  Q,
  TyVarBndr,
  Type,
  newName,
 )
import Plutarch.Helpers.TH (
  checkFieldIsWrapped,
  conToName,
  fullTypeName,
  getArity,
  hasNoFields,
  mkContextOf,
  mkUncons,
  pmkConsE,
  pnilDataE,
  punsafeCoerceE,
 )
import Plutarch.Primitive.Apply (PlutarchType (PRepresentation))
import Plutarch.Primitive.BuiltinFun (pheadList)
import Plutarch.Primitive.CanData (PCanData)
import Plutarch.Primitive.Con (PCon (pcon'))
import Plutarch.Primitive.Data (PData)
import Plutarch.Primitive.List (PBList)
import Plutarch.Primitive.Match (PMatch (pmatch'))

-- | @since wip
deriveDataList :: Vector (TyVarBndr BndrVis) -> Name -> Vector Con -> Q [Dec]
deriveDataList tvbs name constructors = case Vector.unsnoc constructors of
  Nothing -> fail "DataList derivation is not possible for nullary types."
  Just (cs, c) ->
    if Vector.all hasNoFields constructors
      then fail "Use the Enum strategy for types with no fields in any 'arm'."
      else
        if Vector.null cs
          then do
            traverse_ checkFieldIsWrapped constructors
            plutarchTypeDec <- derivePlutarchType tvbs name
            pmatchDec <- derivePMatch tvbs name c
            pconDec <- derivePCon tvbs name c
            pure $ plutarchTypeDec <> pmatchDec <> pconDec
          else fail "Cannot use DataList derivation for a type with multiple data constructors."

-- Helpers

derivePlutarchType :: Vector (TyVarBndr BndrVis) -> Name -> Q [Dec]
derivePlutarchType tyVars tyName =
  [d|
    instance $ctx => PlutarchType $name where
      type PRepresentation $name = PBList PData
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf ''PCanData $ tyVars

derivePMatch :: Vector (TyVarBndr BndrVis) -> Name -> Con -> Q [Dec]
derivePMatch tyVars tyName c =
  [d|
    instance $ctx => PMatch $name where
      pmatch' x f = $(mkMatchBody 'x 'f)
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf ''PCanData $ tyVars
    mkMatchBody :: Name -> Name -> Q Exp
    mkMatchBody listName contName = do
      let cArity = getArity c
      cName <- conToName c
      case cArity of
        -- Generates `f C`, where `C` is the data constructor.
        0 -> [e|$(pure (VarE contName)) $(pure (ConE cName))|]
        -- We have to do this in such a convoluted way because the continuation
        -- (`f` argument to `pmatch'`) has to be placed on the _inside_ of all
        -- of our list unconses. However, at the same time, we also have to
        -- build up a large application of our constructor `C`.
        _ -> go contName cName [] listName (cArity - 1)
    go :: Name -> Name -> [Name] -> Name -> Word -> Q Exp
    go contName cName headsNamesBackwards lastTailName = \case
      0 -> do
        -- We accumulate the heads needed in reverse order, because otherwise,
        -- this is a quadratic procedure. We can reverse in linear time.
        let headsNames = reverse headsNamesBackwards
        -- Build up applications of all heads to the constructor.
        conAppButLast <- foldrM (\headName acc -> AppE acc <$> [e|$punsafeCoerceE $(pure (VarE headName))|]) (ConE cName) headsNames
        -- Add the last argument by taking the head of the last tail.
        conAppE <- AppE conAppButLast <$> [e|$punsafeCoerceE (pheadList @PData # $(pure (VarE lastTailName)))|]
        -- Hit it with the continuation internally.
        pure . AppE (VarE contName) $ conAppE
      n -> mkUncons lastTailName $ \headName tailName ->
        -- We accumulate the needed names of all the heads we have to take
        -- so that we can apply them to the constructor 'all at once' at
        -- the end.
        go contName cName (headName : headsNamesBackwards) tailName (n - 1)

derivePCon :: Vector (TyVarBndr BndrVis) -> Name -> Con -> Q [Dec]
derivePCon tyVars tyName c =
  [d|
    instance $ctx => PCon $name where
      pcon' x = $(match 'x)
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf ''PCanData $ tyVars
    match :: Name -> Q Exp
    match bindName = CaseE (VarE bindName) . (: []) <$> mkMatch
    mkMatch :: Q Match
    mkMatch = do
      let cArity = getArity c
      cName <- conToName c
      fieldNames <- case cArity of
        0 -> pure []
        n -> traverse (\i -> newName $ "f" <> show i) [0, 1 .. n - 1]
      let conMatchPat = ConP cName [] . fmap VarP $ fieldNames
      start <- pnilDataE
      constrList <- foldrM go start fieldNames
      pure . Match conMatchPat (NormalB constrList) $ []
    go :: Name -> Exp -> Q Exp
    go fieldName acc = [e|$pmkConsE # pcoerce $(pure . VarE $ fieldName) # $(pure acc)|]
