{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoPartialTypeSignatures #-}

module Plutarch.TH.DataPlutus (
  deriveDataPlutus,
) where

import Data.Foldable (foldrM, traverse_)
import Data.Traversable.WithIndex (itraverse)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.NonEmpty qualified as NEVector
import Language.Haskell.TH (
  BndrVis,
  Body (NormalB),
  Con,
  Dec,
  Exp (AppE, CaseE, ConE, LitE, VarE),
  Lit (IntegerL),
  Match (Match),
  Name,
  Pat (ConP, VarP),
  Q,
  TyVarBndr,
  Type,
  newName,
 )
import Plutarch.Backend.Term (plam')
import Plutarch.Primitive.Apply (PlutarchType (PRepresentation))
import Plutarch.Primitive.BuiltinFun (pheadList)
import Plutarch.Primitive.CanData (PCanData)
import Plutarch.Primitive.Con (PCon (pcon'))
import Plutarch.Primitive.Data (PData)
import Plutarch.Primitive.Eq (PEq (peq))
import Plutarch.Primitive.Match (PMatch (pmatch'))
import Plutarch.Primitive.Pair (PBPair (PBPair))
import Plutarch.TH.Helpers (
  checkFieldIsWrapped,
  conToName,
  fullTypeName,
  getArity,
  hasNoFields,
  mkContextOf,
  mkUncons,
 )
import PlutusCore qualified as PLC

-- | @since wip
deriveDataPlutus :: Vector (TyVarBndr BndrVis) -> Name -> Vector Con -> Q [Dec]
deriveDataPlutus tvbs name constructors = case Vector.unsnoc constructors of
  Nothing -> fail "DataPlutus derivation is not possible for nullary types."
  Just (cs, c) ->
    if Vector.all hasNoFields constructors
      then fail "Use the Enum strategy for types with no fields in any 'arm'."
      else do
        traverse_ checkFieldIsWrapped constructors
        plutarchTypeDec <- derivePlutarchType tvbs name
        pmatchDec <- derivePMatch tvbs name cs c
        pconDec <- derivePCon tvbs name constructors
        peqDec <- derivePEq tvbs name
        pure $ plutarchTypeDec <> pmatchDec <> pconDec <> peqDec

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
      -- Generates `f C`, where `C` is the data constructor
      0 -> [e|$(pure (VarE contName)) $(pure (ConE conName))|]
      -- We have to do this in such a convoluted way because the continuation
      -- (`f` argument to `pmatch'`) has to be placed on the _inside_ of all
      -- of our list unconses. However, at the same time, we also have to
      -- build up a large application of our constructor `C`.
      _ -> go contName conName [] fieldsName (arity - 1)
    go :: Name -> Name -> [Name] -> Name -> Word -> Q Exp
    go contName cName headsNamesBackwards lastTailName = \case
      0 -> do
        -- We accumulate the heads needed in reverse order, because otherwise,
        -- this is a quadratic procedure. We can reverse in linear time.
        let headsNames = reverse headsNamesBackwards
        -- Build up applications of all heads to the constructor.
        conAppButLast <- foldrM (\headName acc -> AppE acc <$> [e|punsafeCoerce $(pure (VarE headName))|]) (ConE cName) headsNames
        -- Add the last argument by taking the head of the last tail.
        conAppE <- AppE conAppButLast <$> [e|punsafeCoerce (pheadList @PData # $(pure (VarE lastTailName)))|]
        -- Hit it with the continuation internally.
        pure . AppE (VarE contName) $ conAppE
      n -> mkUncons lastTailName $ \headName tailName ->
        -- We accumulate the needed names of all the heads we have to take
        -- so that we can apply them to the constructor 'all at once' at
        -- the end.
        go contName cName (headName : headsNamesBackwards) tailName (n - 1)

derivePCon :: Vector (TyVarBndr BndrVis) -> Name -> Vector Con -> Q [Dec]
derivePCon tyVars tyName constructors =
  [d|
    instance $ctx => PCon $name where
      pcon' x = $(matches 'x)
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf ''PCanData $ tyVars
    matches :: Name -> Q Exp
    matches bindName = CaseE (VarE bindName) . Vector.toList <$> itraverse mkMatch constructors
    mkMatch :: Int -> Con -> Q Match
    mkMatch conIx con = do
      let arity = getArity con
      conName <- conToName con
      fieldNames <- case arity of
        0 -> pure []
        n -> traverse (\i -> newName $ "f" <> show i) [0, 1 .. n - 1]
      let conMatchPat = ConP conName [] . fmap VarP $ fieldNames
      let constrIx = LitE . IntegerL . fromIntegral $ conIx
      constrE <- [e|punsafeConstant (PLC.someValue @Integer $(pure constrIx))|]
      start <- [e|pnilData|]
      constrList <- foldrM go start fieldNames
      matchBody <- [e|pconstrData # $(pure constrE) # $(pure constrList)|]
      pure . Match conMatchPat (NormalB matchBody) $ []
    go :: Name -> Exp -> Q Exp
    go fieldName acc = [e|pmkCons # pcoerce $(pure . VarE $ fieldName) # $(pure acc)|]

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
