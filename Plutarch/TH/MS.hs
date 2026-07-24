{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoPartialTypeSignatures #-}

module Plutarch.TH.MS (deriveMS) where

import Data.Foldable (foldl', foldlM, foldrM)
import Data.Kind qualified as GHC
import Data.Traversable.WithIndex (itraverse)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Language.Haskell.TH (
  BndrVis,
  Body (NormalB),
  Con,
  Dec (ValD),
  Exp (
    AppE,
    AppTypeE,
    CaseE,
    ConE,
    LamE,
    LetE,
    VarE
  ),
  Match (Match),
  Name,
  Pat (ConP, VarP, WildP),
  Q,
  TyVarBndr,
  Type (VarT, WildCardT),
  newName,
 )
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (Term, papp, plam', punsafeCoerce)
import Plutarch.Primitive.Apply (PlutarchType (PRepresentation))
import Plutarch.Primitive.Con (PCon (pcon'))
import Plutarch.Primitive.Function ((:-->))
import Plutarch.Primitive.Match (PMatch (pmatch'))
import Plutarch.TH.Helpers (
  conToFieldTypes,
  conToName,
  fullTypeName,
  getArity,
  hasNoFields,
  mkContextOf,
  mkPLam,
 )

-- | @since wip
deriveMS :: Vector (TyVarBndr BndrVis) -> Name -> Vector Con -> Q [Dec]
deriveMS tvbs name constructors = case Vector.unsnoc constructors of
  Nothing -> fail "Mogensen-Scott derivation is not possible for nullary types."
  Just (cs, c) ->
    if Vector.all hasNoFields constructors
      then fail "Use the Enum strategy for types with no fields in any 'arm'."
      else do
        plutarchTypeDec <- derivePlutarchType tvbs name
        pmatchDec <- derivePMatch tvbs name cs c
        pconDec <- derivePCon tvbs name constructors
        pure $ plutarchTypeDec <> pmatchDec <> pconDec

-- Helpers

derivePlutarchType :: Vector (TyVarBndr BndrVis) -> Name -> Q [Dec]
derivePlutarchType tyVars tyName =
  [d|
    instance $ctx => PlutarchType $name where
      type PRepresentation $name = $name
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf ''PlutarchType $ tyVars

derivePMatch :: Vector (TyVarBndr BndrVis) -> Name -> Vector Con -> Con -> Q [Dec]
derivePMatch tyVars tyName cs c =
  [d|
    instance $ctx => PMatch $name where
      pmatch' ::
        forall (b :: S -> GHC.Type) (s :: S).
        Term s $name -> ($name s -> Term s b) -> Term s b
      pmatch' x f = $(mkPMatchBody 'x 'f ''b)
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf ''PlutarchType $ tyVars
    mkPMatchBody :: Name -> Name -> Name -> Q Exp
    mkPMatchBody msThingName contName resTyName = do
      msType <- mkMSType (VarT resTyName) cs c
      msName <- newName "asMS"
      handlerLast <- mkHandler contName c
      handlersRest <- traverse (mkHandler contName) cs
      let handlers = Vector.snoc handlersRest handlerLast
      bodyAppE <- foldlM (\acc e -> [e|papp $(pure acc) $(pure e)|]) (VarE msName) handlers
      let coerceE = AppE (AppTypeE (AppTypeE (VarE 'punsafeCoerce) WildCardT) msType) . VarE $ msThingName
      pure . LetE [ValD (VarP msName) (NormalB coerceE) []] $ bodyAppE
    mkHandler :: Name -> Con -> Q Exp
    mkHandler contName con = do
      let cArity = getArity con
      cName <- conToName con
      case cArity of
        0 -> [e|$(pure . VarE $ contName) $(pure . VarE $ cName)|]
        _ -> go contName cName [] cArity
    go :: Name -> Name -> [Name] -> Word -> Q Exp
    go contName cName bindsNames = \case
      0 -> do
        let conAppE = foldr (\bindName acc -> AppE acc (VarE bindName)) (ConE cName) bindsNames
        pure . AppE (VarE contName) $ conAppE
      n -> mkPLam $ \bindName ->
        go contName cName (bindName : bindsNames) (n - 1)

derivePCon :: Vector (TyVarBndr BndrVis) -> Name -> Vector Con -> Q [Dec]
derivePCon tyVars tyName constructors =
  [d|
    instance $ctx => PCon $name where
      pcon' x = punsafeCoerce $(cases 'x)
    |]
  where
    name :: Q Type
    name = pure . fullTypeName tyName $ tyVars
    ctx :: Q Type
    ctx = pure . mkContextOf ''PlutarchType $ tyVars
    cases :: Name -> Q Exp
    cases argName = CaseE (VarE argName) . Vector.toList <$> itraverse mkMatch constructors
    mkMatch :: Int -> Con -> Q Match
    mkMatch conIx con = do
      let arity = getArity con
      conName <- conToName con
      fieldNames <- case arity of
        0 -> pure []
        n -> traverse (\i -> newName $ "f" <> show i) [0, 1 .. n - 1]
      let conMatchPat = ConP conName [] . fmap VarP $ fieldNames
      handlerName <- newName "handler"
      let conCount = Vector.length constructors
      let matchBody = go handlerName conIx fieldNames conCount 0
      pure . Match conMatchPat (NormalB matchBody) $ []
    go :: Name -> Int -> [Name] -> Int -> Int -> Exp
    go handlerName conIx fieldNames conCount depth
      | depth == conCount = foldl' (\acc -> AppE (AppE (VarE 'papp) acc) . VarE) (VarE handlerName) fieldNames
      | depth == conIx = AppE (VarE 'plam') . LamE [VarP handlerName] . go handlerName conIx fieldNames conCount $ depth + 1
      | otherwise = AppE (VarE 'plam') . LamE [WildP] . go handlerName conIx fieldNames conCount $ depth + 1

mkMSType :: Type -> Vector Con -> Con -> Q Type
mkMSType resType cs c = do
  lastConFunType <- mkConFunType resType c
  restConFunTypes <- traverse (mkConFunType resType) cs
  let allFunTypes = Vector.snoc restConFunTypes lastConFunType
  foldrM mkArrow resType allFunTypes

mkConFunType :: Type -> Con -> Q Type
mkConFunType rType c = do
  fieldTypes <- conToFieldTypes c
  foldrM mkArrow rType fieldTypes

mkArrow :: Type -> Type -> Q Type
mkArrow t acc = [t|$(pure t) :--> $(pure acc)|]
