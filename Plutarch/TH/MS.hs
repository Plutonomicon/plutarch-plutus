{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoPartialTypeSignatures #-}

module Plutarch.TH.MS (deriveMS) where

import Data.Foldable (foldlM, foldrM)
import Data.Kind qualified as GHC
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Language.Haskell.TH (
  BndrVis,
  Body (NormalB),
  Con,
  Dec (ValD),
  Exp (AppE, AppTypeE, ConE, LetE, VarE),
  Name,
  Pat (VarP),
  Q,
  TyVarBndr,
  Type (VarT, WildCardT),
  newName,
 )
import Plutarch.Backend.S (S)
import Plutarch.Backend.Term (Term, punsafeCoerce)
import Plutarch.Primitive.Apply (PlutarchType (PRepresentation))
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
        pure $ plutarchTypeDec <> pmatchDec

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
