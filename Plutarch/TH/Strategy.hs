{-# LANGUAGE TemplateHaskellQuotes #-}

module Plutarch.TH.Strategy (
  Strategy (..),
  deriveFor,
) where

import Data.Foldable (foldl', foldlM, foldrM, for_)
import Data.Traversable.WithIndex (itraverse)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.NonEmpty qualified as NEVector
import Language.Haskell.TH (
  Bang,
  BndrVis,
  Body (NormalB),
  Con (ForallC, GadtC, InfixC, NormalC, RecC, RecGadtC),
  Dec (DataD, NewtypeD, TySynD),
  Exp (AppE, CaseE, ConE, LamE, LitE, VarE),
  Info (TyConI),
  Lit (IntegerL),
  Match (Match),
  Name,
  Pat (ConP, VarP, WildP),
  Q,
  TyVarBndr (KindedTV, PlainTV),
  Type (AppT, ConT, TupleT, VarT),
  newName,
  reify,
 )
import Plutarch.Backend.Term (Term, plam', punsafeConstr)
import Plutarch.Primitive.Apply (PlutarchType (PRepresentation))
import Plutarch.Primitive.Bool (pand)
import Plutarch.Primitive.Con (PCon (pcon'))
import Plutarch.Primitive.Eq (PEq (peq))
import Plutarch.Primitive.Match (PMatch (pmatch'))
import Plutarch.Primitive.SOP (PSOP)

-- | @since wip
data Strategy = SOP

-- | @since wip
deriveFor :: Name -> Strategy -> Q [Dec]
deriveFor tyName strat = do
  tyDec <- checkTyName tyName
  case tyDec of
    DataD _ name tyVarBinds _ constructors _ -> case strat of
      SOP -> do
        let tvbAsVec = Vector.fromList tyVarBinds
        let consAsVec = Vector.fromList constructors
        for_ consAsVec checkFieldsAreTermsSOP
        case Vector.unsnoc tvbAsVec of
          Nothing -> fail "Types must have an s :: S type parameter in last position."
          Just (tvbs, _) -> case Vector.unsnoc consAsVec of
            Nothing -> fail "Nullary types do not support an SOP derivation strategy."
            Just (cs, c) ->
              if all hasNoFields constructors
                then fail "Use the Enum strategy for types with no fields in any 'arm'."
                else do
                  plutarchTypeDec <- deriveSOPPlutarchType tvbs name
                  pmatchDec <- deriveSOPPMatch tvbs name cs c
                  pconDec <- deriveSOPPCon tvbs name consAsVec
                  peqDec <- deriveSOPPEq tvbs name consAsVec
                  pure $ plutarchTypeDec <> pmatchDec <> pconDec <> peqDec
    NewtypeD {} -> case strat of
      SOP -> fail "Use the Newtype strategy for newtypes."
    TySynD {} -> fail "Type synonyms are not supported. Use the underlying type."
    _ -> fail "Not a valid type name."

-- Helpers

deriveSOPPEq :: Vector (TyVarBndr BndrVis) -> Name -> Vector Con -> Q [Dec]
deriveSOPPEq tyVars tyName constructors =
  [d|
    instance $ctx => PEq $name where
      peq = plam' $ \x -> plam' $ \y -> pmatch x $ \xInner ->
        pmatch y $ \yInner ->
          $(peqImpl 'xInner 'yInner)
    |]
  where
    name :: Q Type
    name = pure $ foldl' (\acc -> AppT acc . VarT . bindToName) (ConT tyName) tyVars
    ctx :: Q Type
    ctx = do
      let len = Vector.length tyVars
      let varNames = fmap (AppT (ConT ''PEq) . VarT . bindToName) tyVars
      pure $ foldl' AppT (TupleT len) varNames
    peqImpl :: Name -> Name -> Q Exp
    peqImpl xName yName = do
      matches <- Vector.toList <$> traverse (mkMatch yName) constructors
      pure . CaseE (VarE xName) $ matches
    mkMatch :: Name -> Con -> Q Match
    mkMatch yName con = do
      let arity = getArity con
      conName <- conToName con
      fieldNamesX <- case arity of
        0 -> pure []
        n -> traverse (\i -> newName $ "x" <> show i) [0, 1 .. n - 1]
      fieldNamesY <- case arity of
        0 -> pure []
        n -> traverse (\i -> newName $ "y" <> show i) [0, 1 .. n - 1]
      let xMatchPat = ConP conName [] . fmap VarP $ fieldNamesX
      let yMatchPat = ConP conName [] . fmap VarP $ fieldNamesY
      hitExp <- case zip fieldNamesX fieldNamesY of
        [] -> [e|ptrue|]
        (xField, yField) : fields -> do
          let xVar = VarE xField
          let yVar = VarE yField
          start <- [e|peq # $(pure xVar) # $(pure yVar)|]
          foldlM mkPand start fields
      missExp <- [e|pfalse|]
      let matchBody = CaseE (VarE yName) [Match yMatchPat (NormalB hitExp) [], Match WildP (NormalB missExp) []]
      pure . Match xMatchPat (NormalB matchBody) $ []
    mkPand :: Exp -> (Name, Name) -> Q Exp
    mkPand acc (xName, yName) = do
      let xVar = VarE xName
      let yVar = VarE yName
      [e|pand (peq # $(pure xVar) # $(pure yVar)) $(pure acc)|]

deriveSOPPCon :: Vector (TyVarBndr BndrVis) -> Name -> Vector Con -> Q [Dec]
deriveSOPPCon tyVars tyName constructors =
  [d|
    instance $ctx => PCon $name where
      pcon' x = $(matches 'x)
    |]
  where
    name :: Q Type
    name = pure $ foldl' (\acc -> AppT acc . VarT . bindToName) (ConT tyName) tyVars
    ctx :: Q Type
    ctx = do
      let len = Vector.length tyVars
      let varNames = fmap (AppT (ConT ''PlutarchType) . VarT . bindToName) tyVars
      pure $ foldl' AppT (TupleT len) varNames
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
      constrVec <- foldrM (\n acc -> [e|Vector.cons (toSomeTerm $(pure (VarE n))) $(pure acc)|]) (VarE 'Vector.empty) fieldNames
      matchBody <- [e|punsafeConstr $(pure constrIx) $(pure constrVec)|]
      pure . Match conMatchPat (NormalB matchBody) $ []

deriveSOPPlutarchType :: Vector (TyVarBndr BndrVis) -> Name -> Q [Dec]
deriveSOPPlutarchType tyVars tyName =
  [d|
    instance $ctx => PlutarchType $name where
      type PRepresentation $name = PSOP
    |]
  where
    name :: Q Type
    name = pure $ foldl' (\acc -> AppT acc . VarT . bindToName) (ConT tyName) tyVars
    ctx :: Q Type
    ctx = do
      let len = Vector.length tyVars
      let varNames = fmap (AppT (ConT ''PlutarchType) . VarT . bindToName) tyVars
      pure $ foldl' AppT (TupleT len) varNames

deriveSOPPMatch :: Vector (TyVarBndr BndrVis) -> Name -> Vector Con -> Con -> Q [Dec]
deriveSOPPMatch tyVars tyName cs c =
  [d|
    instance $ctx => PMatch $name where
      pmatch' x f = let handlers = $(mkHandlers 'f) in punsafeCase x handlers
    |]
  where
    name :: Q Type
    name = pure $ foldl' (\acc -> AppT acc . VarT . bindToName) (ConT tyName) tyVars
    ctx :: Q Type
    ctx = do
      let len = Vector.length tyVars
      let varNames = fmap (AppT (ConT ''PlutarchType) . VarT . bindToName) tyVars
      pure $ foldl' AppT (TupleT len) varNames
    -- We need one handler for each 'arm', of appropriate arity for the field
    -- counts.
    --
    -- We know there's at least one arm because we checked before we made it
    -- here.
    mkHandlers :: Name -> Q Exp
    mkHandlers contName = do
      let arityLast = getArity c
      nameLast <- conToName c
      let aritiesRest = fmap getArity cs
      namesRest <- traverse conToName cs
      handlerLast <- mkHandler contName (nameLast, arityLast)
      handlersRest <- traverse (mkHandler contName) (Vector.zip namesRest aritiesRest)
      start <- [e|NEVector.singleton (toSomeTerm $(pure handlerLast))|]
      foldrM (\e acc -> [e|NEVector.cons (toSomeTerm $(pure e)) $(pure acc)|]) start handlersRest
    mkHandler :: Name -> (Name, Word) -> Q Exp
    mkHandler contName (conName, arity) = do
      argNames <- case arity of
        0 -> pure []
        n -> traverse (\i -> newName $ "x" <> show i) [0, 1 .. n - 1]
      let conCallExp = AppE (VarE contName) . foldl' (\acc -> AppE acc . VarE) (ConE conName) $ argNames
      pure . foldr (\name -> AppE (VarE 'plam') . LamE [VarP name]) conCallExp $ argNames

getArity :: Con -> Word
getArity = \case
  NormalC _ fields -> fromIntegral . length $ fields
  RecC _ fields -> fromIntegral . length $ fields
  InfixC {} -> 2
  ForallC _ _ con -> getArity con
  GadtC _ fields _ -> fromIntegral . length $ fields
  RecGadtC _ fields _ -> fromIntegral . length $ fields

conToName :: Con -> Q Name
conToName = \case
  NormalC name _ -> pure name
  RecC name _ -> pure name
  InfixC _ name _ -> pure name
  ForallC _ _ con -> conToName con
  GadtC {} -> fail "SOP derivation strategy does not work on GADTs."
  RecGadtC {} -> fail "SOP derivation strategy does not work on GADTs."

checkTyName :: Name -> Q Dec
checkTyName tyName = do
  tyInfo <- reify tyName
  case tyInfo of
    TyConI tyDec -> pure tyDec
    _ -> fail $ show tyName <> " does not name a type."

checkFieldsAreTermsSOP :: Con -> Q ()
checkFieldsAreTermsSOP = \case
  NormalC name fields -> for_ fields (checkFieldIsTerm name)
  RecC name fields -> for_ fields (checkNamedFieldIsTerm name)
  InfixC lhs name rhs -> do
    checkFieldIsTerm name lhs
    checkFieldIsTerm name rhs
  ForallC {} -> fail "SOP derivation strategy does not work on nested foralls."
  GadtC {} -> fail "SOP derivation strategy does not work on GADTs."
  RecGadtC {} -> fail "SOP derivation strategy does not work on GADTs."

checkFieldIsTerm :: Name -> (Bang, Type) -> Q ()
checkFieldIsTerm conName (_, t) = digForTerm t
  where
    digForTerm :: Type -> Q ()
    digForTerm = \case
      AppT x _ -> case x of
        ConT n ->
          if n == ''Term
            then pure ()
            else
              fail $
                "Constructor "
                  <> show conName
                  <> " has a field whose type is not wrapped in 'Term'."
        _ -> digForTerm x
      _ ->
        fail $
          "Constructor "
            <> show conName
            <> " has a field whose type is not wrapped in 'Term'."

checkNamedFieldIsTerm :: Name -> (Name, Bang, Type) -> Q ()
checkNamedFieldIsTerm conName (fieldName, _, t) = digForTerm t
  where
    digForTerm :: Type -> Q ()
    digForTerm = \case
      AppT x _ -> case x of
        ConT n ->
          if n == ''Term
            then pure ()
            else
              fail $
                "Constructor "
                  <> show conName
                  <> " has a field whose type is not wrapped in 'Term', specifically "
                  <> show fieldName
        _ -> digForTerm x
      _ ->
        fail $
          "Constructor "
            <> show conName
            <> " has a field whose type is not wrapped in 'Term', specifically "
            <> show fieldName

hasNoFields :: Con -> Bool
hasNoFields = \case
  NormalC _ fields -> null fields
  RecC _ fields -> null fields
  InfixC {} -> False
  ForallC _ _ con -> hasNoFields con
  GadtC _ fields _ -> null fields
  RecGadtC _ fields _ -> null fields

bindToName :: TyVarBndr BndrVis -> Name
bindToName = \case
  PlainTV n _ -> n
  KindedTV n _ _ -> n
