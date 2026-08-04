{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoPartialTypeSignatures #-}

module Plutarch.Helpers.TH (
  checkTyName,
  getArity,
  conToName,
  hasNoFields,
  bindToName,
  checkFieldsAreTerms,
  checkFieldIsWrapped,
  fullTypeName,
  mkContextOf,
  mkUncons,
  toSomeTermE,
  punsafeCaseE,
  punsafeConstrE,
  punsafeConstantE,
  pequalsIntegerE,
  plam'E,
  punsafeCoerceE,
  punConstrDataE,
  pnilDataE,
  pmkConsE,
  pconstrDataE,
  pequalsDataE,
  conToFieldTypes,
  mkPLam,
  mkAnonPLam,
) where

import Data.Foldable (foldl', for_)
import Data.Kind qualified as GHC
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Data.Vector.NonEmpty qualified as NEVector
import Language.Haskell.TH (
  Bang,
  BndrVis,
  Con (ForallC, GadtC, InfixC, NormalC, RecC, RecGadtC),
  Dec,
  Exp (AppE, LamE, VarE),
  Info (TyConI),
  Name,
  Pat (VarP, WildP),
  Q,
  TyVarBndr (KindedTV, PlainTV),
  Type (AppT, ConT, TupleT, VarT),
  newName,
  reify,
 )
import Plutarch.Backend.Term (
  Term,
  plam',
  punsafeCase,
  punsafeCoerce,
  punsafeConstant,
  punsafeConstr,
  toSomeTerm,
 )
import Plutarch.Primitive.BuiltinFun (
  pconstrData,
  pequalsData,
  pequalsInteger,
  pmkCons,
  pnilData,
  punConstrData,
 )
import Plutarch.Primitive.Data (PAsData)

{- | Return the declaration of a type given its name, or error out if the name
does not name a type.

@since wip
-}
checkTyName :: Name -> Q Dec
checkTyName tyName = do
  tyInfo <- reify tyName
  case tyInfo of
    TyConI tyDec -> pure tyDec
    _ -> fail $ show tyName <> " does not name a type."

{- | Given a data constructor, state how many fields it has.

@since wip
-}
getArity :: Con -> Word
getArity = \case
  NormalC _ fields -> fromIntegral . length $ fields
  RecC _ fields -> fromIntegral . length $ fields
  InfixC {} -> 2
  ForallC _ _ con -> getArity con
  GadtC _ fields _ -> fromIntegral . length $ fields
  RecGadtC _ fields _ -> fromIntegral . length $ fields

{- | Get the name of a non-GADT constructor. Error out if given a GADT
constructor.

@since wip
-}
conToName :: Con -> Q Name
conToName = \case
  NormalC name _ -> pure name
  RecC name _ -> pure name
  InfixC _ name _ -> pure name
  ForallC _ _ con -> conToName con
  GadtC {} -> fail "Derivation does not work on GADTs."
  RecGadtC {} -> fail "Derivation does not work on GADTs."

{- | Check that a data constructor has no fields.

@since wip
-}
hasNoFields :: Con -> Bool
hasNoFields = \case
  NormalC _ fields -> null fields
  RecC _ fields -> null fields
  InfixC {} -> False
  ForallC _ _ con -> hasNoFields con
  GadtC _ fields _ -> null fields
  RecGadtC _ fields _ -> null fields

{- | Given a binder, yield the name it binds.

@since wip
-}
bindToName :: TyVarBndr BndrVis -> Name
bindToName = \case
  PlainTV n _ -> n
  KindedTV n _ _ -> n

{- | Check that every field of the given data constructor is @'Term'@-wrapped.
Error if not, or if given a nested @forall@ or GADT.

@since wip
-}
checkFieldsAreTerms :: Con -> Q ()
checkFieldsAreTerms = \case
  NormalC name fields -> for_ fields (go name)
  RecC name fields -> for_ fields (goNamed name)
  InfixC lhs name rhs -> do
    go name lhs
    go name rhs
  ForallC {} -> fail "Derivation does not work on nested foralls."
  GadtC {} -> fail "Derivation does not work on GADTs."
  RecGadtC {} -> fail "Derivation does not work on GADTs."
  where
    go :: Name -> (Bang, Type) -> Q ()
    go conName (_, t) =
      let errMsg =
            "Constructor "
              <> show conName
              <> " has a field whose type is not wrapped in 'Term'."
       in dig errMsg t
    goNamed :: Name -> (Name, Bang, Type) -> Q ()
    goNamed conName (fieldName, _, t) =
      let errMsg =
            "Constructor "
              <> show conName
              <> " has a field whose type is not wrapped in 'Term', specifically "
              <> show fieldName
              <> "."
       in dig errMsg t
    dig :: String -> Type -> Q ()
    dig errMsg = \case
      AppT x _ -> case x of
        ConT n ->
          if n == ''Term
            then pure ()
            else fail errMsg
        _ -> dig errMsg x
      _ -> fail errMsg

{- | Given a \'base\' type name, and a list of type variable binds, construct
the type name with all those binds applied.

@since wip
-}
fullTypeName ::
  forall (f :: GHC.Type -> GHC.Type).
  Foldable f => Name -> f (TyVarBndr BndrVis) -> Type
fullTypeName tyName = foldl' (\acc -> AppT acc . VarT . bindToName) (ConT tyName)

{- | Given a 'Vector' of type variables, and the name of a single-parameter type
class, construct a context asserting all of these type variables are
instances of that type class.

@since wip
-}
mkContextOf :: Name -> Vector (TyVarBndr BndrVis) -> Type
mkContextOf tyClassName tyVars =
  let len = Vector.length tyVars
      varNames = fmap (AppT (ConT tyClassName) . VarT . bindToName) tyVars
   in foldl' AppT (TupleT len) varNames

{- | Verifies that all fields of the given constructor are \'wrapped\' in
@PAsData@.

@since wip
-}
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

{- | Given a name @ell@ corresponding to a @PDList@ variable, constructs the
equivalent of:

@
punsafeCase ell . NEVector.singleton . toSomeTerm $ f headName tailName
@

@headName@ and @tailName@ are passed to the continuation (being generated
locally).

@since wip
-}
mkUncons :: Name -> (Name -> Name -> Q Exp) -> Q Exp
mkUncons listName f = do
  hName <- newName "h"
  tName <- newName "t"
  body <- f hName tName
  let innerLam = AppE (VarE 'plam') . LamE [VarP tName] $ body
  let outerLam = AppE (VarE 'plam') . LamE [VarP hName] $ innerLam
  [e|punsafeCase $(pure (VarE listName)) (NEVector.singleton (toSomeTerm $(pure outerLam)))|]

-- | @since wip
toSomeTermE :: Q Exp
toSomeTermE = pure $ VarE 'toSomeTerm

-- | @since wip
punsafeCaseE :: Q Exp
punsafeCaseE = pure $ VarE 'punsafeCase

-- | @since wip
punsafeConstrE :: Q Exp
punsafeConstrE = pure $ VarE 'punsafeConstr

-- | @since wip
punsafeConstantE :: Q Exp
punsafeConstantE = pure $ VarE 'punsafeConstant

-- | @since wip
pequalsIntegerE :: Q Exp
pequalsIntegerE = pure $ VarE 'pequalsInteger

-- | @since wip
plam'E :: Q Exp
plam'E = pure $ VarE 'plam'

-- | @since wip
punsafeCoerceE :: Q Exp
punsafeCoerceE = pure $ VarE 'punsafeCoerce

-- | @since wip
punConstrDataE :: Q Exp
punConstrDataE = pure $ VarE 'punConstrData

-- | @since wip
pnilDataE :: Q Exp
pnilDataE = pure $ VarE 'pnilData

-- | @since wip
pmkConsE :: Q Exp
pmkConsE = pure $ VarE 'pmkCons

-- | @since wip
pconstrDataE :: Q Exp
pconstrDataE = pure $ VarE 'pconstrData

-- | @since wip
pequalsDataE :: Q Exp
pequalsDataE = pure $ VarE 'pequalsData

{- | Retrieve the types of all fields in a data constructor, in order. Fail if
given something unsupported. Will do 'Term' unwrapping as well.

@since wip
-}
conToFieldTypes :: Con -> Q [Type]
conToFieldTypes = \case
  NormalC _ fields -> traverse (termUnwrap . snd) fields
  RecC _ fields -> traverse (\(_, _, t) -> termUnwrap t) fields
  InfixC (_, t1) _ (_, t2) -> traverse termUnwrap [t1, t2]
  ForallC {} -> fail "Derivation does not work on nested foralls."
  GadtC {} -> fail "Derivation does not work on GADTs."
  RecGadtC {} -> fail "Derivation does not work on GADTs."
  where
    termUnwrap :: Type -> Q Type
    termUnwrap = \case
      AppT _ t -> pure t
      _ -> fail "Unexpected non-Term-wrapped type. If you see this, report a bug."

-- | @since wip
mkPLam :: (Name -> Q Exp) -> Q Exp
mkPLam f = do
  argName <- newName "x"
  body <- f argName
  pure . AppE (VarE 'plam') . LamE [VarP argName] $ body

-- | @since wip
mkAnonPLam :: Q Exp -> Q Exp
mkAnonPLam f = AppE (VarE 'plam') . LamE [WildP] <$> f
