{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoPartialTypeSignatures #-}

module Plutarch.TH.Helpers (
  checkTyName,
  getArity,
  conToName,
  hasNoFields,
  bindToName,
  checkFieldsAreTerms,
  fullTypeName,
  mkContextOf,
) where

import Data.Foldable (foldl', for_)
import Data.Kind qualified as GHC
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Language.Haskell.TH (
  Bang,
  BndrVis,
  Con (ForallC, GadtC, InfixC, NormalC, RecC, RecGadtC),
  Dec,
  Info (TyConI),
  Name,
  Q,
  TyVarBndr (KindedTV, PlainTV),
  Type (AppT, ConT, TupleT, VarT),
  reify,
 )
import Plutarch.Backend.Term (Term)

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
