{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Plutarch.Rec.TH (deriveAll, deriveScottEncoded) where

import Language.Haskell.TH (Q)
import qualified Language.Haskell.TH as TH
import Plutarch ((:-->))
import Plutarch.Rec (PRecord, ScottEncoded)
import qualified Rank2.TH

-- | Use as a TH splice for all necessary @instance@ declarations.
deriveAll :: TH.Name -> Q [TH.Dec]
deriveAll name = (<>) <$> deriveScottEncoded name <*> Rank2.TH.deriveAll name

-- | Use as a TH splice for @type instance ScottEncoded@ declarations.
deriveScottEncoded :: TH.Name -> Q [TH.Dec]
deriveScottEncoded name = do
  (con, tyVars) <- reifyConstructor name
  a <- TH.newName "a"
  let qa = pure (TH.VarT a)
      ty = foldl apply (TH.conT name) (init tyVars)
      apply t v = TH.appT t (TH.varT v)

  [d|type instance ScottEncoded $ty $qa = $(genScottEncoded con qa)|]

genScottEncoded :: TH.Con -> Q TH.Type -> Q TH.Type
genScottEncoded (TH.InfixC (_, left) _name (_, right)) result = argType left (argType right result)
genScottEncoded (TH.NormalC _name fields) result = foldr argType result (snd <$> fields)
genScottEncoded (TH.RecC _name fields) result = foldr argType result (fieldType <$> fields)
  where
    fieldType (_, _, t) = t
genScottEncoded _ _ = error "Can't encode GADTs"

argType :: TH.Type -> Q TH.Type -> Q TH.Type
argType (TH.AppT (TH.VarT _) t) result = [t|$(bare t) :--> $result|]
argType (TH.AppT t (TH.VarT _)) result = [t|PRecord $(bare t) :--> $result|]
argType _ _ = error "Expected an HKD field type of form (f FieldType)"

bare :: TH.Type -> Q TH.Type
bare (TH.SigT t _) = bare t
bare t = pure t

#if MIN_VERSION_template_haskell(2,17,0)
bindingName :: TH.TyVarBndr flag -> TH.Name
bindingName (TH.PlainTV name _) = name
bindingName (TH.KindedTV name _ _) = name
#else
bindingName :: TH.TyVarBndr -> TH.Name
bindingName (TH.PlainTV name) = name
bindingName (TH.KindedTV name _) = name
#endif

reifyConstructor :: TH.Name -> Q (TH.Con, [TH.Name])
reifyConstructor ty = do
  (TH.TyConI tyCon) <- TH.reify ty
  case tyCon of
    TH.DataD _ _nm tyVars _kind [c] _ -> return (c, bindingName <$> tyVars)
    TH.NewtypeD _ _nm tyVars _kind c _ -> return (c, bindingName <$> tyVars)
    _ -> fail "Expected a single-constructor data or newtype"
