module Plutarch.TH.Strategy (
  Strategy (..),
  deriveFor,
) where

import Data.Foldable (traverse_)
import Data.Vector qualified as Vector
import Language.Haskell.TH (
  Dec (DataD, NewtypeD, TySynD),
  Name,
  Q,
 )
import Plutarch.Helpers.TH (checkFieldsAreTerms, checkTyName)
import Plutarch.TH.DataList (deriveDataList)
import Plutarch.TH.DataPlutus (deriveDataPlutus)
import Plutarch.TH.Enum (deriveEnum)
import Plutarch.TH.MS (deriveMS)
import Plutarch.TH.SOP (deriveSOP)

{- | Specifies a representation choice for the data type you want derivations
for. This will determine both what instances are generated, and also what the
generated instances will look like.

@since wip
-}
data Strategy
  = {- | Use UPLC's built-in @SOP@ as the representation. This strategy derives
    'PlutarchType', 'PMatch', 'PCon' and 'PEq' instances.

    @since wip
    -}
    SOP
  | {- | Use @PData@ as the representation. This strategy derives
    'PlutarchType', 'PMatch', 'PCon' and 'PEq' instances.

    This will use a PlutusTx-style sum-of-products encoding with @Constr@,
    even for types with only one \'arm\'.

    @since wip
    -}
    DataPlutus
  | {- | Use an onchain @Integer@ as the representation. This strategy derives
    'PlutarchType', 'PMatch', 'PCon' and 'PEq' instances.

    @since wip
    -}
    Enum
  | {- | Use a @'PBList'@ as the representation. This strategy derives
    'PlutarchType', 'PMatch', and 'PCon' instances.

    @since wip
    -}
    DataList
  | {- | Use a final-encoding lambda as the representation. This strategy
    derives 'PlutarchType', 'PMatch', 'PCon' and 'PEq' instances.

    @since wip
    -}
    MogensenScott

{- | Given a type name, and a 'Strategy', derive all possible instances for that
type as allowed by that 'Strategy'.

= Important note

All 'Strategy' choices assume that the type is formed correctly to be a
Plutarch type. This means the following in practice:

* The type's /last/ type parameter must be of kind 'S'.
* Every field of every \'arm\' must be wrapped in @'Term' s@, where @s@ is
  the type parameter of kind 'S'.

While 'deriveFor' will check for all of these, it cannot \'look through\' type
synonyms. Please make sure that you do not use a type synonym as a field of any data
type that you wish to use 'deriveFor' with.

@since wip
-}
deriveFor :: Name -> Strategy -> Q [Dec]
deriveFor tyName strat = do
  tyDec <- checkTyName tyName
  case tyDec of
    DataD _ name tyVarBinds _ constructors _ -> do
      let tvbAsVec = Vector.fromList tyVarBinds
      let consAsVec = Vector.fromList constructors
      traverse_ checkFieldsAreTerms consAsVec
      case Vector.unsnoc tvbAsVec of
        Nothing -> fail "Types must have an s :: S type parameter in last position."
        Just (tvbs, _) -> case strat of
          SOP -> deriveSOP tvbs name consAsVec
          DataPlutus -> deriveDataPlutus tvbs name consAsVec
          Enum -> deriveEnum tvbs name consAsVec
          DataList -> deriveDataList tvbs name consAsVec
          MogensenScott -> deriveMS tvbs name consAsVec
    NewtypeD {} -> fail "Newtype derivations not supported at present."
    TySynD {} -> fail "Type synonym derivations are not supported. Define using the underlying type."
    _ -> fail $ "Not a valid type name: " <> show tyName
