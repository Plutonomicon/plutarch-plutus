{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Tuple (PTuple (..)) where

--------------------------------------------------------------------------------

import Plutarch (PMatch)
import Plutarch.Builtin (PIsData)
import Plutarch.DataRepr (
  DataReprHandlers (..),
  PDataList,
  PIsDataRepr (..),
  PIsDataReprInstances (..),
  pmatchDataRepr,
 )
import Plutarch.Lift (AsDefaultUni, PLift (..))
import Plutarch.Prelude
import qualified PlutusTx

--------------------------------------------------------------------------------

{- | n-ary tuples, encoded as Plutus Constr.

  Most of the time, 2-tuples in the Plutus API will be encoded
  as this type, rather than BuiltinPair, as it lacks a corresponding
  constructor in `Data`.
-}
newtype PTuple (as :: [k -> Type]) (s :: k)
  = PTuple (Term s (PDataList as))
  deriving
    (PMatch, PIsData, AsDefaultUni)
    via PIsDataReprInstances (PTuple as) [PlutusTx.Data] -- second param is unused

instance PIsDataRepr (PTuple as) where
  type
    PIsDataReprRepr (PTuple as) =
      '[as]

  pmatchRepr dat f =
    pmatchDataRepr dat $
      DRHCons (f . PTuple) DRHNil

deriving via
  PIsDataReprInstances
    (PTuple '[a, b])
    (PHaskellType a, PHaskellType b)
  instance
    ( PlutusTx.FromData (PHaskellType a)
    , PlutusTx.ToData (PHaskellType a)
    , PlutusTx.FromData (PHaskellType b)
    , PlutusTx.ToData (PHaskellType b)
    ) =>
    PLift (PTuple '[a, b])

deriving via
  PIsDataReprInstances
    (PTuple '[a, b, c])
    (PHaskellType a, PHaskellType b, PHaskellType c)
  instance
    ( PlutusTx.FromData (PHaskellType a)
    , PlutusTx.ToData (PHaskellType a)
    , PlutusTx.FromData (PHaskellType b)
    , PlutusTx.ToData (PHaskellType b)
    , PlutusTx.FromData (PHaskellType c)
    , PlutusTx.ToData (PHaskellType c)
    ) =>
    PLift (PTuple '[a, b, c])

deriving via
  PIsDataReprInstances
    (PTuple '[a, b, c, d])
    (PHaskellType a, PHaskellType b, PHaskellType c, PHaskellType d)
  instance
    ( PlutusTx.FromData (PHaskellType a)
    , PlutusTx.ToData (PHaskellType a)
    , PlutusTx.FromData (PHaskellType b)
    , PlutusTx.ToData (PHaskellType b)
    , PlutusTx.FromData (PHaskellType c)
    , PlutusTx.ToData (PHaskellType c)
    , PlutusTx.FromData (PHaskellType d)
    , PlutusTx.ToData (PHaskellType d)
    ) =>
    PLift (PTuple '[a, b, c, d])
