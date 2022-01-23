{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Generic (
  MkSum (..),
  Length,
  Tail,
  TypeAt,
) where

import Data.Fin
import qualified Data.Nat as N
import GHC.TypeLits (ErrorMessage (Text), TypeError)
import Generics.SOP hiding (Proxy)

{- Infrastructure to create a single sum constructor given its type index and value.

- `mkSum @'FZ @(Code a) x` will create the first sum constructor;
- `mkSum @('FS 'FZ) () @(Code a) x` will create the second sum constructor;
- etc.

TODO: Can this be done using injections from `generics-sop`?
-}
class MkSum (idx :: Fin n) xss where
  mkSum :: NP I (TypeAt idx xss) -> NS (NP I) xss

instance MkSum 'FZ (xs ': xss) where
  mkSum = Z
instance MkSum idx xss => MkSum ( 'FS idx) (xs ': xss) where
  mkSum = S . mkSum @_ @idx @xss

type family Length xs :: N.Nat where
  Length '[] = 'N.Z
  Length (x ': xs) = 'N.S (Length xs)

class Tail' (idx :: Fin n) (xss :: [[k]]) where
  type Tail idx xss :: [[k]]

instance Tail' 'FZ xss where
  type Tail 'FZ xss = xss

instance Tail' idx xs => Tail' ( 'FS idx) (x ': xs) where
  type Tail ( 'FS idx) (x ': xs) = Tail idx xs
instance Tail' idx xs => Tail' ( 'FS idx) '[] where
  type Tail ( 'FS idx) '[] = TypeError ( 'Text "Tail: index out of bounds")

class TypeAt' (idx :: Fin n) (xs :: [[k]]) where
  type TypeAt idx xs :: [k]

instance TypeAt' 'FZ (x ': xs) where
  type TypeAt 'FZ (x ': xs) = x

instance TypeAt' ( 'FS idx) (x ': xs) where
  type TypeAt ( 'FS idx) (x ': xs) = TypeAt idx xs
