--------------------------------------------------------------------------------

{- | Convenient lists for Plutarch
 All functions implicitly work on a PBuiltinList containing PData by using 'PList' wrapper.
 Conversions to and from PData are hidden away from interface. As a result, all functions require
 PIsData instances on the contents of the list.

 NOTE: In the future, if we have a typeclass for constants, we ought to implement an instance for
 > instance PConstant a b => PConstant [a] (PList b)
-}
module Plutarch.List (
  PList (..),

  -- * Query
  pelem,
  plength,

  -- * Construction
  pnil,
  psingleton,

  -- * Combine
  pconcat,

  -- * Traversals
  pmap,
  pfilter,

  -- * Catamorphisms
  precList,
  punsafeHead,
  punsafeTail,

  -- * Casting
  plistLiteral,
  pforgetList,
  punsafeCoerceList,
  punsafeAsIntegerList,
) where

import Plutarch
import Plutarch.Bool
import Plutarch.Builtin
import Plutarch.Integer
import Plutarch.Prelude
import qualified PlutusCore as PLC

--------------------------------------------------------------------------------

data PList (a :: k -> Type) (s :: k)
  = PCons (Term s a) (Term s (PList a))
  | PNil

-- Instances

instance PIsData a => PlutusType (PList a) where
  type PInner (PList a) b = PBuiltinList (PAsData a)
  pcon' :: forall s. PList a s -> forall b. Term s (PInner (PList a) b)
  pcon' (PCons x xs) = pconsBuiltin # (pdata x) # (pto xs)
  pcon' PNil = pemptyList
  pmatch' xs f =
    pforce $ pif (pnullBuiltin # xs) (pdelay (f PNil)) (pdelay (f (PCons (pfromData $ pheadBuiltin # xs) (punsafeFrom $ ptailBuiltin # xs))))

instance PEq a => PEq (PList a) where
  (#==) xs ys =
    punsafeBuiltin PLC.EqualsData
      # (pmkList # punsafeCoerce xs)
      # (pmkList # punsafeCoerce ys)

--------------------------------------------------------------------------------
-- Construction

-- | Create a singleton list from an element
psingleton :: PIsData a => Term s (a :--> PList a)
psingleton = plam $ \x -> pcon (PCons x (pcon PNil))

-- | The empty list
pnil :: PIsData a => Term s (PList a)
pnil = pcon PNil

--------------------------------------------------------------------------------
-- Querying

-- | / O(n) /. Check if element is in the list
pelem :: (PIsData a, PEq a) => Term s (a :--> PList a :--> PBool)
pelem =
  phoistAcyclic $
    plam $ \needle ->
      precList
        (\self x xs -> pif (x #== needle) (pcon PTrue) (self # xs))
        (\_self -> pcon PFalse)

-- | / O(n) /. Count the number of elements in the list
plength :: PIsData a => Term s (PList a :--> PInteger)
plength =
  plet
    ( pfix #$ plam $ \self ls n ->
        pmatch ls $ \case
          PCons _x xs -> (self # xs # n + 1)
          PNil -> n
    )
    $ \go -> plam $ \xs -> go # xs # 0

--------------------------------------------------------------------------------

-- | Meta recursive list eliminator
precList :: PIsData a => (Term s (PList a :--> r) -> Term s a -> Term s (PList a) -> Term s r) -> (Term s (PList a :--> r) -> Term s r) -> Term s (PList a :--> r)
precList mcons mnil =
  pfix #$ plam $ \self xs ->
    pmatch xs $ \case
      PCons x xs -> mcons self x xs
      PNil -> mnil self

punsafeHead :: PIsData a => Term s (PList a :--> a)
punsafeHead =
  phoistAcyclic $
    plam $ \xs ->
      pmatch xs $ \case
        PCons x _ -> x
        PNil -> perror

punsafeTail :: PIsData a => Term s (PList a :--> PList a)
punsafeTail =
  phoistAcyclic $
    plam $ \xs ->
      pmatch xs $ \case
        PCons _ xs -> xs
        PNil -> perror

-- | / O(n) /. Map a function over a list of elements
pmap :: (PIsData a, PIsData b) => Term s ((a :--> b) :--> PList a :--> PList b)
pmap = phoistAcyclic $ plam $ \f -> precList (\self x xs -> pcon (PCons (f # x) (self # xs))) (\_self -> pcon PNil)

-- | / O(n) /. Filter elements from a list that don't match the predicate.
pfilter :: (PIsData a) => Term s ((a :--> PBool) :--> PList a :--> PList a)
pfilter =
  phoistAcyclic $
    plam $ \predicate ->
      precList
        ( \self x xs ->
            pif
              (predicate # x)
              (pcon (PCons x (self # xs)))
              (self # xs)
        )
        (\_self -> pnil)

--------------------------------------------------------------------------------

{- | / O(n) /. Concatenate two lists

 Example:
 > pconcat # psingleton x # psingleton y == plistLiteral [x, y]

 pconcat exhibits identities with empty lists such that
 > forall x. pconcat # pnil # x == x
 > forall x. pconcat # x # pnil == x
-}
pconcat :: (PIsData a) => Term s (PList a :--> PList a :--> PList a)
pconcat =
  phoistAcyclic $
    plam $ \xs ys ->
      precList
        ( \self x xs ->
            pcon (PCons x (self # xs))
        )
        (\_self -> ys)
        # xs

--------------------------------------------------------------------------------
-- Casting

plistLiteral :: PIsData a => [Term s (PAsData a)] -> Term s (PList a)
plistLiteral [] = pnil
plistLiteral (x : xs) = pcon (PCons (pfromData x) (plistLiteral xs))

pforgetList :: PIsData a => Term s (PList a) -> Term s (PList PData)
pforgetList = punsafeCoerce

punsafeAsIntegerList :: Term s (PList PData) -> Term s (PList PInteger)
punsafeAsIntegerList = punsafeCoerce

punsafeCoerceList :: Term s (PList a) -> Term s (PList b)
punsafeCoerceList = punsafeCoerce
