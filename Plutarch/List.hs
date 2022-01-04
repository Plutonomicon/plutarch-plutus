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
  pnull,

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
  pfoldr,

  -- * Casting
  pforgetList,
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

instance PIsData a => PlutusType (PList a) where
  type PInner (PList a) b = PBuiltinList (PAsData a)
  pcon' :: forall s. PList a s -> forall b. Term s (PInner (PList a) b)
  pcon' (PCons x xs) = pconsBuiltin # (pdata x) # (pto xs)
  pcon' PNil = pemptyListBuiltin
  pmatch' xs f =
    pforce $
      pchooseListBuiltin
        # xs
        # pdelay (f PNil)
        # pdelay (f (PCons (pfromData $ pheadBuiltin # xs) (punsafeFrom $ ptailBuiltin # xs)))

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

-- | / O(1) /. Check if a list is empty
pnull :: PIsData a => Term s (PList a :--> PBool)
pnull = plam $ \xs -> pnullBuiltin # punsafeCoerce xs

--------------------------------------------------------------------------------

-- | Meta recursive list eliminator
precList :: PIsData a => (Term s (PList a :--> r) -> Term s a -> Term s (PList a) -> Term s r) -> (Term s (PList a :--> r) -> Term s r) -> Term s (PList a :--> r)
precList mcons mnil =
  pfix #$ plam $ \self xs ->
    pmatch xs $ \case
      PCons x xs -> mcons self x xs
      PNil -> mnil self

-- | / O(n) /. Fold on a list right-associatively
pfoldr :: PIsData a => Term s ((a :--> b :--> b) :--> b :--> PList a :--> b)
pfoldr = phoistAcyclic $
  plam $ \f z ->
    precList
      (\self x xs -> f # x # (self # xs))
      (\_self -> z)

punsafeHead :: PIsData a => Term s (PList a :--> a)
punsafeHead = plam $ \xs -> pfromData $ pheadBuiltin # punsafeCoerce xs

punsafeTail :: PIsData a => Term s (PList a :--> PList a)
punsafeTail = plam $ \xs -> punsafeCoerce $ ptailBuiltin # punsafeCoerce xs

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

pforgetList :: PIsData a => Term s (PList a) -> Term s (PList PData)
pforgetList = punsafeCoerce
