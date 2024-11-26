module Plutarch.Enum (
  -- * Type classes
  PCountable (..),
  PEnumerable (..),
) where

import Data.Kind (Type)
import Plutarch.Builtin.Bool (pif)
import Plutarch.Builtin.Integer (PInteger)
import Plutarch.Internal.Eq ((#==))
import Plutarch.Internal.Ord (POrd)
import Plutarch.Internal.Other (pfix, pto)
import Plutarch.Internal.PLam (plam)
import Plutarch.Internal.Term (
  S,
  Term,
  phoistAcyclic,
  (#),
  (#$),
  (:-->),
 )
import Plutarch.Positive (PPositive)

{- | A notion of \'next\' value. More formally, instances of this type class are
discrete linear orders with no maximal element.

= Laws

1. @x /= psuccessor x@
2. @y < x@ @=@ @psuccessor y <= x@
3. @x < psuccessor y@ @=@ @x <= y@

If you define 'psuccessorN', you must also ensure the following hold; the
default implementation ensures this.

4. @psuccessorN 1@ @=@ @psuccessor@
5. @psuccessorN n . psuccessorN m@ @=@ @psuccessorN (n + m)@

Law 1 ensures no value is its own successor. Laws 2 and 3 ensure that there
are no \'gaps\': every value is \'reachable\' from any lower value by a
finite number of applications of 'successor'.

@since WIP
-}
class POrd a => PCountable (a :: S -> Type) where
  -- | @since WIP
  psuccessor :: forall (s :: S). Term s (a :--> a)

  -- | The default implementation of this function is inefficient: if at all
  -- possible, give instances an optimized version that doesn't require
  -- recursion.
  --
  -- @since WIP
  {-# INLINEABLE psuccessorN #-}
  psuccessorN :: forall (s :: S). Term s (PPositive :--> a :--> a)
  psuccessorN = phoistAcyclic $ plam $ \n x -> go n # (psuccessor # x) # 1
    where
      go ::
        forall (s' :: S).
        Term s' PPositive ->
        Term s' (a :--> PPositive :--> a)
      go limit = pfix #$ plam $ \self acc count ->
        pif
          (count #== limit)
          acc
          (self # (psuccessor # acc) # (count + 1))

-- | @since WIP
instance PCountable PInteger where
  {-# INLINEABLE psuccessor #-}
  psuccessor = phoistAcyclic $ plam (+ 1)
  {-# INLINEABLE psuccessorN #-}
  psuccessorN = phoistAcyclic $ plam $ \p i -> pto p + i

-- | @since WIP
instance PCountable PPositive where
  {-# INLINEABLE psuccessor #-}
  psuccessor = phoistAcyclic $ plam (+ 1)
  {-# INLINEABLE psuccessorN #-}
  psuccessorN = phoistAcyclic $ plam (+)

{- | Similar to 'PCountable', but has the ability to get a \'previous\' value as
well. More formally, instances of this type class are discrete linear orders
with no maximal or minimal element.

= Laws

1. @ppredecessor . psuccessor@ @=@ @psuccessor . ppredecessor@ @=@ @id@

If you define 'ppredecessorN', you must also ensure the following hold; the
default implementation ensures this.

2. @ppredecessorN 1@ @=@ @ppredecessor@
3. @ppredecessorN n . ppredecessorN m@ @=@ @ppredecessorN (n + m)@

From Law 1, we obtain the following theorem:

* @x /= predecessor x@

@since WIP
-}
class PCountable a => PEnumerable (a :: S -> Type) where
  -- | @since WIP
  ppredecessor :: forall (s :: S). Term s (a :--> a)

  -- | The default implementation of this function is inefficient: if at all
  -- possible, give instances an optimized version that doesn't require
  -- recursion.
  --
  -- @since WIP
  {-# INLINEABLE ppredecessorN #-}
  ppredecessorN :: forall (s :: S). Term s (PPositive :--> a :--> a)
  ppredecessorN = phoistAcyclic $ plam $ \n x -> go n # (ppredecessor # x) # 1
    where
      go ::
        forall (s' :: S).
        Term s' PPositive ->
        Term s' (a :--> PPositive :--> a)
      go limit = pfix #$ plam $ \self acc count ->
        pif
          (count #== limit)
          acc
          (self # (ppredecessor # acc) # (count + 1))

-- | @since WIP
instance PEnumerable PInteger where
  {-# INLINEABLE ppredecessor #-}
  ppredecessor = phoistAcyclic $ plam (- 1)
  {-# INLINEABLE ppredecessorN #-}
  ppredecessorN = phoistAcyclic $ plam $ \p i -> i - pto p
