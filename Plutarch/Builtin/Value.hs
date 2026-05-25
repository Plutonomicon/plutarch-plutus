-- | @since 1.14.0
module Plutarch.Builtin.Value (
  -- * Type
  PBuiltinValue (..),

  -- * Functions
  pinsertCoin,
  plookupCoin,
  punionValue,
  pvalueContains,
  pvalueData,
  punValueData,
  pscaleValue,
) where

import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Bool (PBool)
import Plutarch.Builtin.ByteString (PByteString)
import Plutarch.Builtin.Data (PData)
import Plutarch.Builtin.Integer (PInteger)
import Plutarch.Builtin.Opaque (POpaque)
import Plutarch.Internal.Term (S, Term, punsafeBuiltin, (:-->))
import PlutusCore qualified as PLC

{- | A builtin Plutus @Value@.

@since 1.14.0
-}
newtype PBuiltinValue (s :: S) = PBuiltinValue (Term s POpaque)
  deriving stock
    ( -- | @since 1.14.0
      Generic
    )
  deriving anyclass
    ( -- | @since 1.14.0
      SOP.Generic
    )

{- | Given a currency, a token name, an amount, an an existing 'PBuiltinValue',
produce a new 'PBuiltinValue', identical to the argument 'PBuiltinValue',
except as follows:

* If the amount is @0@, the new 'PBuiltinValue' will have the given
  currency-token name entry deleted;
* If the argument 'PBuiltinValue' is empty, the new 'PBuiltinValue' will be a
  singleton containing the given currency-token name entry with the given
  (nonzero) amount.
* If the argument 'PBuiltinValue' already contains the given currency-token
  name entry, the new 'PBuiltinValue' will have that currency-token name
  entry replaced with the given (nonzero) amount.
* Otherwise, the new 'PBuiltinValue' will contain a new currency-token name
  entry, with the given (nonzero) amount.

= Important note

If given a currency name, or a token name, that isn't valid, this will error.

@since 1.14.0
-}
pinsertCoin ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PByteString
        :--> PInteger
        :--> PBuiltinValue
        :--> PBuiltinValue
    )
pinsertCoin = punsafeBuiltin PLC.InsertCoin

{- | Given a currency, a token name, and a 'PBuiltinValue', return the amount
associated with the given currency-token name entry. If there is no such
entry, the result is @0@.

= Important note

If given a currency name, or token name, that isn't valid, this will produce
@0@.

@since 1.14.0
-}
plookupCoin ::
  forall (s :: S).
  Term
    s
    ( PByteString
        :--> PByteString
        :--> PBuiltinValue
        :--> PInteger
    )
plookupCoin = punsafeBuiltin PLC.LookupCoin

{- | Given two 'PBuiltinValue's, constructs their union. More specifically, for
every currency-token name entry in either argument, the result will contain a
currency-token name entry as well. If the entry is in one argument, it will
be copied as-is; otherwise, the amounts will be combined from both maps using
addition.

= Important note

'PBuiltinValue's cannot store amounts outside of the range of a 128-bit
signed integer. If any amount produced by this operation would fall outside
of this range, this function will error.

@since 1.14.0
-}
punionValue ::
  forall (s :: S).
  Term
    s
    ( PBuiltinValue
        :--> PBuiltinValue
        :--> PBuiltinValue
    )
punionValue = punsafeBuiltin PLC.UnionValue

{- | @'pvalueContains' x y@ is true when, for any currency-token name entry in
@y@, the amount associated with that entry in @x@ is not smaller.

= Important note

Neither argument can contain any negative entries. If either argument has a
negative entry, this will error.

@since 1.14.0
-}
pvalueContains ::
  forall (s :: S).
  Term
    s
    ( PBuiltinValue
        :--> PBuiltinValue
        :--> PBool
    )
pvalueContains = punsafeBuiltin PLC.ValueContains

{- | Converts a 'PBuiltinValue' into its @Data@ encoding.

@since 1.14.0
-}
pvalueData ::
  forall (s :: S).
  Term s (PBuiltinValue :--> PData)
pvalueData = punsafeBuiltin PLC.ValueData

{- | Converts a valid @Data@ encoding of a 'PBuiltinValue' into the
'PBuiltinValue' it represents, and errors otherwise.

@since 1.14.0
-}
punValueData ::
  forall (s :: S).
  Term s (PData :--> PBuiltinValue)
punValueData = punsafeBuiltin PLC.UnValueData

{- | Given a 'PBuiltinValue' and a scalar, produce a new 'PBuiltinValue' with
all amounts multiplied by that scalar. Note that if the scalar is @0@, then
the result will be an empty 'PBuiltinValue'.

@since 1.14.0
-}
pscaleValue ::
  forall (s :: S).
  Term s (PInteger :--> PBuiltinValue :--> PBuiltinValue)
pscaleValue = punsafeBuiltin PLC.ScaleValue
