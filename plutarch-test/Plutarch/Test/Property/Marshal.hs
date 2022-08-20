{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Plutarch.Test.Property.Marshal (
  Marshal (marshal),
) where

import Plutarch.Lift (PLifted)
import Plutarch.Prelude

-- | Class of Haskell types that can be marshalled to a Plutarch term.
class Marshal h (p :: PType) | h -> p where
  marshal :: h -> ClosedTerm p
  default marshal :: (PLifted p ~ h, PLift p) => h -> ClosedTerm p
  marshal x = pconstant x

instance Marshal h p => Marshal [h] (PList p) where
  marshal xs = foldr (\h t -> pcons # marshal h # t) pnil xs

instance Marshal ha pa => Marshal (Maybe ha) (PMaybe pa) where
  marshal (Just x) = pcon $ PJust $ marshal x
  marshal Nothing = pcon PNothing

instance (Marshal ha pa, Marshal hb pb) => Marshal (ha, hb) (PPair pa pb) where
  marshal (a, b) = pcon $ PPair (marshal a) (marshal b)

instance Marshal Integer PInteger where
  marshal n = fromInteger n

instance Marshal Rational PRational where
  marshal r = fromRational r

instance Marshal Bool PBool where
  marshal True = pcon PTrue
  marshal False = pcon PFalse

instance Marshal () PUnit where
  marshal () = pcon PUnit
