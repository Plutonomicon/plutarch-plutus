{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}

module Plutarch.Test.Utils (
  fewerTests,
) where

import Test.Tasty.QuickCheck (QuickCheckTests)

fewerTests :: QuickCheckTests -> QuickCheckTests -> QuickCheckTests
fewerTests divisor = (`quot` divisor)
