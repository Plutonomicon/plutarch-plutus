{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Plutarch.Internal.Witness (witness) where

import Data.Proxy (Proxy)

witness :: c => Proxy c -> ()
witness _ = ()
