{-# LANGUAGE TypeData #-}

module Plutarch.Backend.S (S) where

-- Note (Koz, 08/06/2026): `type data` is a particularly hideous GHCism used to
-- define (effectively) a kind without forcing us to promote a type. Whenever
-- you see `type data Foo = Bar | Baz`, you can read that as a declaration of a
-- new _kind_ `Foo`, containing the types `Bar` and `Baz`.

{- | An empty kind to ensure that the @s@ parameter never gets used for
anything.

@since wip
-}
type data S
