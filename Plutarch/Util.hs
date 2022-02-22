module Plutarch.Util ( type (:$) ) where 

infixr 0 :$

{- | 
    An typeoperator that works similar to the equivalent '$' on term level 
    use it for declarations like: 
    'Term _ :$ PAsData PSomething' to avoid using brackets
-}
type a :$ b = a b 
