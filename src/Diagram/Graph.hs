{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}

module Diagram.Graph
    ( Graph(..)
    ) where

import GHC.TypeLits ( Nat, type (+), KnownNat )

-- | Intermediate phase translation
data Phase = None | Pi2 | Pi4 | Pi
    deriving (Eq, Show)

-- | ZX-diagram graph reprsentation
data Graph :: Nat -> Nat -> * where
    Empty   -- * the empty diagram
        :: Graph 0 0

    Cup     -- * covariant time reversal
        :: Graph 0 2

    Cap     -- * contravariant time reversal
        :: Graph 2 0

    Wire    -- * identity wire
        :: Graph 1 1

    H       -- * hadamard box
        :: Graph 1 1

    Z       -- * z spider
        :: Phase -> Graph i o

    X       -- * x spider
        :: Phase -> Graph i o

    Dag     -- * dagger
        :: Graph i o -> Graph o i

    CJI     -- * Choi-Jamiolkowski isomporphism
        :: Graph i o -> Graph 0 (i+o)

    IJC     -- * reverse CJI
        :: Graph 0 (i+o) -> Graph i o

    Par     -- * parallel composition
        :: Graph i1 o1 -> Graph i2 o2 -> Graph (i1+i2) (o1+o2)

    Ser     -- * serial composition
        :: (o1~i2) => Graph i1 o1 -> Graph i2 o2 -> Graph i1 o2
