{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Diagram.Graph where

import Control.Monad.State
import Control.Arrow
import Diagram.Template

-- | 
data NodeType
    = Red   Double
    | Green Double 
    | Box
    | Blank
    deriving (Eq, Show)

data Basis
    = Z
    | X
    | H
    | W
    deriving (Eq, Show)

data Node = Node
    { basis :: Basis
    , arg   :: Double
    , arity :: Int
    , out   :: [Node]
    , id    :: Int
    } deriving (Eq, Show) 

newtype Graph a = Gr { unGr :: State Int a }
    deriving (Functor, Applicative, Monad, MonadState Int)

node :: Basis -> Double -> Int -> [Node] -> Graph Node
node b a n o = do
    i <- get
    put (i + 1)
    return $ Node b a n o i

abstraction :: Basis -> Double -> ([Node] -> [Node]) -> [Node] -> Graph Node
abstraction basis alpha = liftM2 (node basis alpha) length

zeta :: Double -> ([Node] -> [Node]) -> [Node] -> Graph Node
zeta = abstraction Z

xi :: Double -> ([Node] -> [Node]) -> [Node] -> Graph Node
xi = abstraction X

-- xi1 :: Double -> (Node -> [Node]) -> Node -> Graph Node
-- xi1 alpha f = xi alpha (f =<<) . pure

xii3 alpha f n1 n2 n3 = xi alpha (\[x, y, z] -> f x y z) [n1, n2, n3]

$(genAbstractions 10)

-- foo :: Node -> Graph Node
-- foo = xi1 pi (\x -> [x,x])

had :: Node -> Graph Node
had n = node H 0 1 [n]

splits :: Node -> Graph Node
splits n = node H 0 1 [n,n]

pauliX :: Node -> Graph Node
pauliX n = node X pi 1 [n]

isometry :: Double -> Node -> Graph Node
isometry phase x = node X phase 1 [x,x] 

share :: Node -> Graph Node
share = isometry 0 

partIsometry :: Double -> Node -> Graph Node
partIsometry phase x = node X phase 2 [x]

fuse :: Node -> Graph Node
fuse = partIsometry 0

-- ghz :: Graph Node
-- ghz = do
--     a <- xi 0 
--     b <- xi 0 (\x -> x) []
