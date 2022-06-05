{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}

module Diagram.Graph where

import Control.Monad.State
import Diagram.Template ( genAbstractions )
import qualified Data.Sized as S

data Basis
    = Z
    | X
    | H
    | W
    deriving (Eq, Show)

data Node = Node
    { basis :: Maybe Basis
    , arg   :: Maybe Double
    , arity :: Int
    , out   :: [] Node
    , tag   :: Int
    } deriving (Eq, Show)

newtype Graph a = Gr { unGr :: State Int a }
    deriving (Functor, Applicative, Monad, MonadState Int)

runGraph :: Graph c -> c
runGraph = flip evalState 0 . unGr

node :: Basis -> Double -> Int -> [Node] -> Graph Node
node b a n o = do
    t <- get
    put (t + 1)
    return $ Node (Just b) (Just a) n o t
{-# INLINE node #-}

abstraction :: Basis -> Double -> ([Node] -> [Node]) -> [Node] -> Graph Node
abstraction basis alpha = liftM2 (node basis alpha) length

zeta, xi, heta, womega :: Double -> ([Node] -> [Node]) -> [Node] -> Graph Node
zeta   = abstraction Z
xi     = abstraction X
heta   = abstraction H
womega = abstraction W

$(genAbstractions "xi" 5)
$(genAbstractions "zeta" 5)
$(genAbstractions "heta" 5)
$(genAbstractions "womega" 5)

eigenX :: Double -> Int -> Graph Node
eigenX alpha n = xi1 alpha (replicate n)
             =<< node X 0 1 [] 

eigenZ :: Double -> Int -> Graph Node
eigenZ alpha n = zeta1 alpha (replicate n)
             =<< node Z 0 1 [] 

had :: Node -> Graph Node
had = heta1 0 pure

splits :: Node -> Graph Node
splits = heta1 0 (\x -> [x,x])

splitn :: Int -> Node -> Graph Node
splitn = heta1 0 . replicate

mergen :: [Node] -> Graph Node
mergen = heta 0 (pure . head)

pauliX :: Node -> Graph Node
pauliX = xi1 pi pure

xNode :: Double -> [Node] -> Graph Node
xNode = flip xi id

isometry :: Double -> Node -> Graph Node
isometry = flip xi1 (\x -> [x,x]) 

share :: Node -> Graph Node
share = isometry 0

partIsometry :: Double -> Node -> Node -> Graph Node
partIsometry = flip xi2 (\x _ -> [x])

fuse :: Node -> Node -> Graph Node
fuse = partIsometry 0

-- ghz :: Graph Node
-- ghz = do
--     a <- xi 0 
--     b <- xi 0 (\x -> x) []
