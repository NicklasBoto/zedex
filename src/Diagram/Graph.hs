{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Diagram.Graph where

import Control.Monad.State
import Diagram.Template ( genAbstractions )

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

$(genAbstractions "zeta"   5)
$(genAbstractions "xi"     5)
$(genAbstractions "heta"   5)
$(genAbstractions "womega" 5)

wire :: Graph Node
wire = do
  t <- get
  put (t + 1)
  return $ Node Nothing Nothing 1 [] t

eigenGenerator :: Basis -> Double -> Int -> Graph Node
eigenGenerator basis alpha n = node basis alpha 0 =<< replicateM n wire

eigenZ, eigenX, eigenH, eigenW  :: Double -> Int -> Graph Node
eigenZ = eigenGenerator Z
eigenX = eigenGenerator X
eigenH = eigenGenerator H
eigenW = eigenGenerator W

new :: Int -> Graph Node
new n = eigenZ (pi * fromIntegral n) 1

splitn :: Int -> Node -> Graph Node
splitn = heta1 0 . replicate

mergen :: [Node] -> Graph Node
mergen = heta 0 \case
  []  -> []
  n:_ -> [n]

pauliX :: Node -> Graph Node
pauliX = xi1 pi pure

had :: Node -> Graph Node
had = heta1 0 pure
