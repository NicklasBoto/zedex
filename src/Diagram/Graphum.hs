{-# LANGUAGE NamedFieldPuns #-}

module Diagram.Graphum where

import Control.Monad.State
    ( MonadState(put, get), State, zipWithM, when, runState )

-- | Abstraction basis datatype
data Basis
  = Z
  | X
  | H
  deriving ( Eq, Show )

-- | Graphum datatype
data Graphum
  = Graph
  { bas :: Basis              -- ^ abstraction basis
  , arg :: Double             -- ^ head node argument
  , ary :: Int                -- ^ input arity
  , out :: [ (Graphum, Int) ] -- ^ body grapha with arity numbering
  , pos :: Maybe (Int, Int)   -- ^ lattice position
  } deriving ( Eq, Show )

-- | Fix the nodes of a graphum to the points of a lattice
latticize :: Int -> Int -> Graphum -> State Int Graphum
latticize r c g@Graph{out, pos} = do
  
  -- update maximal qubit row number
  max_r <- get
  when (r > max_r) (put r)

  -- recursively number graphum body
  let f r' (g',i) = do
        x <- latticize r' (c+1) g'
        return (x, i)

  out' <- zipWithM f [c..] out

  return g{ out = out', pos = Just (r,c) }

-- | Latticize grapha without reference
numberGraphum :: Graphum
  -> ( Graphum -- ^ numbered graphum
     , Int     -- ^ max qubit row number
     )
numberGraphum = flip runState 0 . latticize 0 0
