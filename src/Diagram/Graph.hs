{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | 
data NodeType
    = Red   Double
    | Green Double 
    | Box
    | Blank
    deriving (Eq, Show)

data Node = Node 
    { nodetype :: NodeType
    , adjacent :: [Node]
    }

type Graph = [Node]

translate :: Graph -> String
translate = concatMap (uncurry $ go 0) . zip [0..]
    where go rnum qnum Node{..} = case nodetype of
            Red alpha -> ""
            Green alpha -> ""
            Box -> ""
            Blank -> ""


