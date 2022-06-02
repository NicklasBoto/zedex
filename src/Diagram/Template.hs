{-# LANGUAGE TemplateHaskell #-}

module Diagram.Template
    ( -- genAbstractions
    ) where

import Control.Monad
import Language.Haskell.TH

delist :: Int -> Q Exp
delist n = do
    xs <- replicateM n (newName "x")
    let f = mkName "f"
        args = ListP (map VarP xs)
        expr = foldl1 AppE (map VarE (f:xs))
    return $ LamE [args] expr

-- xii3 alpha f n1 n2 n3 = xi alpha (\[x, y, z] -> f x y z) [n1, n2, n3]

mkAbs:: String -> Int -> Q Dec
mkAbs basis n = do
    args  <- replicateM n (newName "n")
    let bs = mkName basis
        af = mkName "alpha"
        fn = mkName "f"
        ex = [| $(varE bs) $(varE af) $(delist n) |]
        ps = [p| $(varP af) $(varP af) |]
    funD (mkName basis) [clause (varP (mkName "alpha") ) (normalB e) []]
    undefined
        


-- gen :: String -> Int -> Q [Dec]
-- gen basis n = [d| $(funD (mkName name)) alpha f = $(varE (mkName basis)) alpha $(delist n) |]

-- genAbstractions :: String -> Int -> Q [Dec]
-- genAbstractions basis n = forM [1..n] mkDelist
--   where mkDelist ith = do
--           dl <- delist ith
--           let name = mkName $ basis ++ show ith
--           -- return $ FunD name [Clause [] (NormalB dl) []]
--           [d| name alpha f = xi dl |]
