{-# LANGUAGE TemplateHaskell #-}

module Diagram.Template
    ( genAbstractions
    ) where

import Control.Monad ( replicateM, forM )
import Language.Haskell.TH
    ( mkName,
      varP,
      clause,
      varE,
      listE,
      normalB,
      funD,
      Quote(newName),
      Exp(LamE, AppE, VarE),
      Q,
      Pat(ListP, VarP),
      Dec )

delist :: Int -> Q Exp
delist n = do
    xs <- replicateM n (newName "x")
    let f = mkName "f"
        args = ListP (map VarP xs)
        expr = foldl1 AppE (map VarE (f:xs))
    return $ LamE [args] expr

mkAbs:: String -> Int -> Q Dec
mkAbs basis n = do
    args  <- replicateM n (newName "n")
    let bs = mkName (basis ++ show n)
        af = mkName "alpha"
        fn = mkName "f"
        ex = [| $(varE (mkName basis)) $(varE af) $(delist n) $(listE (map varE args)) |]
        ps = varP af :  varP fn : map varP args :: [Q Pat]
    funD bs [ clause ps (normalB ex) [] ]

genAbstractions :: String -> Int -> Q [Dec]
genAbstractions basis n = forM [1..n] (mkAbs basis)
