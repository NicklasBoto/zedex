module Frontend.IAST.Abs 
  ( Id (..)
  , Program (..)
  , Basis (..)
  , Toplevel (..)
  , Expr (..)
  ) where

import Data.Complex (Complex)

data Id = Id
    { pos :: (Int, Int)
    , name :: String 
    }
  deriving (Eq, Show, Read)
  
type Program = [Toplevel]

data Basis
    = Z
    | X
    | None
  deriving (Eq, Show, Read)
 
data Toplevel
    = ToplF Id [Id] Expr
    | Topl Basis (Complex Double) [Id] Expr
  deriving (Eq, Show, Read)
  
data Expr
    = Var Id
    | Tup [Expr]
    | Unit
    | App Expr Expr
    | Comp Expr Expr
    | Had Expr
    | Abs Basis (Complex Double) [Id] Expr
  deriving (Eq, Show, Read)
  