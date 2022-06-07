module Frontend.IAST.Abs where

import Data.Complex (Complex)

data Id = Id
    { pos :: (Int, Int)
    , name :: String 
    }
  deriving (Eq, Show, Read)
  
newtype Program = Progr [Toplevel]
  deriving (Eq, Show, Read) 

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
    | Tup Expr [Expr]
    | Unit
    | App Expr Expr
    | Comp Expr Expr
    | Had Expr
    | Abs Basis (Complex Double) [Id] Expr
  deriving (Eq, Show, Read)
  