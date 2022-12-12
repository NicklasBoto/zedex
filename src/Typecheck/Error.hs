{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Typecheck.Error where

import Frontend.Zedex.Abs
import Frontend.Zedex.Print
import Control.Monad.Except
  ( ExceptT(ExceptT), MonadError, Except )

data TypeError
  = Urk
  | UndefinedVariable String (Int,Int)
  | TypeMismatch Type Type
  | FunctionTypeMismatch String (Int,Int) Type Type
  | NotFunctionType Expr Type
  deriving Eq

instance Show TypeError where
  show :: TypeError -> String
  show Urk = "urk"
  show (UndefinedVariable id pos) = "undefined variable " ++ printTree id ++ " at " ++ show pos
  show (TypeMismatch t1 t2) = "couldn't match expected type '" ++ printTree t1 
                           ++ "' with actual type '" ++ printTree t2 ++ "'"
  show (FunctionTypeMismatch id pos t1 t2) = "couldn't match expected type '" ++ printTree t1
                                          ++ "' with actual type '" ++ printTree t2
                                          ++ "' in function " ++ id ++ " at " ++ show pos
  show (NotFunctionType m t) = "term '" ++ printTree m ++ "' of type '" ++ printTree t ++ "' is not a function"

newtype Error a = Error { runError :: Except TypeError a }
  deriving (Functor, Applicative, Monad, MonadError TypeError)
