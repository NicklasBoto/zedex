{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE LambdaCase                 #-}

module Typecheck.Check where

import Frontend.Zedex.Abs
    ( Expr(..),
      Id(..),
      Program(..),
      Toplevel(..),
      Type(..),
      TypedId(..),
      Var(..) )
import Frontend.Zedex.Par ( pProgram, myLexer )
import Frontend.Zedex.Layout ( resolveLayout )
import Typecheck.Error
    ( TypeError(NotFunctionType, UndefinedVariable,
                FunctionTypeMismatch, TypeMismatch),
      Error(runError) )
import Control.Monad.State
    ( modify, MonadState(put, get), StateT(..) )
import Control.Monad.Except ( runExcept, MonadError(throwError) )
import Data.Functor ( (<&>) )
import Data.Bifunctor ( Bifunctor(second) )
import Data.Maybe ( mapMaybe )
import Data.List ( groupBy )
import Data.Map (Map)
import qualified Data.Map as Map

pattern (:=>), (:*) :: Type -> Type -> Type
pattern n :=> p = TTup (TDag n) [p]
pattern a :*  b = TTup a [b]

pattern (:->) :: Type -> [Type] -> Type
pattern n :-> ps = TTup (TDag n) ps

pattern TDInt :: Integer -> Type
pattern TDInt n = TDag (TInt n)

type Context = Map String Type

newtype Check a = Check { runCheck :: StateT Context Error a }
  deriving ( Functor
          , Applicative
          , Monad
          , MonadError TypeError
          , MonadState Context
          )

lookupType :: Id -> Check Type
lookupType (Id (pos, id)) = do
  ctx <- get
  case Map.lookup id ctx of
    Just ty -> return ty
    Nothing -> throwError (UndefinedVariable id pos)

extendContext :: [TypedId] -> Check ()
extendContext [] = return ()
extendContext (TI (Id (pos, id)) ty : ids) = modify (Map.insert id ty) >> extendContext ids

getVariableTypes :: [TypedId] -> [Type]
getVariableTypes = map (\(TI _ t) -> t)

extendContextWithFlags :: [Var] -> Check ()
extendContextWithFlags = modify . Map.union . Map.fromList . deflagArgs

genContext :: [Toplevel] -> Context
genContext = Map.fromList .map getTy

getTy :: Toplevel -> (String, Type)
getTy = \case
  ToplF (Id (_,id)) ty vars _ -> (id, df ty vars)
  ToplB _ (Id (_,id)) ty vars _ -> (id, df ty vars)
  where df ty = foldr ((:=>) . snd) ty . deflagArgs

deflagArgs :: [Var] -> [(String, Type)]
deflagArgs = mapMaybe deflag

deflag :: Var -> Maybe (String, Type)
deflag = \case
  VVar (TI (Id (_,id)) ty) -> Just (id, ty)
  VDag var -> deflag var <&> second TDag
  VUnd _ -> Nothing
  VBas bas (TI (Id (_,id)) ty) -> Just (id, ty)

infer :: Program -> Check [Type]
infer (Progr p) = put (genContext p) >> mapM inferToplevel p

inferToplevel :: Toplevel -> Check Type
inferToplevel = \case
  ToplF (Id (pos,id)) ret_ty vars ex -> do
    extendContextWithFlags vars
    ex_ty <- inferExpr ex
    if ex_ty === ret_ty
      then return (foldr ((:=>) . snd) ret_ty (mapMaybe deflag vars))
      else throwError (FunctionTypeMismatch id pos ret_ty ex_ty)
  ToplB _ (Id (pos,id)) ret_ty vars ex -> do
    extendContextWithFlags vars
    ex_ty <- inferExpr ex
    if ex_ty === ret_ty
      then return (foldr ((:=>) . snd) ret_ty (mapMaybe deflag vars))
      else throwError (FunctionTypeMismatch id pos ret_ty ex_ty)

inferExpr :: Expr -> Check Type
inferExpr = \case
  EVar id -> lookupType id

  EUndef -> return TBot

  ETup ex exs -> do
    ~(t:ts) <- mapM inferExpr (ex:exs)
    return (TTup t ts)

  EUnit -> return (TInt 0)

  EHad ex -> inferExpr ex <&> TDag

  EGen _ n -> return (TInt n)

  EUnf _ -> return (TInt 1 :=> TInt 1)

  EApp m n -> do
    tm <- inferExpr m
    tn <- inferExpr n
    case tm of
      n :-> (p:ps) ->
        if n === tn
          then return (TTup p ps)
          else throwError (TypeMismatch n tn)
      TDag (n :-> (p:ps)) ->
        if TDag n === tn
          then return (TDag (TTup p ps))
          else throwError (TypeMismatch (TDag n) tn)
      _ -> throwError (NotFunctionType m tm)

  EComp m n -> do
    tm <- inferExpr m
    tn <- inferExpr n
    case tm of
      mi :=> mo -> case tn of
        ni :=> no -> if no === mi
                       then return (ni :=> mo)
                       else throwError (TypeMismatch no mi)
        _ -> throwError (NotFunctionType n tn)
      _ -> throwError (NotFunctionType m tm)

  EAbs ids ex -> do
    extendContext ids
    let (ty:tys) = getVariableTypes ids
    ret <- inferExpr ex
    if null tys
      then return (ty :=> ret)
      else return (TTup ty tys :=> ret)

  EBAbs _ ids ex -> do
    extendContext ids
    let (ty:tys) = getVariableTypes ids
    ret <- inferExpr ex
    if null tys
      then return (ty :=> ret)
      else return (TTup ty tys :=> ret)

parse :: String -> Program
parse = either errorWithoutStackTrace id . pProgram . resolveLayout True . myLexer

test :: String -> String
test e = case runExcept (runError (runStateT (runCheck (infer (parse e))) Map.empty)) of
  Left e -> errorWithoutStackTrace (show e)
  Right (x,_) -> "OK!"

testFile :: FilePath -> IO ()
testFile path = readFile path >>= putStrLn . test

(===) :: Type -> Type -> Bool
TBot === t2   = True
t1   === TBot = True
t1   === t2   = reduceType t1 == reduceType t2

reduceType :: Type -> [Type]
reduceType = addTypes . types

types :: Type -> [Type]
types (TTup a as) = types a ++ concatMap types as
types (TDag (TDag t)) = types t
types (TDag t) = map TDag (types t)
types (TInt n) = [TInt n]
types TBot = [TBot]

addTypes :: [Type] -> [Type]
addTypes = map foldInt . groupBy dir
  where dir (TInt _) (TInt _) = True
        dir (TDag (TInt _)) (TDag (TInt _)) = True
        dir _ _ = False

foldInt :: [Type] -> Type
foldInt (TInt n : ts) = foldl (\(TInt x) (TInt y) -> TInt (x+y)) (TInt n) ts
foldInt ~(TDInt n : ts) = foldl (\(TDInt x) (TDInt y) -> TDInt (x+y)) (TDInt n) ts
