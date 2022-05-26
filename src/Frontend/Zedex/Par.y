-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Frontend.Zedex.Par
  ( happyError
  , myLexer
  , pProgram
  , pListToplevel
  , pToplevel
  , pExpr3
  , pExpr2
  , pExpr1
  , pExpr
  , pListExpr
  , pListId
  , pComplex2
  , pComplex1
  , pComplex
  ) where
import qualified Frontend.Zedex.Abs
import Frontend.Zedex.Lex
}

%name pProgram Program
%name pListToplevel ListToplevel
%name pToplevel Toplevel
%name pExpr3 Expr3
%name pExpr2 Expr2
%name pExpr1 Expr1
%name pExpr Expr
%name pListExpr ListExpr
%name pListId ListId
%name pComplex2 Complex2
%name pComplex1 Complex1
%name pComplex Complex
-- no lexer declaration
%monad { Either String } { (>>=) } { return }
%tokentype {Token}
%token
  '$' { PT _ (TS _ 1) }
  '(' { PT _ (TS _ 2) }
  ')' { PT _ (TS _ 3) }
  '+' { PT _ (TS _ 4) }
  ',' { PT _ (TS _ 5) }
  '-' { PT _ (TS _ 6) }
  '-i' { PT _ (TS _ 7) }
  '.' { PT _ (TS _ 8) }
  '/' { PT _ (TS _ 9) }
  ':=' { PT _ (TS _ 10) }
  ';' { PT _ (TS _ 11) }
  '@' { PT _ (TS _ 12) }
  'X' { PT _ (TS _ 13) }
  'Z' { PT _ (TS _ 14) }
  '\\' { PT _ (TS _ 15) }
  '^' { PT _ (TS _ 16) }
  'e' { PT _ (TS _ 17) }
  'i' { PT _ (TS _ 18) }
  'pi' { PT _ (TS _ 19) }
  '~' { PT _ (TS _ 20) }
  'ζ' { PT _ (TS _ 21) }
  'λ' { PT _ (TS _ 22) }
  'ξ' { PT _ (TS _ 23) }
  'π' { PT _ (TS _ 24) }
  '∘' { PT _ (TS _ 25) }
  L_Id { PT _ (T_Id _) }
  L_Scalar { PT _ (T_Scalar $$) }

%%

Id :: { Frontend.Zedex.Abs.Id}
Id  : L_Id { Frontend.Zedex.Abs.Id (mkPosToken $1) }

Scalar :: { Frontend.Zedex.Abs.Scalar}
Scalar  : L_Scalar { Frontend.Zedex.Abs.Scalar $1 }

Program :: { Frontend.Zedex.Abs.Program }
Program : ListToplevel { Frontend.Zedex.Abs.Progr $1 }

ListToplevel :: { [Frontend.Zedex.Abs.Toplevel] }
ListToplevel : {- empty -} { [] }
             | Toplevel ';' ListToplevel { (:) $1 $3 }

Toplevel :: { Frontend.Zedex.Abs.Toplevel }
Toplevel : Id ListId ':=' Expr { Frontend.Zedex.Abs.ToplF $1 $2 $4 }
         | 'ξ' '(' Complex ')' ListId ':=' Expr { Frontend.Zedex.Abs.ToplX $3 $5 $7 }
         | 'X' '(' Complex ')' ListId ':=' Expr { Frontend.Zedex.Abs.toplxs $3 $5 $7 }
         | 'ζ' '(' Complex ')' ListId ':=' Expr { Frontend.Zedex.Abs.ToplZ $3 $5 $7 }
         | 'Z' '(' Complex ')' ListId ':=' Expr { Frontend.Zedex.Abs.toplzs $3 $5 $7 }

Expr3 :: { Frontend.Zedex.Abs.Expr }
Expr3 : Id { Frontend.Zedex.Abs.EVar $1 }
      | '(' Expr ',' ListExpr ')' { Frontend.Zedex.Abs.ETup $2 $4 }
      | '(' ')' { Frontend.Zedex.Abs.EUnit }
      | '(' Expr ')' { $2 }

Expr2 :: { Frontend.Zedex.Abs.Expr }
Expr2 : Expr2 Expr3 { Frontend.Zedex.Abs.EApp $1 $2 }
      | Expr3 { $1 }

Expr1 :: { Frontend.Zedex.Abs.Expr }
Expr1 : Expr2 '$' Expr1 { Frontend.Zedex.Abs.edolr $1 $3 }
      | Expr2 '∘' Expr1 { Frontend.Zedex.Abs.EComp $1 $3 }
      | Expr2 '@' Expr1 { Frontend.Zedex.Abs.ecomps $1 $3 }
      | '~' Expr1 { Frontend.Zedex.Abs.EHad $2 }
      | 'λ' ListId '.' Expr { Frontend.Zedex.Abs.EAbs $2 $4 }
      | '\\' ListId '.' Expr { Frontend.Zedex.Abs.eabss $2 $4 }
      | 'ξ' '(' Complex ')' ListId '.' Expr { Frontend.Zedex.Abs.EXAbs $3 $5 $7 }
      | 'X' '(' Complex ')' ListId '.' Expr { Frontend.Zedex.Abs.exabss $3 $5 $7 }
      | 'ζ' '(' Complex ')' ListId '.' Expr { Frontend.Zedex.Abs.EZAbs $3 $5 $7 }
      | 'Z' '(' Complex ')' ListId '.' Expr { Frontend.Zedex.Abs.ezabss $3 $5 $7 }
      | Expr2 { $1 }

Expr :: { Frontend.Zedex.Abs.Expr }
Expr : Expr1 { $1 }

ListExpr :: { [Frontend.Zedex.Abs.Expr] }
ListExpr : Expr { (:[]) $1 } | Expr ',' ListExpr { (:) $1 $3 }

ListId :: { [Frontend.Zedex.Abs.Id] }
ListId : Id { (:[]) $1 } | Id ListId { (:) $1 $2 }

Complex2 :: { Frontend.Zedex.Abs.Complex }
Complex2 : Scalar '+' Scalar 'i' { Frontend.Zedex.Abs.CComp $1 $3 }
         | Scalar '-' Scalar 'i' { Frontend.Zedex.Abs.CComn $1 $3 }
         | Scalar { Frontend.Zedex.Abs.creal $1 }
         | '-i' { Frontend.Zedex.Abs.cnmag }
         | 'i' { Frontend.Zedex.Abs.cjmag }
         | 'π' { Frontend.Zedex.Abs.CPi }
         | 'pi' { Frontend.Zedex.Abs.cpis }
         | 'e' { Frontend.Zedex.Abs.CE }
         | '(' Complex ')' { $2 }

Complex1 :: { Frontend.Zedex.Abs.Complex }
Complex1 : Complex2 '^' Complex1 { Frontend.Zedex.Abs.CExp $1 $3 }
         | Complex2 '/' Complex1 { Frontend.Zedex.Abs.CDiv $1 $3 }
         | Complex2 Complex1 { Frontend.Zedex.Abs.CMul $1 $2 }
         | Complex2 { $1 }

Complex :: { Frontend.Zedex.Abs.Complex }
Complex : Complex1 { $1 }
{

happyError :: [Token] -> Either String a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer = tokens
}

