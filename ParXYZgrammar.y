-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParXYZgrammar where
import AbsXYZgrammar
import LexXYZgrammar
import ErrM

}

%name pProgram Program
-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype {Token}
%token
  '!' { PT _ (TS _ 1) }
  '!=' { PT _ (TS _ 2) }
  '%' { PT _ (TS _ 3) }
  '&' { PT _ (TS _ 4) }
  '&&' { PT _ (TS _ 5) }
  '(' { PT _ (TS _ 6) }
  ')' { PT _ (TS _ 7) }
  '*' { PT _ (TS _ 8) }
  '+' { PT _ (TS _ 9) }
  ',' { PT _ (TS _ 10) }
  '-' { PT _ (TS _ 11) }
  '.' { PT _ (TS _ 12) }
  '/' { PT _ (TS _ 13) }
  ':' { PT _ (TS _ 14) }
  ';' { PT _ (TS _ 15) }
  '<' { PT _ (TS _ 16) }
  '<=' { PT _ (TS _ 17) }
  '=' { PT _ (TS _ 18) }
  '==' { PT _ (TS _ 19) }
  '>' { PT _ (TS _ 20) }
  '>=' { PT _ (TS _ 21) }
  'Generator' { PT _ (TS _ 22) }
  'bool' { PT _ (TS _ 23) }
  'else' { PT _ (TS _ 24) }
  'false' { PT _ (TS _ 25) }
  'for' { PT _ (TS _ 26) }
  'func' { PT _ (TS _ 27) }
  'func*' { PT _ (TS _ 28) }
  'if' { PT _ (TS _ 29) }
  'int' { PT _ (TS _ 30) }
  'next' { PT _ (TS _ 31) }
  'print' { PT _ (TS _ 32) }
  'println' { PT _ (TS _ 33) }
  'return' { PT _ (TS _ 34) }
  'string' { PT _ (TS _ 35) }
  'true' { PT _ (TS _ 36) }
  'void' { PT _ (TS _ 37) }
  'while' { PT _ (TS _ 38) }
  'yield' { PT _ (TS _ 39) }
  '{' { PT _ (TS _ 40) }
  '||' { PT _ (TS _ 41) }
  '}' { PT _ (TS _ 42) }

L_ident  { PT _ (TV $$) }
L_integ  { PT _ (TI $$) }
L_quoted { PT _ (TL $$) }


%%

Ident   :: { Ident }   : L_ident  { Ident $1 }
Integer :: { Integer } : L_integ  { (read ( $1)) :: Integer }
String  :: { String }  : L_quoted {  $1 }

Program :: { Program }
Program : ListStmt { AbsXYZgrammar.Program (reverse $1) }
Block :: { Block }
Block : '{' ListStmt '}' { AbsXYZgrammar.Block (reverse $2) }
ListStmt :: { [Stmt] }
ListStmt : {- empty -} { [] } | ListStmt Stmt { flip (:) $1 $2 }
Stmt :: { Stmt }
Stmt : ';' { AbsXYZgrammar.Empty }
     | Block { AbsXYZgrammar.BStmt $1 }
     | Type ListItem ';' { AbsXYZgrammar.Decl $1 $2 }
     | Ident '=' Expr ';' { AbsXYZgrammar.Ass $1 $3 }
     | 'return' Expr ';' { AbsXYZgrammar.Ret $2 }
     | 'return' ';' { AbsXYZgrammar.VRet }
     | 'if' '(' Expr ')' Block { AbsXYZgrammar.Cond $3 $5 }
     | 'if' '(' Expr ')' Block 'else' Block { AbsXYZgrammar.CondElse $3 $5 $7 }
     | 'while' '(' Expr ')' Block { AbsXYZgrammar.While $3 $5 }
     | 'for' '(' Ident ':' Expr ')' Block { AbsXYZgrammar.ForGen $3 $5 $7 }
     | Expr ';' { AbsXYZgrammar.SExp $1 }
     | 'func' Type Ident '(' ListArg ')' Block { AbsXYZgrammar.Function $2 $3 $5 $7 }
     | 'func*' Type Ident '(' ListArg ')' Block { AbsXYZgrammar.GeneratorDef $2 $3 $5 $7 }
     | 'yield' Expr ';' { AbsXYZgrammar.Yield $2 }
     | 'print' '(' Expr ')' ';' { AbsXYZgrammar.Print $3 }
     | 'println' '(' Expr ')' ';' { AbsXYZgrammar.PrintLn $3 }
Item :: { Item }
Item : Ident { AbsXYZgrammar.NoInit $1 }
     | Ident '=' Expr { AbsXYZgrammar.Init $1 $3 }
ListItem :: { [Item] }
ListItem : Item { (:[]) $1 } | Item ',' ListItem { (:) $1 $3 }
Arg :: { Arg }
Arg : Type Ident { AbsXYZgrammar.ValArg $1 $2 }
    | Type '&' Ident { AbsXYZgrammar.RefArg $1 $3 }
ListArg :: { [Arg] }
ListArg : {- empty -} { [] }
        | Arg { (:[]) $1 }
        | Arg ',' ListArg { (:) $1 $3 }
Expr6 :: { Expr }
Expr6 : Ident '.' 'next' '(' ')' { AbsXYZgrammar.ENextGen $1 }
      | Ident { AbsXYZgrammar.EVar $1 }
      | Integer { AbsXYZgrammar.ELitInt $1 }
      | 'true' { AbsXYZgrammar.ELitTrue }
      | 'false' { AbsXYZgrammar.ELitFalse }
      | Ident '(' ListExpr ')' { AbsXYZgrammar.EApp $1 $3 }
      | String { AbsXYZgrammar.EString $1 }
      | '(' Expr ')' { $2 }
Type :: { Type }
Type : 'int' { AbsXYZgrammar.Int }
     | 'string' { AbsXYZgrammar.Str }
     | 'bool' { AbsXYZgrammar.Bool }
     | 'void' { AbsXYZgrammar.Void }
     | 'Generator' '<' Type '>' { AbsXYZgrammar.Generator $3 }
Expr5 :: { Expr }
Expr5 : '-' Expr6 { AbsXYZgrammar.Neg $2 }
      | '!' Expr6 { AbsXYZgrammar.Not $2 }
      | Expr6 { $1 }
Expr4 :: { Expr }
Expr4 : Expr4 MulOp Expr5 { AbsXYZgrammar.EMul $1 $2 $3 }
      | Expr5 { $1 }
Expr3 :: { Expr }
Expr3 : Expr3 AddOp Expr4 { AbsXYZgrammar.EAdd $1 $2 $3 }
      | Expr4 { $1 }
Expr2 :: { Expr }
Expr2 : Expr2 RelOp Expr3 { AbsXYZgrammar.ERel $1 $2 $3 }
      | Expr3 { $1 }
Expr1 :: { Expr }
Expr1 : Expr2 '&&' Expr1 { AbsXYZgrammar.EAnd $1 $3 }
      | Expr2 { $1 }
Expr :: { Expr }
Expr : Expr1 '||' Expr { AbsXYZgrammar.EOr $1 $3 } | Expr1 { $1 }
ListExpr :: { [Expr] }
ListExpr : {- empty -} { [] }
         | Expr { (:[]) $1 }
         | Expr ',' ListExpr { (:) $1 $3 }
AddOp :: { AddOp }
AddOp : '+' { AbsXYZgrammar.Plus } | '-' { AbsXYZgrammar.Minus }
MulOp :: { MulOp }
MulOp : '*' { AbsXYZgrammar.Times }
      | '/' { AbsXYZgrammar.Div }
      | '%' { AbsXYZgrammar.Mod }
RelOp :: { RelOp }
RelOp : '<' { AbsXYZgrammar.LTH }
      | '<=' { AbsXYZgrammar.LE }
      | '>' { AbsXYZgrammar.GTH }
      | '>=' { AbsXYZgrammar.GE }
      | '==' { AbsXYZgrammar.EQU }
      | '!=' { AbsXYZgrammar.NE }
{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
}

