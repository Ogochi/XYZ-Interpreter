-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParGrammar where
import AbsGrammar
import LexGrammar
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
  ';' { PT _ (TS _ 14) }
  '<' { PT _ (TS _ 15) }
  '<=' { PT _ (TS _ 16) }
  '=' { PT _ (TS _ 17) }
  '==' { PT _ (TS _ 18) }
  '>' { PT _ (TS _ 19) }
  '>=' { PT _ (TS _ 20) }
  '[' { PT _ (TS _ 21) }
  '[]' { PT _ (TS _ 22) }
  ']' { PT _ (TS _ 23) }
  'add' { PT _ (TS _ 24) }
  'bool' { PT _ (TS _ 25) }
  'drop' { PT _ (TS _ 26) }
  'else' { PT _ (TS _ 27) }
  'false' { PT _ (TS _ 28) }
  'func' { PT _ (TS _ 29) }
  'func*' { PT _ (TS _ 30) }
  'gen' { PT _ (TS _ 31) }
  'if' { PT _ (TS _ 32) }
  'int' { PT _ (TS _ 33) }
  'length' { PT _ (TS _ 34) }
  'next' { PT _ (TS _ 35) }
  'print' { PT _ (TS _ 36) }
  'return' { PT _ (TS _ 37) }
  'string' { PT _ (TS _ 38) }
  'struct' { PT _ (TS _ 39) }
  'true' { PT _ (TS _ 40) }
  'void' { PT _ (TS _ 41) }
  'while' { PT _ (TS _ 42) }
  'yeld' { PT _ (TS _ 43) }
  '{' { PT _ (TS _ 44) }
  '||' { PT _ (TS _ 45) }
  '}' { PT _ (TS _ 46) }

L_ident  { PT _ (TV $$) }
L_integ  { PT _ (TI $$) }
L_quoted { PT _ (TL $$) }


%%

Ident   :: { Ident }   : L_ident  { Ident $1 }
Integer :: { Integer } : L_integ  { (read ( $1)) :: Integer }
String  :: { String }  : L_quoted {  $1 }

Program :: { Program }
Program : ListStmt { AbsGrammar.Program (reverse $1) }
Block :: { Block }
Block : '{' ListStmt '}' { AbsGrammar.Block (reverse $2) }
ListStmt :: { [Stmt] }
ListStmt : {- empty -} { [] } | ListStmt Stmt { flip (:) $1 $2 }
Stmt :: { Stmt }
Stmt : ';' { AbsGrammar.Empty }
     | Block { AbsGrammar.BStmt $1 }
     | Type ListItem ';' { AbsGrammar.Decl $1 $2 }
     | Ident '=' Expr ';' { AbsGrammar.Ass $1 $3 }
     | Ident '.' Field '=' Expr ';' { AbsGrammar.StructAss $1 $3 $5 }
     | 'return' Expr ';' { AbsGrammar.Ret $2 }
     | 'return' ';' { AbsGrammar.VRet }
     | 'if' '(' Expr ')' Block { AbsGrammar.Cond $3 $5 }
     | 'if' '(' Expr ')' Block 'else' Block { AbsGrammar.CondElse $3 $5 $7 }
     | 'while' '(' Expr ')' Block { AbsGrammar.While $3 $5 }
     | Expr ';' { AbsGrammar.SExp $1 }
     | 'func' Type Ident '(' ListArg ')' Block { AbsGrammar.Function $2 $3 $5 $7 }
     | 'func*' Type Ident '(' ListArg ')' GenBlock { AbsGrammar.GeneratorDef $2 $3 $5 $7 }
     | 'struct' Ident '{' ListStructItem '}' { AbsGrammar.StructDef $2 $4 }
     | 'print' Expr ';' { AbsGrammar.Print $2 }
     | Ident '.' 'drop' '(' ')' ';' { AbsGrammar.ListDrop $1 }
     | Ident '.' 'add' '(' Expr ')' ';' { AbsGrammar.ListAdd $1 $5 }
Item :: { Item }
Item : Ident { AbsGrammar.NoInit $1 }
     | Ident '=' Expr { AbsGrammar.Init $1 $3 }
ListItem :: { [Item] }
ListItem : Item { (:[]) $1 } | Item ',' ListItem { (:) $1 $3 }
Arg :: { Arg }
Arg : Type Ident { AbsGrammar.ValArg $1 $2 }
    | Type '&' Ident { AbsGrammar.RefArg $1 $3 }
ListArg :: { [Arg] }
ListArg : {- empty -} { [] }
        | Arg { (:[]) $1 }
        | Arg ',' ListArg { (:) $1 $3 }
GenBlock :: { GenBlock }
GenBlock : '{' ListGenStmt '}' { AbsGrammar.GenBlock (reverse $2) }
GenStmt :: { GenStmt }
GenStmt : Stmt { AbsGrammar.GenStmt $1 }
        | 'yeld' Expr ';' { AbsGrammar.Yeld $2 }
ListGenStmt :: { [GenStmt] }
ListGenStmt : {- empty -} { [] }
            | ListGenStmt GenStmt { flip (:) $1 $2 }
StructItem :: { StructItem }
StructItem : Type Ident { AbsGrammar.StructItem $1 $2 }
ListStructItem :: { [StructItem] }
ListStructItem : {- empty -} { [] }
               | StructItem { (:[]) $1 }
               | StructItem ';' ListStructItem { (:) $1 $3 }
Expr6 :: { Expr }
Expr6 : Ident '.' 'length' { AbsGrammar.EListLength $1 }
      | Ident '[' Expr ']' { AbsGrammar.EListElem $1 $3 }
      | Ident '.' Field { AbsGrammar.EStructField $1 $3 }
      | Ident '.' 'next' '(' ')' { AbsGrammar.ENextGen $1 }
      | Ident { AbsGrammar.EVar $1 }
      | Integer { AbsGrammar.ELitInt $1 }
      | '[]' { AbsGrammar.ELitList }
      | 'true' { AbsGrammar.ELitTrue }
      | 'false' { AbsGrammar.ELitFalse }
      | Ident '(' ListExpr ')' { AbsGrammar.EApp $1 $3 }
      | String { AbsGrammar.EString $1 }
      | '(' Expr ')' { $2 }
Field :: { Field }
Field : Ident { AbsGrammar.SingleField $1 }
      | Ident '.' Ident { AbsGrammar.ManyFields $1 $3 }
Type :: { Type }
Type : 'int' { AbsGrammar.Int }
     | 'string' { AbsGrammar.Str }
     | 'bool' { AbsGrammar.Bool }
     | 'void' { AbsGrammar.Void }
     | '[' Type ']' { AbsGrammar.List $2 }
     | 'struct' Ident { AbsGrammar.Struct $2 }
     | 'gen' Type Ident { AbsGrammar.Generator $2 $3 }
Expr5 :: { Expr }
Expr5 : '-' Expr6 { AbsGrammar.Neg $2 }
      | '!' Expr6 { AbsGrammar.Not $2 }
      | Expr6 { $1 }
Expr4 :: { Expr }
Expr4 : Expr4 MulOp Expr5 { AbsGrammar.EMul $1 $2 $3 }
      | Expr5 { $1 }
Expr3 :: { Expr }
Expr3 : Expr3 AddOp Expr4 { AbsGrammar.EAdd $1 $2 $3 }
      | Expr4 { $1 }
Expr2 :: { Expr }
Expr2 : Expr2 RelOp Expr3 { AbsGrammar.ERel $1 $2 $3 }
      | Expr3 { $1 }
Expr1 :: { Expr }
Expr1 : Expr2 '&&' Expr1 { AbsGrammar.EAnd $1 $3 } | Expr2 { $1 }
Expr :: { Expr }
Expr : Expr1 '||' Expr { AbsGrammar.EOr $1 $3 } | Expr1 { $1 }
ListExpr :: { [Expr] }
ListExpr : {- empty -} { [] }
         | Expr { (:[]) $1 }
         | Expr ',' ListExpr { (:) $1 $3 }
AddOp :: { AddOp }
AddOp : '+' { AbsGrammar.Plus } | '-' { AbsGrammar.Minus }
MulOp :: { MulOp }
MulOp : '*' { AbsGrammar.Times }
      | '/' { AbsGrammar.Div }
      | '%' { AbsGrammar.Mod }
RelOp :: { RelOp }
RelOp : '<' { AbsGrammar.LTH }
      | '<=' { AbsGrammar.LE }
      | '>' { AbsGrammar.GTH }
      | '>=' { AbsGrammar.GE }
      | '==' { AbsGrammar.EQU }
      | '!=' { AbsGrammar.NE }
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

