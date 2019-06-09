module SkelXYZgrammar where

-- Haskell module generated by the BNF converter

import AbsXYZgrammar
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident string -> failure x
transProgram :: Program -> Result
transProgram x = case x of
  Program stmts -> failure x
transBlock :: Block -> Result
transBlock x = case x of
  Block stmts -> failure x
transStmt :: Stmt -> Result
transStmt x = case x of
  Empty -> failure x
  BStmt block -> failure x
  Decl type_ items -> failure x
  Ass ident expr -> failure x
  Ret expr -> failure x
  VRet -> failure x
  Cond expr block -> failure x
  CondElse expr block1 block2 -> failure x
  While expr block -> failure x
  ForGen ident expr block -> failure x
  SExp expr -> failure x
  Function type_ ident args block -> failure x
  GeneratorDef type_ ident args block -> failure x
  Yield expr -> failure x
  Print expr -> failure x
transItem :: Item -> Result
transItem x = case x of
  NoInit ident -> failure x
  Init ident expr -> failure x
transArg :: Arg -> Result
transArg x = case x of
  ValArg type_ ident -> failure x
  RefArg type_ ident -> failure x
transType :: Type -> Result
transType x = case x of
  Int -> failure x
  Str -> failure x
  Bool -> failure x
  Void -> failure x
  Generator type_ -> failure x
transExpr :: Expr -> Result
transExpr x = case x of
  ENextGen ident -> failure x
  EVar ident -> failure x
  ELitInt integer -> failure x
  ELitTrue -> failure x
  ELitFalse -> failure x
  EApp ident exprs -> failure x
  EString string -> failure x
  Neg expr -> failure x
  Not expr -> failure x
  EMul expr1 mulop expr2 -> failure x
  EAdd expr1 addop expr2 -> failure x
  ERel expr1 relop expr2 -> failure x
  EAnd expr1 expr2 -> failure x
  EOr expr1 expr2 -> failure x
transAddOp :: AddOp -> Result
transAddOp x = case x of
  Plus -> failure x
  Minus -> failure x
transMulOp :: MulOp -> Result
transMulOp x = case x of
  Times -> failure x
  Div -> failure x
  Mod -> failure x
transRelOp :: RelOp -> Result
transRelOp x = case x of
  LTH -> failure x
  LE -> failure x
  GTH -> failure x
  GE -> failure x
  EQU -> failure x
  NE -> failure x

