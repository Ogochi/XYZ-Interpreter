module StaticCheck where

import AbsXYZgrammar
import StaticCheckUtils
import StaticCheckTypes

import Data.Maybe
import Data.Map  as Map hiding(foldl)
import Control.Monad.Reader
import Control.Monad.Except

runStaticCheck tree = runExceptT $ runReaderT (checkStmts tree) (Map.empty, Void)

checkStmts :: [Stmt] -> StaticCheckMonad (Maybe StaticCheckEnv)
checkStmts [] = return Nothing
checkStmts (stmt:rest) = do
  checkResult <- checkStmt stmt

  case checkResult of
    Nothing -> do
      result <- checkStmts rest
      return result
    Just newEnv -> do
      (_, funcType) <- ask
      result <- local (const (newEnv, funcType)) $ checkStmts rest
      return result

checkStmt :: Stmt -> StaticCheckMonad (Maybe StaticCheckEnv)

-- BLock
checkStmt (BStmt (Block stmts)) = do
  result <- local id $ checkStmts stmts
  return result

-- Empty
checkStmt Empty = return Nothing

-- Print
checkStmt (Print exp) = do
  checkExp exp
  return Nothing

-- Decl
checkStmt (Decl declType []) = do
  (env, _) <- ask
  return $ Just env
checkStmt (Decl declType (item:rest)) = do
  (_, funcType) <- ask
  Just newEnv <- checkDeclItem declType item
  restResult <- local (const (newEnv, funcType)) $ checkStmt (Decl declType rest)
  return restResult

-- Ass
checkStmt (Ass ident exp) = do
  expType <- checkExp exp
  mem <- getMemory ident
  case mem of
    Func _ -> throwError $ WrongTypeException "Couldn't assign value to function."
    Var varType -> if varType == expType
      then return Nothing
      else throwError $ WrongTypeException "Assignment value type different from variable type."

-- SExp
checkStmt (SExp exp) = do
  checkExp exp
  return Nothing

-- If
checkStmt (CondElse exp b1 b2) = do
  expType <- checkExp exp
  case expType of
    Bool -> do
      result1 <- checkStmt (BStmt b1)
      result2 <- checkStmt (BStmt b2)
      return Nothing
    _ -> throwError $ WrongTypeException "Expression inside If should be boolean."

checkStmt (Cond exp block) = checkStmt (CondElse exp block (Block []))

-- While
checkStmt (While exp block) = checkStmt (Cond exp block)

-- Function
checkStmt (Function returnType (Ident s) args (Block stmts)) = do
  (env, _) <- ask
  let newEnv = insert s (Func (returnType, argsToTypesList args)) env
  extendedEnv <- extendEnvByArgs newEnv args
  result <- local (const extendedEnv) $ checkFuncBlock stmts
  return $ Just newEnv

-- Helper functions

-- TODO
extendEnvByArgs :: StaticCheckEnv -> [Arg] -> StaticCheckMonad ExtendedEnv
extendEnvByArgs _ _ = return (Map.empty, Int)

checkFuncBlock :: [Stmt] -> StaticCheckMonad (Maybe StaticCheckEnv)
checkFuncBlock _ = return Nothing

checkDeclItem :: Type -> Item -> StaticCheckMonad (Maybe StaticCheckEnv)
checkDeclItem itemType (NoInit (Ident s)) = do
  (env, funcType) <- ask
  return $ Just $ insert s (Var itemType) env

checkDeclItem itemType (Init ident exp) = do
  expType <- checkExp exp
  if expType == itemType
    then checkDeclItem itemType (NoInit ident)
    else throwError $ WrongTypeException "Initialization value type different from variable type."

-- Expressions
checkExp :: Expr -> StaticCheckMonad Type

checkExp (EString _) = return Str
checkExp ELitTrue = return Bool
checkExp ELitFalse = return Bool
checkExp (ELitInt _) = return Int
checkExp (EOr exp1 exp2) =
  expOperation exp1 exp2 Bool "OR requires boolean values." Bool

checkExp (EAnd exp1 exp2) =
  expOperation exp1 exp2 Bool "AND requires boolean values." Bool

checkExp (ERel exp1 _ exp2) =
  expOperation exp1 exp2 Int "Relational operators requires integer values." Bool

checkExp (EAdd exp1 addOp exp2) = do
  case addOp of
    Plus -> do
      t1 <- checkExp exp1
      t2 <- checkExp exp2
      result <- addOperation t1 t2
      return result
    Minus -> expOperation exp1 exp2 Int "Substraction requires integer values." Int

checkExp (EMul exp1 _ exp2) =
  expOperation exp1 exp2 Int "Multiplication requires integer values." Int

checkExp (Neg exp) =
  expOperation exp (ELitInt 0) Int "Negation requires integer values." Int

checkExp (Not exp) =
  expOperation exp ELitTrue Bool "NOT requires boolean values." Bool

checkExp (EVar ident) = do
  Var varType <- getMemory ident
  return varType

addOperation :: Type -> Type -> StaticCheckMonad Type
addOperation Str Str = return Str
addOperation Int Int = return Int
addOperation _ _ = throwError $ WrongTypeException "Adding requires integer or string values."

expOperation :: Expr -> Expr -> Type -> String -> Type -> StaticCheckMonad Type
expOperation exp1 exp2 requiredType exceptionMsg returnType = do
  t1 <- checkExp exp1
  t2 <- checkExp exp2
  if not (t1 == requiredType) || not (t1 == t2)
    then throwError $ WrongTypeException exceptionMsg
    else return returnType
