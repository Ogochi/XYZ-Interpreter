module StaticCheck where

import AbsXYZgrammar
import StaticCheckUtils
import StaticCheckTypes

import Data.Maybe
import Data.Map as Map hiding(foldl)
import Control.Monad.Reader
import Control.Monad.Except

runStaticCheck tree = runExceptT $ runReaderT (checkStmts tree) (Map.empty, Void, True)

checkStmts :: [Stmt] -> StaticCheckMonad (Maybe StaticCheckEnv)
checkStmts [] = return Nothing
checkStmts (stmt:rest) = do
  checkResult <- checkStmt stmt

  case checkResult of
    Nothing -> do
      result <- checkStmts rest
      return result
    Just newEnv -> do
      (_, funcType, isFunction) <- ask
      result <- local (const (newEnv, funcType, isFunction)) $ checkStmts rest
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

-- Return
checkStmt (Ret exp) = do
  expType <- checkExp exp
  (_, returnType, isFunction) <- ask

  if not isFunction
    then throwError $ ReturnNotInFunctionException
    else
      if expType == returnType
        then return Nothing
        else throwError $ WrongTypeException "Function type and return type mismatch."

checkStmt VRet = do
  (_, returnType, isFunction) <- ask

  if not isFunction
    then throwError $ ReturnNotInFunctionException
    else
      case returnType of
        Void -> return Nothing
        _ -> throwError $ WrongTypeException "Can't return nothing in non-void function."

-- Decl
checkStmt (Decl declType []) = do
  (env, _, _) <- ask
  return $ Just env
checkStmt (Decl declType (item:rest)) = do
  (_, funcType, isFunction) <- ask
  Just newEnv <- checkDeclItem declType item
  restResult <- local (const (newEnv, funcType, isFunction)) $ checkStmt (Decl declType rest)
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
checkStmt (Function returnType (Ident s) args block) = do
  (env, _, _) <- ask
  let newEnv = insert s (Func (returnType, argsToTypesList args)) env
  let extendedEnv = extendEnvByArgs newEnv args
  result <- local (const (extendedEnv, returnType, True)) $ checkStmt (BStmt block)
  return $ Just newEnv

-- TODO
-- yield tylko w ciałach generatorów
-- generator jest jak funkcja
-- przypisanie do generatora tylko przez aplikację
-- next() tylko na generatorach
-- generator nie ma wartosci w exp
-- Generator
-- checkStmt (Generator returnType (Ident s) args block) = do


-- Helper functions

extendEnvByArgs :: StaticCheckEnv -> [Arg] -> StaticCheckEnv
extendEnvByArgs env [] = env
extendEnvByArgs env ((ValArg argType (Ident s)):rest) =
  extendEnvByArgs (insert s (Var argType) env) rest
extendEnvByArgs env ((RefArg argType (Ident s)):rest) =
  extendEnvByArgs (insert s (Var argType) env) rest

checkDeclItem :: Type -> Item -> StaticCheckMonad (Maybe StaticCheckEnv)
checkDeclItem itemType (NoInit (Ident s)) = do
  (env, funcType, _) <- ask
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
  var <- getMemory ident
  case var of
    Var varType -> return varType
    Func (_, _) -> throwError $ FunctionHasNotValueException

checkExp (EApp ident exps) = do
  memory <- getMemory ident
  case memory of
    Var _ -> throwError CanNotMakeVariableApplicationException
    Func (returnType, argTypes) -> do
      typesFromExps <- expsToTypes exps
      if length typesFromExps == length argTypes
        then if typesFromExps == argTypes
          then return returnType
          else throwError $ WrongTypeException "Args and params types mismatch in function application."
        else let (Ident s) = ident in throwError $ WrongArgsCountException s

-- Helper functions
expsToTypes :: [Expr] -> StaticCheckMonad [Type]
expsToTypes [] = return []
expsToTypes (exp:rest) = do
  expType <- checkExp exp
  restResult <- expsToTypes rest
  return $ expType : restResult

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
