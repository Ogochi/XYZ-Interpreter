module StaticCheck where

import AbsXYZgrammar
import Utils

import Data.Map as Map
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.Except


data StaticCheckMemory =
    Var Type
  | Func (Type, [Type])

type StaticCheckEnv = Map String StaticCheckMemory
type ExtendedEnv = (StaticCheckEnv, Type)

type StaticCheckMonad = ReaderT ExtendedEnv (ExceptT StaticCheckException IO)

data StaticCheckException =
    WrongTypeException String


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

-- Decl
checkStmt (Decl declType []) = return Nothing
checkStmt (Decl declType (item:rest)) = do
  (_, funcType) <- ask
  Just newEnv <- checkDeclItem declType item
  restResult <- local (const (newEnv, funcType)) $ checkStmt (Decl declType rest)
  return restResult

checkDeclItem :: Type -> Item -> StaticCheckMonad (Maybe StaticCheckEnv)
checkDeclItem itemType (NoInit (Ident s)) = do
  (env, funcType) <- ask
  return $ Just $ insert s (Var itemType) env

checkDeclItem itemType (Init (Ident s) exp) = return Nothing -- TODO

checkExp :: Expr -> StaticCheckMonad Type

checkExp (EString _) = return Str
checkExp ELitTrue = return Bool
checkExp ELitFalse = return Bool
checkExp (ELitInt _) = return Int
checkExp (EOr exp1 exp2) = do
  t1 <- checkExp exp1
  t2 <- checkExp exp2
  if not (t1 == Bool) || not (t1 == t2)
    then throwError $ WrongTypeException "OR requires boolean values"
    else return Bool
