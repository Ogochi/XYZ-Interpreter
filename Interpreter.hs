module Interpreter where

import AbsGrammar
import Types

import Data.Map as Map
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

runInterpret :: [Stmt] -> Mode -> IO (Either RuntimeException (Maybe Memory))
runInterpret tree mode = runExceptT $ evalStateT (runReaderT (interpretStmts tree) Map.empty) (Map.empty, 0, mode)

interpretStmts :: [Stmt] -> PStateMonad
interpretStmts (x:rest) = do
  result <- execStmt x
  if isJust result then
    return result
  else do
    restResult <- interpretStmts rest
    return restResult
interpretStmts [] = return Nothing

execStmt :: Stmt -> PStateMonad

-- Empty
execStmt Empty = return Nothing

-- Block
execStmt (BStmt (Block stmts)) = do
  result <- local id (interpretStmts stmts)
  return result

-- SExp
execStmt (SExp expr) = do
  result <- evalExp expr

  return Nothing

evalExp :: Expr -> PStateMonad

evalExp (ELitInt num) = return $ Just (IntVar num)
evalExp ELitTrue = return $ Just (BoolVar True)
evalExp ELitFalse = return $ Just (BoolVar False)
evalExp (EString s) = return $ Just (StringVar s)

evalExp (EAdd exp1 addOp exp2) = do
  Just res1 <- evalExp exp1
  Just res2 <- evalExp exp2
  return $ Just (makeAddOp addOp res1 res2)

evalExp (EMul exp1 mulOp exp2) = do
  Just res1 <- evalExp exp1
  Just res2 <- evalExp exp2
  result <- makeMulOp mulOp res1 res2
  return result

makeAddOp :: AddOp -> Memory -> Memory -> Memory
makeAddOp Plus (StringVar s1) (StringVar s2) = StringVar $ s1 ++ s2
makeAddOp Plus (IntVar i1) (IntVar i2) = IntVar $ i1 + i2
makeAddOp Minus (IntVar i1) (IntVar i2) = IntVar $ i1 - i2

makeMulOp :: MulOp -> Memory -> Memory -> PStateMonad
makeMulOp Times (IntVar i1) (IntVar i2) = return $ Just $ IntVar $ i1 * i2
makeMulOp Mod (IntVar i1) (IntVar i2) = return $ Just $ IntVar $ i1 `mod` i2
makeMulOp Div (IntVar i1) (IntVar i2) = do
  if i2 == 0 then
    throwError ZeroDivException
  else
    return $ Just $ IntVar $ quot i1 i2
