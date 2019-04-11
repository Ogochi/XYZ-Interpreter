module Interpreter where

import AbsXYZgrammar
import Types
import Utils

import Data.Map as Map
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

runInterpret :: [Stmt] -> Mode -> IO (Either RuntimeException Result)
runInterpret tree mode = runExceptT $ evalStateT (runReaderT (interpretStmts tree) Map.empty) (Map.empty, 0, mode)

interpretStmts :: [Stmt] -> PStateMonad Result
interpretStmts (x:rest) = do
  (result, env) <- execStmt x
  if isJust result then
    return (result, env)
  else do
    restResult <- local (const env) (interpretStmts rest)
    return restResult
interpretStmts [] = justReturn

execStmt :: Stmt -> PStateMonad Result

-- Empty
execStmt Empty = justReturn

-- Block
execStmt (BStmt (Block stmts)) = do
  result <- local id (interpretStmts stmts)
  return result

-- Print
execStmt (Print exp) = evalExp exp >>= liftIO . putStr . show >> justReturn

-- If
execStmt (Cond exp block) = execStmt $ CondElse exp block (Block [])

execStmt (CondElse exp b1 b2) = do
  BoolVar expVal <- evalExp exp
  result <- execStmt $ BStmt $ if expVal then b1 else b2
  return result

-- While
execStmt (While exp block) = do
  BoolVar expVal <- evalExp exp
  if expVal then do
    result@(mem, env) <- execStmt $ BStmt block
    if isJust mem then
      return result
    else do
      result2 <- execStmt $ While exp block
      return result2
  else
    justReturn

-- Ass
execStmt (Ass ident exp) = evalExp exp >>= setVar ident >>= return

-- Decl
execStmt (Decl declType (item:rest)) = do
  (_, env ) <- addDecl declType item
  result <- local (const env) $ execStmt (Decl declType rest)
  return result
execStmt (Decl _ []) = justReturn

-- SExp
execStmt (SExp exp) = do
  result <- evalExp exp
  mode <- getInterpreterMode
  case mode of
    StdinMode -> liftIO . putStr . show $ result

  justReturn

addDecl :: Type -> Item -> PStateMonad Result
addDecl _ (Init ident exp) = do
  expVal <- evalExp exp
  result <- addVar ident expVal
  return result
addDecl declType (NoInit ident) = do
  let value = case declType of
                Int -> ELitInt 0
                Str -> EString ""
                Bool -> ELitFalse
  result <- addDecl declType (Init ident value)
  return result


evalExp :: Expr -> PStateMonad Memory

evalExp (EVar ident) = getVar ident;

evalExp (ELitInt num) = return $ IntVar num
evalExp ELitTrue = return $ BoolVar True
evalExp ELitFalse = return $ BoolVar False
evalExp (EString s) = return $ StringVar s

evalExp (EAdd exp1 addOp exp2) = do
  res1 <- evalExp exp1
  res2 <- evalExp exp2
  return $ makeAddOp addOp res1 res2

evalExp (EMul exp1 mulOp exp2) = do
  res1 <- evalExp exp1
  res2 <- evalExp exp2
  result <- makeMulOp mulOp res1 res2
  return result

evalExp (ERel exp1 relOp exp2) = do
  res1 <- evalExp exp1
  res2 <- evalExp exp2
  return $ BoolVar (makeRelOp relOp res1 res2)

evalExp (EAnd exp1 exp2) = do
  BoolVar res1 <- evalExp exp1
  if res1 then do
    res2 <- evalExp exp2
    return res2
  else
    return $ BoolVar False

evalExp (EOr exp1 exp2) = do
  BoolVar res1 <- evalExp exp1
  if res1 then
    return $ BoolVar True
  else do
    res2 <- evalExp exp2
    return res2

evalExp (Not exp) = do
  BoolVar res <- evalExp exp
  return $ BoolVar $ not res

evalExp (Neg exp) = do
  IntVar res <- evalExp exp
  return $ IntVar $ -res

makeAddOp :: AddOp -> Memory -> Memory -> Memory
makeAddOp Plus (StringVar s1) (StringVar s2) = StringVar $ s1 ++ s2
makeAddOp Plus (IntVar i1) (IntVar i2) = IntVar $ i1 + i2
makeAddOp Minus (IntVar i1) (IntVar i2) = IntVar $ i1 - i2

makeMulOp :: MulOp -> Memory -> Memory -> PStateMonad Memory
makeMulOp Times (IntVar i1) (IntVar i2) = return $ IntVar $ i1 * i2
makeMulOp Mod (IntVar i1) (IntVar i2) = do
  if i2 == 0 then
    throwError ZeroModException
  else
    return $ IntVar $ i1 `mod` i2
makeMulOp Div (IntVar i1) (IntVar i2) = do
  if i2 == 0 then
    throwError ZeroDivException
  else
    return $ IntVar $ quot i1 i2

makeRelOp :: RelOp -> Memory -> Memory -> Bool
makeRelOp EQU val1 val2 = val1 == val2
makeRelOp NE val1 val2 = val1 /= val2
makeRelOp LTH (IntVar i1) (IntVar i2) = i1 < i2
makeRelOp LE (IntVar i1) (IntVar i2) = i1 <= i2
makeRelOp GTH (IntVar i1) (IntVar i2) = i1 > i2
makeRelOp GE (IntVar i1) (IntVar i2) = i1 >= i2
