module Utils where

import Types
import AbsXYZgrammar

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Data.Map as Map
import Data.Maybe
import Prelude hiding(lookup)


getInterpreterMode :: PStateMonad Mode
getInterpreterMode = do
  (_, _, mode) <- get
  return mode

getIdentLoc :: Ident -> PStateMonad Location
getIdentLoc (Ident s) = do
  Just location <- asks $ lookup s
  return location

updateGen :: Location -> [Stmt] -> Env -> PStateMonad Result
updateGen loc stmts env = do
  (mem, newLoc, mode) <- get
  let Just (GenVar (returnType, _, _)) = lookup loc mem

  put (insert loc (GenVar (returnType, stmts, env)) mem, newLoc, mode)
  justReturn

getVar :: Ident -> PStateMonad Memory
getVar (Ident s) = do
  Just location <- asks $ lookup s

  (mem, _, _) <- get
  return $ fromJust $ lookup location mem

setVar :: Ident -> Memory -> PStateMonad Result
setVar (Ident s) val = do
  Just loc <- asks $ lookup s
  (mem, newLoc, mode) <- get
  put (insert loc val mem, newLoc, mode)
  justReturn

addVarToEnv :: Env -> Ident -> Memory -> PStateMonad Result
addVarToEnv env (Ident s) val = do
  (mem, newLoc, mode) <- get
  put (insert newLoc val mem, newLoc + 1, mode)
  return (Nothing, insert s newLoc env)

addVar :: Ident -> Memory -> PStateMonad Result
addVar ident val = do
  env <- ask
  result <- addVarToEnv env ident val
  return result

addFunc :: Ident -> (Type, [Arg], [Stmt]) -> PStateMonad Result
addFunc (Ident s) (returnType, args, stmts) = do
  env <- ask
  (mem, newLoc, mode) <- get
  let newEnv = insert s newLoc env

  put (insert newLoc (FuncDef (returnType, args, stmts, newEnv)) mem, newLoc + 1, mode)
  return (Nothing, newEnv)

addGen :: Ident -> (Type, [Arg], [Stmt]) -> PStateMonad Result
addGen (Ident s) (returnType, args, stmts) = do
  env <- ask
  (mem, newLoc, mode) <- get
  let newEnv = insert s newLoc env

  put (insert newLoc (GenDef (returnType, args, stmts, newEnv)) mem, newLoc + 1, mode)
  return (Nothing, newEnv)

isVoid :: Type -> Bool
isVoid Void = True
isVoid _ = False

justReturn :: PStateMonad Result
justReturn = do
  env <- ask
  return (Nothing, env)

justReturnGen :: PStateMonad GenResult
justReturnGen = do
  env <- ask
  return (Nothing, [], env)
