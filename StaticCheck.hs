module StaticCheck where

import AbsXYZgrammar

import Data.Map as Map
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.Except


data StaticCheckMemory =
    Var Type
  | Func (Type, [Type])

type StaticCheckEnv = Map String StaticCheckMemory

type StaticCheckMonad = ReaderT StaticCheckEnv (ExceptT StaticCheckException IO) (Maybe String)

data StaticCheckException =
    WrongTypeException


runStaticCheck tree = runExceptT $ runReaderT (checkStmts tree) Map.empty

checkStmts :: [Stmt] -> StaticCheckMonad
checkStmts [] = return Nothing
