module StaticCheckUtils where

import StaticCheckTypes
import AbsXYZgrammar

import Data.Map as Map
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.Except
import Prelude hiding(lookup)


getMemory :: Ident -> StaticCheckMonad StaticCheckMemory
getMemory (Ident s) = do
  (env, _, _) <- ask
  let variable = lookup s env
  case variable of
    Nothing -> throwError $ UndefinedException s
    Just mem -> return mem

argsToTypesList :: [Arg] -> [Type]
argsToTypesList [] = []
argsToTypesList (arg:rest) = (argToType arg) : argsToTypesList rest

argToType :: Arg -> Type
argToType (ValArg argType _) = argType
argToType (RefArg argType _) = argType
