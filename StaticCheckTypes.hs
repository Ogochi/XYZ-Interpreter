module StaticCheckTypes where

import AbsXYZgrammar

import Data.Map as Map
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.Except

data StaticCheckMemory =
    Var Type
  | Func (Type, [Type]) -- return type, arg types

type StaticCheckEnv = Map String StaticCheckMemory
type ExtendedEnv = (StaticCheckEnv, Type)

type StaticCheckMonad = ReaderT ExtendedEnv (ExceptT StaticCheckException IO)

data StaticCheckException =
    WrongTypeException String
  | UndefinedException String
