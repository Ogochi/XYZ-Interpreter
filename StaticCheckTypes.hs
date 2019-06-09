module StaticCheckTypes where

import AbsXYZgrammar

import Data.Map as Map
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.Except

data StaticCheckMemory =
    Var Type
  | GenVar Type -- return types
  | Func (Type, [Type]) -- return type, arg types
  | Gen (Type, [Type])

type StaticCheckEnv = Map String StaticCheckMemory
type ExtendedEnv = (StaticCheckEnv, Type, Bool) -- Bool - isFunction

type StaticCheckMonad = ReaderT ExtendedEnv (ExceptT StaticCheckException IO)

data StaticCheckException =
    WrongTypeException String
  | UndefinedException String
  | FunctionHasNotValueException
  | CanNotMakeVariableApplicationException
  | WrongArgsCountException String
  | ReturnNotInFunctionException
  | YieldNotInGeneratorException
  | GeneratorHasNotValueException
  | GeneratorVarHasNotValueException
  | NextNotOnGeneratorException
  | ForGenOnlyOverGeneratorException
