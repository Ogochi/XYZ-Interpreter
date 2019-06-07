module Types where

import AbsXYZgrammar

import Data.Map as Map
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

data Mode = StdinMode | FileMode

type IdentString = String
type Location = Integer

type Env = Map IdentString Location

data Memory =
    IntVar Integer
  | BoolVar Bool
  | StringVar String
  | GenVar (Type, [Stmt], Env)
  | FuncDef (Type, [Arg], [Stmt], Env)
  | GenDef (Type, [Arg], [Stmt], Env)
  deriving Eq

type PState = (Map Location Memory, Location, Mode)

data RuntimeException =
    ZeroDivException
  | ZeroModException
  | NoReturnStmtException
  | NoGenResultException
  | WrongRefArgException

-- (mem value we need to pass ex. with return, next env)
type Result = (Maybe Memory, Env)
type GenResult = (Maybe Memory, [Stmt], Env)

-- Main monad holds env and state using Exception
type PStateMonad = ReaderT Env (StateT PState (ExceptT RuntimeException IO))

-- Show for Memory
instance Show Memory where
  show (IntVar i) = show i
  show (BoolVar b) = show b
  show (StringVar s) = s
