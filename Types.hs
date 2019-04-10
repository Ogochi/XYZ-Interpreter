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

data Memory = IntVar Integer | BoolVar Bool | StringVar String deriving (Show, Eq)
type PState = (Map Location Memory, Location, Mode)

data RuntimeException = ZeroDivException | ZeroModException

type ReturnResult = Maybe Memory
type PStateMonad = ReaderT Env (StateT PState (ExceptT RuntimeException IO))
