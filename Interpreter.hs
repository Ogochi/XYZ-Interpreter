module Interpreter where

import AbsGrammar
import Types

import Data.Map as Map
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

runInterpret :: [Stmt] -> Mode -> IO (Either RuntimeException (Maybe Memory))
runInterpret tree mode = runExceptT $ evalStateT (runReaderT (interpretTree tree) Map.empty) (Map.empty, 0, mode)

interpretTree :: [Stmt] -> PStateMonad
interpretTree _ = do
  throwError OperationNotSupportedException
