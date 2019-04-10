module Utils where

import Types

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

getInterpreterMode :: PStateMonad Mode
getInterpreterMode = do
  (_, _, mode) <- get
  return mode
