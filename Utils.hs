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

getNewLocation :: PStateMonad Location
getNewLocation = do
  (mem, location, mode) <- get
  put (mem, location + 1, mode)
  return location

getVar :: Ident -> PStateMonad Memory
getVar (Ident s) = do
  Just location <- asks $ lookup s
  (mem, _, _) <- get
  return $ fromJust $ lookup location mem
