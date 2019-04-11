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

getVar :: Ident -> PStateMonad Memory
getVar (Ident s) = do
  Just location <- asks $ lookup s
  (mem, _, _) <- get
  return $ fromJust $ lookup location mem

addVar :: Ident -> Memory -> PStateMonad ReturnResult
addVar (Ident s) val = do
  (mem, newLoc, mode) <- get
  put (insert newLoc val mem, newLoc + 1, mode)
  return Nothing
