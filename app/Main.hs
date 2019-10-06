module Main where

import Sim

import Control.Monad.Loops
import Control.Monad.Random

main :: IO ()
main = do
  st <- evalRandIO $ initState 2
  (scores, st) <- evalRandIO $ run (const dummyPlayer) st game
  print scores
  print st
