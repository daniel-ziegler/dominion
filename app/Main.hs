module Main where

import Players
import Sim

import Control.Monad.Random

main :: IO ()
main = do
  st <- evalRandIO $ initState 2
  (scores, st) <- run players st game
  print scores
  print st
  where
    players (Player 0) = mcsPlayer
    players (Player 1) = promptPlayer
