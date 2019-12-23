module Main where

import Sim

import Control.Monad.Loops
import Control.Monad.Random

main :: IO ()
main = do
  st <- evalRandIO $ initState 2
  (scores, st) <- evalRandIO $ run (const randomPlayer) st game
  print scores
  print st
  where
    players (Player 0) = randomPlayer
    players (Player 1) = firstChoicePlayer
