module Main where

import Control.Monad.Random
import Criterion.Main
import System.Random

import Dominion
import Game
import Players

examplePlayers (Player 0) = mcsPlayer 0
examplePlayers (Player 1) = randomPlayer

runExampleSmallstep :: IO ()
runExampleSmallstep = do
  setStdGen $ mkStdGen 0
  st <- evalRandIO $ initState 2
  (scores, st) <- smallstepRunGame examplePlayers st game
  print scores

runExampleBigstep :: IO ()
runExampleBigstep = do
  setStdGen $ mkStdGen 0
  st <- evalRandIO $ initState 2
  (scores, st) <- bigstepRunGame examplePlayers st game
  print scores

main = do
  defaultMain
    [ bgroup
        "game"
        [bench "bigstep" $ nfIO runExampleBigstep] {-bench "smallstep" $ nfIO runExampleSmallstep,-}
    ]
