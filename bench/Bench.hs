module Main where

import Control.Monad.Random
import Criterion.Main

import Dominion
import Game
import Players

examplePlayers (Player 0) = mcsPlayer 0
examplePlayers (Player 1) = randomPlayer

runExampleSmallstep :: IO ()
runExampleSmallstep = do
  st <- evalRandIO $ initState 2
  (scores, st) <- smallstepRunGame examplePlayers st game
  print scores

runExampleBigstep :: IO ()
runExampleBigstep = do
  st <- evalRandIO $ initState 2
  (scores, st) <- bigstepRunGame examplePlayers st game
  print scores

main = do
  defaultMain
    [ bgroup
        "game"
        [bench "smallstep" $ nfIO runExampleSmallstep, bench "bigstep" $ nfIO runExampleBigstep]
    ]
