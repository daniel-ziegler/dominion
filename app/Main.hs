module Main where

import Data.List.NonEmpty (NonEmpty(..))

import Dominion
import Game
import Players

import Control.Monad.Random
import Debug.Trace
import System.Random

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
  setStdGen $ mkStdGen 0
  runExampleBigstep
