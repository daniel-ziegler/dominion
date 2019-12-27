module Main where

--import Criterion.Main
import Players
import Sim

import Control.Monad.Random

examplePlayers (Player 0) = mcsPlayer 0
examplePlayers (Player 1) = promptPlayer

runExampleSmallstep :: IO ()
runExampleSmallstep = do
  st <- evalRandIO $ initState 2
  (scores, st) <- smallstepRunGame examplePlayers st game
  print scores

runExampleBigstep :: IO ()
runExampleBigstep = do
  st <- evalRandIO $ initState 2
  (scores, st) <- bigstepRunGame examplePlayers st game Nil
  print scores

main = runExampleBigstep
{-
  defaultMain
    [bgroup "game" [bench "smallstep" $ nfIO runExample, bench "bigstep" $ nfIO runExampleBigstep]]-}
