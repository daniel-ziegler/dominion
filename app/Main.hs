module Main where

--import Criterion.Main
import Players
import Sim

import Control.Monad.Random

runExample :: IO ()
runExample = do
  st <- evalRandIO $ initState 2
  (scores, st) <- runGame players st game
  print scores
  where
    players (Player 0) = mcsPlayer 0
    players (Player 1) = promptPlayer

{-
runExampleBigstep :: IO ()
runExampleBigstep = do
  st <- evalRandIO $ initState 2
  (scores, st) <- run players st game
  print scores
  where
    players (Player 0) = mcsPlayer
    players (Player 1) = firstChoicePlayer
-}
main = replicateM_ 100 runExample {-
main =
  defaultMain
    [bgroup "game" [bench "smallstep" $ nfIO runExample]] --, bench "bigstep" $ nfIO runExampleBigstep]]
-}
