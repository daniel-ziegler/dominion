{-# LANGUAGE GADTs #-}

module Main where

import Sim

import Control.Monad.Loops
import Control.Monad.Random
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import qualified Data.List.NonEmpty as NE

promptPlayer :: PlayerImpl IO
promptPlayer (PickMechanic ms) = do
  print $ NE.toList ms
  return $ NE.head ms

main :: IO ()
main = do
  st <- evalRandIO $ initState 2
  (scores, st) <- run players st game
  print scores
  print st
  where
    players (Player 0) = randomPlayer
    players (Player 1) = promptPlayer
