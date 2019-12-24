{-# LANGUAGE GADTs #-}

module Main where

import Sim

import Control.Monad
import Control.Monad.Loops
import Control.Monad.Random
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import qualified Data.List.NonEmpty as NE

import Text.Read

promptPlayer :: PlayerImpl IO
promptPlayer (PickMechanic ms) gs =
  if NE.length ms == 1
    then return $ NE.head ms
    else do
      print gs
      putStr "Moves: "
      print $ zip [0 ..] $ NE.toList ms
      index <-
        untilJust $
        runMaybeT $ do
          liftIO $ putStr "Which? "
          choiceStr <- liftIO $ getLine
          ix <- MaybeT $ return $ readMaybe choiceStr
          if ix < NE.length ms
            then return ix
            else mzero
      return $ ms NE.!! index

main :: IO ()
main = do
  st <- evalRandIO $ initState 2
  (scores, st) <- run players st game
  print scores
  print st
  where
    players (Player 0) = randomPlayer
    players (Player 1) = promptPlayer
