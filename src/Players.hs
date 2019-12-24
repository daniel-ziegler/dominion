{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Players where

import Control.Monad.Loops
import Control.Monad.Random
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.List
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Ord
import Text.Read

import Sim

firstChoicePlayer :: Monad m => PlayerImpl m
firstChoicePlayer (PickMechanic ms) gs = return $ NE.head ms

randomPlayer :: MonadRandom m => PlayerImpl m
randomPlayer (PickMechanic ms) gs = getRandomChoice ms

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

mcsPlayer ::
     forall m. MonadRandom m
  => PlayerImpl m
mcsPlayer (PickMechanic ms) gs = do
  values <- traverse evalMove ms
  return $ snd $ maximumBy (comparing fst) $ NE.zip values ms
  where
    evalMove :: Mechanic -> m Double
    evalMove mv = getRandomR (-5.0, 5.0)
