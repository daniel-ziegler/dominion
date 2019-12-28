{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

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

firstChoicePlayer :: Monad m => PlayerImpl m r
firstChoicePlayer (PickMechanic ms) gs cont = return $ NE.head ms

randomPlayer :: MonadRandom m => PlayerImpl m r
randomPlayer (PickMechanic ms) gs cont = getRandomChoice ms

promptPlayer :: PlayerImpl IO r
promptPlayer (PickMechanic ms) gs cont =
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
  => Int
  -> PlayerImpl m [Int]
mcsPlayer player (PickMechanic ms) gs cont = do
  values <- traverse evalMove ms -- TODO: dedup moves
  return $ snd $ maximumBy (comparing fst) $ NE.zip values ms
    -- TODO: multiple rollouts, parallelism
  where
    evalMove :: Mechanic -> m Double
    evalMove mv = do
      (scores, _) <- bigstepRunGame (const randomPlayer) gs (cont mv)
      let ourScore = scores !! player
      let maxOther = maximum $ deleteAt player scores
      return $ fromInteger $ toInteger $ ourScore - maxOther

deleteAt idx xs = l ++ r
  where
    (l, (_:r)) = splitAt idx xs
