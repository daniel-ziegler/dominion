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

import Dominion
import Game

firstChoicePlayer :: Monad m => DPlayerImpl m r
firstChoicePlayer mvs gs cont = return $ NE.head mvs

randomPlayer :: MonadRandom m => DPlayerImpl m r
randomPlayer mvs gs cont = getRandomChoice mvs

promptPlayer :: DPlayerImpl IO r
promptPlayer mvs gs cont =
  if NE.length mvs == 1
    then return $ NE.head mvs
    else do
      print gs
      putStr "Moves: "
      print $ zip [0 ..] $ NE.toList mvs
      index <-
        untilJust $
        runMaybeT $ do
          liftIO $ putStr "Which? "
          choiceStr <- liftIO $ getLine
          ix <- MaybeT $ return $ readMaybe choiceStr
          if ix < NE.length mvs
            then return ix
            else mzero
      return $ mvs NE.!! index

mcsPlayer ::
     forall m. MonadRandom m
  => Int
  -> DPlayerImpl m [Int]
mcsPlayer player mvs gs cont = do
  values <- traverse evalMove mvs -- TODO: dedup moves
  return $ snd $ maximumBy (comparing fst) $ NE.zip values mvs
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
