{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game
  ( Player(Player)
  , Game()
  , PlayerImpl
  , changeState
  , randomChoice
  , playerChoice
  , note
  , bigstepRunGame
  , smallstepRunGame
  , getState
  , getRandomChoice
  , modifyState
  , inverseMap
  , completeArray
  ) where

import Control.Monad.Random hiding (fromList)
import Control.Monad.State.Lazy
import Data.Array.IArray
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map

newtype Player =
  Player Int
  deriving (Eq, Ord, Show, Ix)

data Game gs mv a where
  Note :: String -> Game gs mv a -> Game gs mv a
  Pure :: a -> Game gs mv a
  Bind :: Game gs mv a -> (a -> Game gs mv b) -> Game gs mv b
  ChangeState :: State gs a -> Game gs mv a
  RandomChoice :: NonEmpty a -> Game gs mv a
  PlayerChoice :: Player -> NonEmpty mv -> Game gs mv mv

instance Functor (Game gs mv) where
  fmap f g = Bind g (Pure . f)

instance Applicative (Game gs mv) where
  pure = Pure
  gf <*> ga = Bind ga (\a -> Bind gf (\f -> return $ f a))

instance Monad (Game gs mv) where
  x >>= f = Bind x f

{- Use these in case we want to switch to a GameMonad typeclass -}
changeState :: State gs a -> Game gs mv a
changeState = ChangeState

randomChoice :: NonEmpty a -> Game gs mv a
randomChoice = RandomChoice

playerChoice :: Player -> NonEmpty mv -> Game gs mv mv
playerChoice = PlayerChoice

note :: String -> Game gs mv a -> Game gs mv a
note = Note

-- TODO: Observation instead of game state
-- The last parameter is the "rest of the game" continuation
type PlayerImpl gs mv m r = NonEmpty mv -> gs -> (mv -> Game gs mv r) -> m mv

getRandomChoice :: MonadRandom m => NonEmpty a -> m a
getRandomChoice xs = do
  i <- getRandomR (0, NE.length xs - 1)
  return $ xs NE.!! i

data ContStack gs mv a r where
  Nil :: ContStack gs mv a a
  Cons :: forall gs mv a b r. (a -> Game gs mv b) -> ContStack gs mv b r -> ContStack gs mv a r

runCont :: ContStack gs mv a r -> a -> Game gs mv r
runCont Nil x = return x
runCont (Cons f fs) x = do
  v <- f x
  runCont fs v

stepGame ::
     MonadRandom m => (Player -> PlayerImpl gs mv m r) -> gs -> Game gs mv r -> m (Game gs mv r, gs)
stepGame _ gs (Pure r) = return (Pure r, gs)
stepGame _ gs (Bind (Pure r) f) = return (f r, gs)
stepGame _ gs (Bind (Bind x g) f) = return (Bind x (\r -> Bind (g r) f), gs)
stepGame _ gs (Bind (ChangeState u) f) =
  let (r, gs') = runState u gs
   in return (f r, gs')
stepGame _ gs (Bind (RandomChoice xs) f) = do
  r <- getRandomChoice xs
  return (f r, gs)
stepGame players gs (Bind (PlayerChoice p mvs) f) = do
  choice <- players p mvs gs f
  return (f choice, gs)
stepGame players gs (Bind (Note _ m) f) = stepGame players gs (Bind m f)
stepGame _ _ _ = undefined

instance Show mv => Show (Game gs mv r) where
  show (Note s m) = s ++ ": " ++ show m
  show (Pure _) = "return ?"
  show (Bind m _) = "(" ++ show m ++ " >>= ?)"
  show (PlayerChoice p mvs) = "player" ++ show p ++ "(" ++ show mvs ++ ")"
  show (RandomChoice _) = "random ?"
  show (ChangeState _) = "changestate ?"

smallstepRunGame ::
     MonadRandom m => (Player -> PlayerImpl gs mv m r) -> gs -> Game gs mv r -> m (r, gs)
smallstepRunGame _ gs (Pure x) = return (x, gs)
smallstepRunGame players gs g = do
  (g', gs') <- stepGame players gs g
  smallstepRunGame players gs' g'

bigstepRun ::
     MonadRandom m
  => (Player -> PlayerImpl gs mv m r)
  -> gs
  -> Game gs mv a
  -> ContStack gs mv a r
  -> m (a, gs)
bigstepRun _ gs (Pure x) _ = return (x, gs)
bigstepRun players gs (Bind x f) cont = do
  (x', gs') <- bigstepRun players gs x (Cons f cont)
  bigstepRun players gs' (f x') cont
bigstepRun _ gs (ChangeState u) _ = return $ runState u gs
bigstepRun _ gs (RandomChoice xs) _ = do
  x <- getRandomChoice xs
  return $ (x, gs)
bigstepRun players gs (PlayerChoice p c) cont = do
  choice <- players p c gs (runCont cont)
  return (choice, gs)
bigstepRun players gs (Note _ m) cont = bigstepRun players gs m cont

bigstepRunGame ::
     MonadRandom m => (Player -> PlayerImpl gs mv m a) -> gs -> Game gs mv a -> m (a, gs)
bigstepRunGame players gs m = bigstepRun players gs m Nil

getState :: (gs -> a) -> Game gs mv a
getState = changeState . gets

modifyState :: (gs -> gs) -> Game gs mv ()
modifyState = changeState . modify

universe :: (Bounded a, Enum a) => [a]
universe = [minBound .. maxBound]

{- stolen from Relude -}
inverseMap ::
     forall a k. (Bounded a, Enum a, Ord k)
  => (a -> k)
  -> (k -> Maybe a)
inverseMap f = \k -> Map.lookup k dict
  where
    dict :: Map.Map k a
    dict = Map.fromList $ zip (map f univ) univ
    univ :: [a]
    univ = universe

completeArray :: (Ix i, Bounded i) => [(i, e)] -> Array i e
completeArray = array (minBound, maxBound)
