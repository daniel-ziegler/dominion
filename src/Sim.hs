{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Sim where

import Flow

import Control.Monad
import Control.Monad.Loops
import Control.Monad.Random
import Control.Monad.Random.Class
import Control.Monad.State.Lazy
import Data.Array.IArray
import Data.Ix
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Debug.Trace
import System.Random.Shuffle

data CardType
  = Action
  | Treasure
  | Victory
  | CurseType
  | Attack
  | Reaction
  deriving (Eq, Ord, Show)

data Card
  = Copper
  | Silver
  | Gold
  -- Curse
  | Curse
  -- Victory
  | Estate
  | Duchy
  | Province
  -- Kingdom
  -- 2
  | Cellar
  | Chapel
  | Moat
  -- 3
  | Harbinger
  | Merchant
  | Vassal
  | Village
  | Workshop
  -- 4
  | Bureacrat
  | Gardens
  | Militia
  | Moneylender
  | Poacher
  | Remodel
  | Smithy
  | ThroneRoom
  -- 5
  | Bandit
  | CouncilRoom
  | Festival
  | Laboratory
  | Library
  | Market
  | Mine
  | Sentry
  | Witch
  | Artisan
  deriving (Eq, Ord, Show, Bounded, Enum)

cardBasePrice card =
  case card of
    Copper -> 0
    Silver -> 3
    Gold -> 6
    -- Curse
    Curse -> 0
    -- Victory
    Estate -> 2
    Duchy -> 5
    Province -> 8
    -- Kingdom
    -- 2
    Cellar -> 2
    Chapel -> 2
    Moat -> 2
    -- 3
    Harbinger -> 3
    Merchant -> 3
    Vassal -> 3
    Village -> 3
    Workshop -> 3
    -- 4
    Bureacrat -> 4
    Gardens -> 4
    Militia -> 4
    Moneylender -> 4
    Poacher -> 4
    Remodel -> 4
    Smithy -> 4
    ThroneRoom -> 4
    -- 5
    Bandit -> 5
    CouncilRoom -> 5
    Festival -> 5
    Laboratory -> 5
    Library -> 5
    Market -> 5
    Mine -> 5
    Sentry -> 5
    Witch -> 5
    Artisan -> 5

cardTypeList card =
  case card of
    Copper -> [Treasure]
    Silver -> [Treasure]
    Gold -> [Treasure]
    -- Curse
    Curse -> [CurseType]
    -- Victory
    Estate -> [Victory]
    Duchy -> [Victory]
    Province -> [Victory]
    -- Kingdom
    -- 2
    Cellar -> [Action]
    Chapel -> [Action]
    Moat -> [Action, Reaction]
    -- 3
    Harbinger -> [Action]
    Merchant -> [Action]
    Vassal -> [Action]
    Village -> [Action]
    Workshop -> [Action]
    -- 4
    Bureacrat -> [Action]
    Gardens -> [Victory]
    Militia -> [Action, Attack]
    Moneylender -> [Action]
    Poacher -> [Action]
    Remodel -> [Action]
    Smithy -> [Action]
    ThroneRoom -> [Action]
    -- 5
    Bandit -> [Action, Attack]
    CouncilRoom -> [Action]
    Festival -> [Action]
    Laboratory -> [Action]
    Library -> [Action]
    Market -> [Action]
    Mine -> [Action]
    Sentry -> [Action]
    Witch -> [Action, Attack]
    Artisan -> [Action]

cardHasType ty card = ty `elem` cardTypeList card

data SupplyPile
  = CopperPile
  | SilverPile
  | GoldPile
  | EstatePile
  | DuchyPile
  | ProvincePile
  | CursePile
  -- 2
  | CellarPile
  | ChapelPile
  | MoatPile
  -- 3
  | HarbingerPile
  | MerchantPile
  | VassalPile
  | VillagePile
  | WorkshopPile
  -- 4
  | BureacratPile
  | GardensPile
  | MilitiaPile
  | MoneylenderPile
  | PoacherPile
  | RemodelPile
  | SmithyPile
  | ThroneRoomPile
  -- 5
  | BanditPile
  | CouncilRoomPile
  | FestivalPile
  | LaboratoryPile
  | LibraryPile
  | MarketPile
  | MinePile
  | SentryPile
  | WitchPile
  | ArtisanPile
  deriving (Eq, Ord, Show)

originPile :: Card -> SupplyPile
originPile card =
  case card of
    Copper -> CopperPile
    Silver -> SilverPile
    Gold -> GoldPile
    -- Curse
    Curse -> CursePile
    -- Victory
    Estate -> EstatePile
    Duchy -> DuchyPile
    Province -> ProvincePile
    -- Kingdom
    -- 2
    Cellar -> CellarPile
    Chapel -> ChapelPile
    Moat -> MoatPile
    -- 3
    Harbinger -> HarbingerPile
    Merchant -> MerchantPile
    Vassal -> VassalPile
    Village -> VillagePile
    Workshop -> WorkshopPile
    -- 4
    Bureacrat -> BureacratPile
    Gardens -> GardensPile
    Militia -> MilitiaPile
    Moneylender -> MoneylenderPile
    Poacher -> PoacherPile
    Remodel -> RemodelPile
    Smithy -> SmithyPile
    ThroneRoom -> ThroneRoomPile
    -- 5
    Bandit -> BanditPile
    CouncilRoom -> CouncilRoomPile
    Festival -> FestivalPile
    Laboratory -> LaboratoryPile
    Library -> LibraryPile
    Market -> MarketPile
    Mine -> MinePile
    Sentry -> SentryPile
    Witch -> WitchPile
    Artisan -> ArtisanPile

pileCard = inverseMap originPile .> fromJust

victoryCardCount :: Int -> Int
victoryCardCount nPlayers
  | nPlayers <= 2 = 8
  | otherwise = 12

initCount :: SupplyPile -> Int -> Int
initCount EstatePile = victoryCardCount
initCount DuchyPile = victoryCardCount
initCount ProvincePile = victoryCardCount
initCount CopperPile = \nPlayers -> 60 - 7 * nPlayers
initCount SilverPile = const 40
initCount GoldPile = const 30
initCount _ = const 10

initPile :: Int -> SupplyPile -> [Card]
initPile nPlayers pile = replicate (initCount pile nPlayers) (pileCard pile)

newtype Player =
  Player Int
  deriving (Eq, Ord, Show, Ix)

playerIndex (Player i) = i

data PlayerPile
  = Deck
  | Hand
  | InPlay
  | Discard
  | SetAside -- TODO: different kinds of set aside zones?
  deriving (Eq, Ord, Show, Bounded, Enum, Ix)

data Pile
  = PlayerPile Player PlayerPile
  | Supply SupplyPile
  | Trash
  deriving (Eq, Ord, Show)

data Counter
  = Actions
  | Coins
  | Buys
  deriving (Eq, Ord, Show, Bounded, Ix)

data Mechanic
  = Nop
  | Buy SupplyPile
  | Gain PlayerPile Pile
  | DiscardCard Card
  | Play Card
  -- etc

data PlayerPrompt a where
  PickMechanic :: NonEmpty Mechanic -> PlayerPrompt Mechanic

data GameState =
  GameState
    { playerPiles :: Array Player (Array PlayerPile [Card])
    , supply :: Map.Map SupplyPile [Card]
    , trash :: [Card]
    , turn :: Player
    , counters :: Array Counter Int
    }
  deriving (Show)

nPlayers (GameState {playerPiles}) = 1 + (playerIndex $ snd $ bounds playerPiles)

type StateUpdate = State GameState

data Game a where
  Pure :: a -> Game a
  Bind :: Game a -> (a -> Game b) -> Game b
  ChangeState :: StateUpdate a -> Game a
  RandomChoice :: NonEmpty a -> Game a
  PlayerChoice :: Player -> PlayerPrompt a -> Game a

instance Functor Game where
  fmap f g = Bind g (Pure . f)

instance Applicative Game where
  pure = Pure
  gf <*> ga = Bind ga (\a -> Bind gf (\f -> return $ f a))

instance Monad Game where
  x >>= f = Bind x f

{- Use these in case we want to switch to a GameMonad typeclass -}
changeState = ChangeState

randomChoice = RandomChoice

playerChoice = PlayerChoice

initCounters :: Array Counter Int
initCounters = completeArray [(Actions, 1), (Coins, 0), (Buys, 1)]

initState ::
     forall g. RandomGen g
  => Int
  -> Rand g GameState
initState nPlayers = do
  allPlayerPiles <- replicateM nPlayers playerPiles
  let playerPiles =
        array (Player 0, Player $ nPlayers - 1) $
        zip ([0 .. nPlayers - 1] |> map Player) allPlayerPiles
  let supplyPiles =
        Map.fromSet (initPile nPlayers) $
        Set.fromList
          [ CopperPile
          , SilverPile
          , GoldPile
          , EstatePile
          , DuchyPile
          , ProvincePile
          , CursePile
          , SmithyPile
          ]
  return $
    GameState
      { playerPiles = playerPiles
      , supply = supplyPiles
      , trash = []
      , turn = Player 0
      , counters = initCounters
      }
  where
    playerPiles :: Rand g (Array PlayerPile [Card])
    playerPiles = do
      cards <- shuffleM $ replicate 3 Estate ++ replicate 7 Copper
      let (hand, deck) = splitAt 5 cards
      let allEmpty = completeArray [(pz, []) | pz <- [minBound .. maxBound]]
      return $ allEmpty // [(Hand, hand), (Deck, deck)]

type PlayerImpl = forall a. PlayerPrompt a -> a

getRandomChoice :: RandomGen g => NonEmpty a -> Rand g a
getRandomChoice xs = do
  i <- getRandomR (0, NE.length xs - 1)
  return $ xs NE.!! i

run :: RandomGen g => (Player -> PlayerImpl) -> GameState -> Game a -> Rand g (a, GameState)
run players gs (Pure x) = return (x, gs)
run players gs (Bind x f) = do
  (x', gs') <- run players gs x
  run players gs' (f x')
run players gs (ChangeState u) = return $ runState u gs
run players gs (RandomChoice xs) = do
  x <- getRandomChoice xs
  return $ (x, gs)
run players gs (PlayerChoice p c) = return (players p $ c, gs)

dummyPlayer :: PlayerImpl
dummyPlayer (PickMechanic ms) = NE.last ms

getState :: (GameState -> a) -> Game a
getState = changeState . gets

modifyState :: (GameState -> GameState) -> Game ()
modifyState = changeState . modify

computeCardPrice :: Card -> Game Int
computeCardPrice = return . cardBasePrice

computeCardScore :: Card -> Game Int
computeCardScore Curse = return (-1)
computeCardScore Estate = return 1
computeCardScore Duchy = return 3
computeCardScore Province = return 5
computeCardScore _ = return 0

adjustCounter :: Counter -> Int -> Game ()
adjustCounter ctr delta =
  modifyState
    (\gs ->
       let newval = counters gs ! ctr + delta
        in if newval < 0
             then error $ "underflowed " ++ show ctr
             else gs {counters = counters gs // [(ctr, newval)]})

gameShuffle :: [a] -> Game [a]
gameShuffle xs = do
  seq <- rseq (length xs - 1)
  return $ shuffle xs seq
  where
    rseq :: Int -> Game [Int]
    rseq 0 = return []
    rseq i = do
      first <- randomChoice (0 :| [1 .. i])
      rest <- rseq (i - 1)
      return (first : rest)

getPile :: Pile -> Game [Card]
getPile pile = getState $ getPile' pile
  where
    getPile' (PlayerPile player playerPile) gs = playerPiles gs ! player ! playerPile
    getPile' (Supply supplyPile) gs = supply gs Map.! supplyPile
    getPile' Trash gs = trash gs

setPile :: Pile -> [Card] -> Game ()
setPile pile cards = modifyState $ setPile' pile
  where
    setPile' (PlayerPile player playerPile) gs =
      let forPlayer = playerPiles gs ! player
          forPlayer' = forPlayer // [(playerPile, cards)]
       in gs {playerPiles = playerPiles gs // [(player, forPlayer')]}
    setPile' (Supply supplyPile) gs = gs {supply = Map.insert supplyPile cards $ supply gs}
    setPile' Trash gs = gs {trash = cards}

modifyPile :: Pile -> ([Card] -> [Card]) -> Game ()
modifyPile pile f = do
  cards <- getPile pile
  setPile pile (f cards)

movePile :: Pile -> Pile -> Game ()
movePile from to = do
  cards <- getPile from
  setPile from []
  modifyPile to (cards ++)

moveTopCard :: Pile -> Pile -> Game Bool
moveTopCard from to = do
  cards <- getPile from
  case cards of
    c:cards' -> do
      modifyPile from tail
      modifyPile to (c :)
      return True
    [] -> return False

moveCard :: Card -> Pile -> Pile -> Game Bool
moveCard card from to = do
  cards <- getPile from
  if card `elem` cards
    then do
      modifyPile from (delete card)
      modifyPile to (card :)
      return True
    else return False

shufflePile :: Pile -> Game ()
shufflePile pile = do
  cards <- getPile pile
  shuffled <- gameShuffle cards
  setPile pile shuffled

draw :: Game Bool
draw = do
  player <- getState turn
  deck <- getPile (PlayerPile player Deck)
  when (null deck) $
    shufflePile (PlayerPile player Discard) >>
    movePile (PlayerPile player Discard) (PlayerPile player Deck)
  if (null deck)
    then return False
    else do
      moveTopCard (PlayerPile player Deck) (PlayerPile player Hand)
      return True

actionPhase :: Game ()
actionPhase = do
  player <- getState turn
  nActions <- getState ((! Actions) . counters)
  if nActions == 0
    then return ()
    else do
      hand <- getPile (PlayerPile player Hand)
      choice <-
        playerChoice player (PickMechanic $ Nop :| (map Play $ filter (cardHasType Action) hand))
      didSomething <- doMechanic choice
      when didSomething actionPhase

playTreasuresPhase :: Game ()
playTreasuresPhase = do
  player <- getState turn
  hand <- getPile (PlayerPile player Hand)
  choice <-
    playerChoice player (PickMechanic $ Nop :| (map Play $ filter (cardHasType Treasure) hand))
  case choice of
    Nop -> return ()
    Play card -> do
      moveCard card (PlayerPile player Hand) (PlayerPile player InPlay)
      playEffect card
      playTreasuresPhase

pilePrice :: SupplyPile -> Game (Maybe Int)
pilePrice pile = do
  cards <- getState (\gs -> supply gs Map.! pile)
  case cards of
    [] -> return Nothing
    (c:cards') -> Just <$> computeCardPrice c

priceFilteredSupplyPiles :: (Int -> Bool) -> Game [SupplyPile]
priceFilteredSupplyPiles goodPrice = do
  getState (supply .> Map.keys) >>= filterM (\p -> any goodPrice <$> pilePrice p)

buyPhase :: Game ()
buyPhase = do
  player <- getState turn
  nBuys <- getState ((! Buys) . counters)
  if nBuys == 0
    then return ()
    else do
      coins <- getState (\gs -> counters gs ! Coins)
      affordablePiles <- priceFilteredSupplyPiles (<= coins)
      choice <- playerChoice player (PickMechanic $ Nop :| map Buy affordablePiles)
      boughtSomething <- doMechanic choice
      when boughtSomething buyPhase

cleanupPhase :: Game ()
cleanupPhase = do
  player <- getState turn
  movePile (PlayerPile player InPlay) (PlayerPile player Discard)
  movePile (PlayerPile player Hand) (PlayerPile player Discard)
  replicateM_ 5 draw

nextPlayer :: Int -> Player -> Player
nextPlayer nPlayers (Player n) = Player ((n + 1) `mod` nPlayers)

advanceTurn :: Game ()
advanceTurn = do
  modifyState (\gs -> gs {counters = initCounters, turn = nextPlayer (nPlayers gs) (turn gs)})

doTurn :: Game ()
doTurn = do
  actionPhase
  playTreasuresPhase
  buyPhase
  cleanupPhase
  advanceTurn

doMechanic :: Mechanic -> Game Bool
doMechanic Nop = return False
doMechanic (Buy supplyPile) = do
  price <- fromJust <$> pilePrice supplyPile
  adjustCounter Coins (-price)
  adjustCounter Buys (-1)
  doMechanic $ Gain Hand (Supply supplyPile)
doMechanic (Gain to from) = do
  player <- getState turn
  moveTopCard from (PlayerPile player to)
doMechanic (Play card) = do
  player <- getState turn
  moveCard card (PlayerPile player Hand) (PlayerPile player InPlay)
  adjustCounter Actions (-1)
  playEffect card
  return True

playEffect card = do
  player <- getState turn
  case card of
    Copper -> adjustCounter Coins 1
    Silver -> adjustCounter Coins 2
    Gold -> adjustCounter Coins 3
    {- Artisan -> do
      gainOptions <- priceFilteredSupplyPiles (\p -> 0 <= p && p <= 5)
      choice <- playerChoice player (PickMechanic $ Nop :| (map (Gain Hand) gainOptions))
      case choice of -}
    Smithy -> replicateM_ 3 draw
    c -> error $ "haven't implemented " ++ show c

isGameOver :: Game Bool
isGameOver = do
  supplyPiles <- getState supply
  let emptyPiles = Map.filter null supplyPiles
  return $ Map.member ProvincePile emptyPiles || Map.size emptyPiles >= 3

playerScore :: Player -> Game Int
playerScore p = do
  piles <- getState (\gs -> playerPiles gs ! p)
  let allCards = concat $ elems piles
  cardScores <- mapM computeCardScore allCards
  return $ sum cardScores

game :: Game [Int]
game = do
  whileM_ (not <$> isGameOver) doTurn
  n <- getState nPlayers
  map Player [0 .. n - 1] |> mapM playerScore

dummyRun st game = evalRandIO $ run (const dummyPlayer) st game

universe :: (Bounded a, Enum a) => [a]
universe = [minBound .. maxBound]

counts :: (Ord a) => [a] -> Map.Map a Int
counts = (`zip` (repeat 1)) .> Map.fromListWith (+)

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
