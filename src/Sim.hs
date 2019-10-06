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
  deriving (Eq, Ord, Show)

data PlayerPile
  = Deck
  | Hand
  | InPlay
  | Discard
  | SetAside -- TODO: different kinds of set aside zones?
  deriving (Eq, Ord, Show, Bounded, Ix)

data BoardPile
  = Supply SupplyPile
  | Trash
  deriving (Eq, Ord, Show, Bounded, Ix)

data Counter
  = Actions
  | Coins
  | Buys
  deriving (Eq, Ord, Show, Bounded, Ix)

data Mechanic
  = StartTurn Player
  | Nop
  | Buy SupplyPile
  | Gain Card PlayerPile
  | DiscardCard Card
  | Play Card
  -- etc

data PlayerPrompt a where
  PickMechanic :: NonEmpty Mechanic -> PlayerPrompt Mechanic

data GameState =
  GameState
    { nPlayers :: Int
    , playerPiles :: Array Int (Array PlayerPile [Card])
    , boardPiles :: Array BoardPile
    , turn :: Player
    , counters :: Array Counter Int
    }
  deriving (Show)

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
  allPlayerCards <-
    mapM (\p -> (Map.mapKeys (OfPlayer $ Player p)) <$> playerCards) [0 .. nPlayers - 1]
  let supplyPiles =
        Map.mapKeys Supply $
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
      { nPlayers = nPlayers
      , zones = Map.insert Trash [] $ Map.unions (supplyPiles : allPlayerCards)
      , turn = Player 0
      , counters = initCounters
      }
  where
    playerCards :: Rand g (Array PlayerPile [Card])
    playerCards = do
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

shufflePile :: Pile -> Game ()
shufflePile zone = do
  cards <- getState (\gs -> zones gs Map.! zone)
  shuffled <- gameShuffle cards
  modifyState (\gs -> gs {zones = Map.insert zone shuffled $ zones gs})

draw :: Game Bool
draw = do
  player <- getState turn
  deck <- getState (\gs -> zones gs Map.! (OfPlayer player Deck))
  when (null deck) $
    shufflePile (OfPlayer player Discard) >>
    movePile (OfPlayer player Discard) (OfPlayer player Deck)
  case deck of
    [] -> return False
    (card:deck') -> do
      moveCard card (OfPlayer player Deck) (OfPlayer player Hand)
      return True

playEffect :: Card -> Game ()
playEffect card =
  case card of
    Smithy -> replicateM_ 3 draw
    Copper -> adjustCounter Coins 1
    Silver -> adjustCounter Coins 2
    Gold -> adjustCounter Coins 3
    c -> error $ "haven't implemented " ++ show c

actionPhase :: Game ()
actionPhase = do
  player <- getState turn
  nActions <- getState ((! Actions) . counters)
  if nActions == 0
    then return ()
    else do
      hand <- getState (\gs -> zones gs Map.! (OfPlayer player Hand))
      choice <-
        playerChoice player (PickMechanic $ Nop :| (map Play $ filter (cardHasType Action) hand))
      case choice of
        Nop -> return ()
        Play card -> do
          moveCard card (OfPlayer player Hand) (OfPlayer player InPlay)
          adjustCounter Actions (-1)
          playEffect card
          actionPhase

playTreasuresPhase :: Game ()
playTreasuresPhase = do
  player <- getState turn
  hand <- getState (\gs -> zones gs Map.! (OfPlayer player Hand))
  choice <-
    playerChoice player (PickMechanic $ Nop :| (map Play $ filter (cardHasType Treasure) hand))
  case choice of
    Nop -> return ()
    Play card -> do
      moveCard card (OfPlayer player Hand) (OfPlayer player InPlay)
      playEffect card
      playTreasuresPhase

gainFrom :: Pile -> Game ()
gainFrom from = do
  player <- getState turn
  cards <- getState (\gs -> zones gs Map.! from)
  moveCard (head cards) from (OfPlayer player Discard)

buyPhase :: Game ()
buyPhase = do
  player <- getState turn
  nActions <- getState ((! Buys) . counters)
  if nActions == 0
    then return ()
    else do
      coins <- getState (\gs -> counters gs ! Coins)
      -- this makes me want to make zones better
      let pileWithTop (Supply p, c:cs) = Just (p, c)
          pileWithTop _ = Nothing
      pilesWithTops <- getState (zones .> Map.assocs .> mapMaybe pileWithTop)
      prices <- mapM (snd .> computeCardPrice) pilesWithTops
      let pilesWithPrices = zip (map fst pilesWithTops) prices
      let affordablePiles = (pilesWithPrices |> filter (snd .> (<= coins)) |> map fst)
      choice <- playerChoice player (PickMechanic $ Nop :| map Buy affordablePiles)
      case choice of
        Nop -> return ()
        Buy supplyPile -> do
          let price = fromJust $ lookup supplyPile pilesWithPrices
          adjustCounter Coins (-price)
          adjustCounter Buys (-1)
          gainFrom (Supply supplyPile)
          buyPhase

cleanupPhase :: Game ()
cleanupPhase = do
  player <- getState turn
  movePile (OfPlayer player InPlay) (OfPlayer player Discard)
  movePile (OfPlayer player Hand) (OfPlayer player Discard)
  replicateM_ 5 draw

nextPlayer :: Int -> Player -> Player
nextPlayer nPlayers (Player n) = Player ((n + 1) `mod` nPlayers)

advanceTurn :: Game ()
advanceTurn = do
  modifyState (\gs -> gs {counters = initCounters, turn = nextPlayer (nPlayers gs) (turn gs)})

simTurn :: Game ()
simTurn = do
  actionPhase
  playTreasuresPhase
  buyPhase
  cleanupPhase
  advanceTurn

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
