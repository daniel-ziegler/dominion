{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dominion where

import Flow

import Control.Monad
import Control.Monad.Loops
import Control.Monad.Random hiding (fromList)
import Control.Monad.State.Lazy
import Data.Array.IArray
import Data.List
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import System.Random.Shuffle

import Game

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

cardBasePrice :: Card -> Int
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

cardTypeList :: Card -> [CardType]
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

cardHasType :: CardType -> Card -> Bool
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

pileCard :: SupplyPile -> Card
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

playerIndex :: Player -> Int
playerIndex (Player i) = i

data PlayerPile
  = Deck
  | Hand
  | InPlay
  | Discard
  | SetAside
  | Selecting
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
  | Play Player Card
  | Buy Player SupplyPile
  -- TODO switch to CardInstance so this is less stupid
  | Draw Player
  | Gain Player Pile PlayerPile
  | DiscardFrom Player PlayerPile Card
  | TopdeckFrom Player PlayerPile Card
  | TrashFrom Player PlayerPile Card
  -- etc
  deriving (Show)

data DGameState =
  DGameState
    { playerPiles :: Array Player (Array PlayerPile [Card])
    , supply :: Map.Map SupplyPile [Card]
    , trash :: [Card]
    , turn :: Player
    , counters :: Array Counter Int
    }
  deriving (Show)

type DGame a = Game DGameState Mechanic a

type DPlayerImpl m r = PlayerImpl DGameState Mechanic m r

gameNPlayers :: DGameState -> Int
gameNPlayers (DGameState {playerPiles}) = 1 + (playerIndex $ snd $ bounds playerPiles)

initCounters :: Array Counter Int
initCounters = completeArray [(Actions, 1), (Coins, 0), (Buys, 1)]

initState ::
     forall m. MonadRandom m
  => Int
  -> m DGameState
initState nPlayers = do
  allPlayerPiles <- replicateM nPlayers playerPiles
  let playerPs =
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
          , ArtisanPile
          , BanditPile
          , CouncilRoomPile
          , SentryPile
          , SmithyPile
          ]
  return $
    DGameState
      { playerPiles = playerPs
      , supply = supplyPiles
      , trash = []
      , turn = Player 0
      , counters = initCounters
      }
  where
    playerPiles :: m (Array PlayerPile [Card])
    playerPiles = do
      cards <- shuffleM $ replicate 3 Estate ++ replicate 7 Copper
      let (hand, deck) = splitAt 5 cards
      let allEmpty = completeArray [(pz, []) | pz <- [minBound .. maxBound]]
      return $ allEmpty // [(Hand, hand), (Deck, deck)]

computeCardPrice :: Card -> DGame Int
computeCardPrice = return . cardBasePrice

computeCardScore :: Card -> DGame Int
computeCardScore Curse = return (-1)
computeCardScore Estate = return 1
computeCardScore Duchy = return 3
computeCardScore Province = return 5
computeCardScore _ = return 0

adjustCounter :: Counter -> Int -> DGame ()
adjustCounter ctr delta =
  modifyState
    (\gs ->
       let newval = counters gs ! ctr + delta
        in if newval < 0
             then error $ "underflowed " ++ show ctr
             else gs {counters = counters gs // [(ctr, newval)]})

gameShuffle :: [a] -> DGame [a]
gameShuffle [] = return []
gameShuffle xs = do
  rs <- rseq (length xs - 1)
  return $ shuffle xs rs
  where
    rseq :: Int -> DGame [Int]
    rseq 0 = return []
    rseq i =
      Note ("rseq " ++ show i) $ do
        first <- randomChoice (0 :| [1 .. i])
        rest <- rseq (i - 1)
        return (first : rest)

getPile :: Pile -> DGame [Card]
getPile pile = getState $ getPile' pile
  where
    getPile' (PlayerPile player playerPile) gs = playerPiles gs ! player ! playerPile
    getPile' (Supply supplyPile) gs = supply gs Map.! supplyPile
    getPile' Trash gs = trash gs

setPile :: Pile -> [Card] -> DGame ()
setPile pile cards = modifyState $ setPile' pile
  where
    setPile' (PlayerPile player playerPile) gs =
      let forPlayer = playerPiles gs ! player
          forPlayer' = forPlayer // [(playerPile, cards)]
       in gs {playerPiles = playerPiles gs // [(player, forPlayer')]}
    setPile' (Supply supplyPile) gs = gs {supply = Map.insert supplyPile cards $ supply gs}
    setPile' Trash gs = gs {trash = cards}

modifyPile :: Pile -> ([Card] -> [Card]) -> DGame ()
modifyPile pile f = do
  cards <- getPile pile
  setPile pile (f cards)

movePile :: Pile -> Pile -> DGame ()
movePile from to = do
  cards <- getPile from
  setPile from []
  modifyPile to (cards ++)

-- WARNING: won't trigger reshuffle when you take off a player's deck.
-- Use moveFromDeckTop instead for that.
moveTopCards :: Int -> Pile -> Pile -> DGame Int
moveTopCards n from to = do
  cards <- take n <$> getPile from
  modifyPile from (drop n)
  modifyPile to (cards ++)
  return $ length cards

moveTopCard :: Pile -> Pile -> DGame Bool
moveTopCard from to = do
  n <- moveTopCards 1 from to
  return $ n > 0

moveFromDeckTop :: Int -> Player -> Pile -> DGame Int
moveFromDeckTop n fromPlayer to = do
  taken <- moveTopCards n (PlayerPile fromPlayer Deck) to
  if taken == n
    then return taken
    else do
      reshuffle fromPlayer
      moreTaken <- moveTopCards (n - taken) (PlayerPile fromPlayer Deck) to
      return $ taken + moreTaken

moveCard :: Card -> Pile -> Pile -> DGame Bool
moveCard card from to = do
  cards <- getPile from
  if card `elem` cards
    then do
      modifyPile from (delete card)
      modifyPile to (card :)
      return True
    else return False

shufflePile :: Pile -> DGame ()
shufflePile pile = do
  cards <- getPile pile
  shuffled <- gameShuffle cards
  setPile pile shuffled

actionPhase :: DGame ()
actionPhase = do
  you <- getState turn
  nActions <- getState ((! Actions) . counters)
  if nActions == 0
    then return ()
    else do
      hand <- getPile (PlayerPile you Hand)
      choice <- playerChoice you (Nop :| (map (Play you) $ filter (cardHasType Action) hand))
      didSomething <- doMechanic choice
      when didSomething actionPhase

playTreasuresPhase :: DGame ()
playTreasuresPhase = do
  you <- getState turn
  hand <- getPile (PlayerPile you Hand)
  choice <- playerChoice you (Nop :| (map (Play you) $ filter (cardHasType Treasure) hand))
  case choice of
    Nop -> return ()
    Play _ card -> do
      void $ moveCard card (PlayerPile you Hand) (PlayerPile you InPlay)
      playEffect card
      playTreasuresPhase
    _ -> error "how'd you pick that?"

pilePrice :: SupplyPile -> DGame (Maybe Int)
pilePrice pile = do
  cards <- getState (\gs -> supply gs Map.! pile)
  case cards of
    [] -> return Nothing
    (c:_) -> Just <$> computeCardPrice c

priceFilteredSupplyPiles :: (Int -> Bool) -> DGame [SupplyPile]
priceFilteredSupplyPiles goodPrice = do
  getState (supply .> Map.keys) >>= filterM (\p -> any goodPrice <$> pilePrice p)

buyPhase :: DGame ()
buyPhase = do
  you <- getState turn
  nBuys <- getState ((! Buys) . counters)
  if nBuys == 0
    then return ()
    else do
      coins <- getState (\gs -> counters gs ! Coins)
      affordablePiles <- priceFilteredSupplyPiles (<= coins)
      choice <- playerChoice you (Nop :| map (Buy you) affordablePiles)
      boughtSomething <- doMechanic choice
      when boughtSomething buyPhase

cleanupPhase :: DGame ()
cleanupPhase = do
  you <- getState turn
  movePile (PlayerPile you InPlay) (PlayerPile you Discard)
  movePile (PlayerPile you Hand) (PlayerPile you Discard)
  replicateM_ 5 $ doMechanic (Draw you)

nextPlayer :: Int -> Player -> Player
nextPlayer nPlayers (Player n) = Player ((n + 1) `mod` nPlayers)

advanceTurn :: DGame ()
advanceTurn = do
  modifyState (\gs -> gs {counters = initCounters, turn = nextPlayer (gameNPlayers gs) (turn gs)})

doTurn :: DGame ()
doTurn = do
  actionPhase
  playTreasuresPhase
  buyPhase
  cleanupPhase
  advanceTurn

reshuffle :: Player -> DGame ()
reshuffle player = do
  shufflePile (PlayerPile player Discard)
  movePile (PlayerPile player Discard) (PlayerPile player Deck)

doMechanic :: Mechanic -> DGame Bool
doMechanic Nop = return False
doMechanic (Buy player supplyPile) = do
  price <- fromJust <$> pilePrice supplyPile
  adjustCounter Coins (-price)
  adjustCounter Buys (-1)
  doMechanic $ Gain player (Supply supplyPile) Hand
doMechanic (Gain player from to) = do
  moveTopCard from (PlayerPile player to)
doMechanic (Draw player) = do
  (> 0) <$> moveFromDeckTop 1 player (PlayerPile player Hand)
doMechanic (Play player card) = do
  moved <- moveCard card (PlayerPile player Hand) (PlayerPile player InPlay)
  if not moved
    then error ("no " ++ show card ++ " in hand to play")
    else do
      adjustCounter Actions (-1)
      playEffect card
      return True
doMechanic (DiscardFrom player from card) = do
  moveCard card (PlayerPile player from) (PlayerPile player Discard)
doMechanic (TopdeckFrom player from card) = do
  moveCard card (PlayerPile player from) (PlayerPile player Deck)
doMechanic (TrashFrom player from card) = do
  moveCard card (PlayerPile player from) Trash

allPlayers :: DGame [Player]
allPlayers = do
  n <- getState gameNPlayers
  return $ map Player [0 .. n - 1]

allOtherPlayersDo :: (Player -> DGame ()) -> DGame ()
allOtherPlayersDo f = do
  you <- getState turn
  players <- allPlayers
  filter (/= you) players |> mapM_ f

pickMechanicUntilQuit :: Player -> DGame [Mechanic] -> DGame ()
pickMechanicUntilQuit player getChoices = do
  choices <- getChoices
  choice <- playerChoice player (Nop :| choices)
  case choice of
    Nop -> return ()
    _ -> doMechanic choice >> pickMechanicUntilQuit player getChoices

pickMechanicUntilEmpty :: Player -> DGame [Mechanic] -> DGame ()
pickMechanicUntilEmpty player getChoices = do
  maybeChoices <- nonEmpty <$> getChoices
  case maybeChoices of
    Nothing -> return ()
    Just choices -> do
      choice <- playerChoice player choices
      doMechanic choice >> pickMechanicUntilEmpty player getChoices

pickMechanicUnlessEmpty :: Player -> [Mechanic] -> DGame Bool
pickMechanicUnlessEmpty player maybeChoices = do
  case nonEmpty maybeChoices of
    Nothing -> return False
    Just choices -> do
      choice <- playerChoice player choices
      doMechanic choice

playEffect :: Card -> DGame ()
playEffect card = do
  you <- getState turn
  case card of
    Copper -> adjustCounter Coins 1
    Silver -> adjustCounter Coins 2
    Gold -> adjustCounter Coins 3
    Artisan -> do
      gainOptions <- nonEmpty <$> priceFilteredSupplyPiles (\p -> 0 <= p && p <= 5)
      case gainOptions of
        Nothing -> return ()
        Just gainOptions -> do
          choice <- playerChoice you (NE.map (\p -> Gain you (Supply p) Hand) gainOptions)
          void $ doMechanic choice
      topdeckOptions <- nonEmpty <$> getPile (PlayerPile you Hand)
      case topdeckOptions of
        Nothing -> return ()
        Just topdeckOptions -> do
          choice <- playerChoice you (NE.map (\c -> TopdeckFrom you Hand c) topdeckOptions)
          void $ doMechanic choice
    Bandit -> do
      void $ doMechanic (Gain you (Supply GoldPile) Discard)
      allOtherPlayersDo $ \otherPlayer -> do
        void $ moveFromDeckTop 2 otherPlayer (PlayerPile otherPlayer Selecting)
        cards <- getPile (PlayerPile otherPlayer Selecting)
        let choices = cards |> filter (\c -> cardHasType Treasure c && c /= Copper)
        void $ pickMechanicUnlessEmpty otherPlayer $ map (TrashFrom otherPlayer Selecting) choices
        movePile (PlayerPile otherPlayer Selecting) (PlayerPile otherPlayer Discard)
    CouncilRoom -> do
      replicateM_ 4 $ doMechanic (Draw you)
      adjustCounter Buys 1
      allOtherPlayersDo $ void . doMechanic . Draw
    Sentry -> do
      void $ doMechanic (Draw you)
      adjustCounter Actions 1
      void $ moveFromDeckTop 2 you (PlayerPile you Selecting)
      pickMechanicUntilQuit you $
        map (TrashFrom you Selecting) <$> getPile (PlayerPile you Selecting)
      pickMechanicUntilQuit you $
        map (DiscardFrom you Selecting) <$> getPile (PlayerPile you Selecting)
      pickMechanicUntilEmpty you $
        map (TopdeckFrom you Selecting) <$> getPile (PlayerPile you Selecting)
    Smithy -> replicateM_ 3 $ doMechanic (Draw you)
    c -> error $ "haven't implemented " ++ show c

isDGameOver :: DGame Bool
isDGameOver = do
  supplyPiles <- getState supply
  let emptyPiles = Map.filter null supplyPiles
  return $ Map.member ProvincePile emptyPiles || Map.size emptyPiles >= 3

playerScore :: Player -> DGame Int
playerScore p = do
  piles <- getState (\gs -> playerPiles gs ! p)
  let allCards = concat $ elems piles
  cardScores <- mapM computeCardScore allCards
  return $ sum cardScores

game :: DGame [Int]
game = do
  whileM_ (not <$> isDGameOver) doTurn
  allPlayers >>= mapM playerScore
