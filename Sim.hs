{-# LANGUAGE RankNTypes, GADTs #-}
module Sim where

import Control.Monad.State.Lazy
import Control.Monad.Random
import Control.Monad.Random.Class
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as Map

data CardType =
    Action
  | Treasure
  | Victory
  | CurseType
  | Attack
  | Reaction
  deriving (Eq, Ord, Show)

data Card =
  -- Treasure
    Copper
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
  deriving (Eq, Ord, Show)

data CardInstance = CardInstance { card :: Card, id :: Int }

cardBasePrice card = case card of
  -- Treasure
  Copper -> 0
  Silver -> 3
  Gold   -> 6
  -- Curse
  Curse -> 0
  -- Victory
  Estate   -> 2
  Duchy    -> 5
  Province -> 8
  -- Kingdom
  -- 2
  Cellar -> 2
  Chapel -> 2
  Moat   -> 2
  -- 3
  Harbinger -> 3
  Merchant  -> 3
  Vassal    -> 3
  Village   -> 3
  Workshop  -> 3
  -- 4
  Bureacrat   -> 4
  Gardens     -> 4
  Militia     -> 4
  Moneylender -> 4
  Poacher     -> 4
  Remodel     -> 4
  Smithy      -> 4
  ThroneRoom  -> 4
  -- 5
  Bandit      -> 5
  CouncilRoom -> 5
  Festival    -> 5
  Laboratory  -> 5
  Library     -> 5
  Market      -> 5
  Mine        -> 5
  Sentry      -> 5
  Witch       -> 5
  Artisan     -> 5

cardTypeList card = case card of
  -- Treasure
  Copper -> [Treasure]
  Silver -> [Treasure]
  Gold   -> [Treasure]
  -- Curse
  Curse -> [CurseType]
  -- Victory
  Estate   -> [Victory]
  Duchy    -> [Victory]
  Province -> [Victory]
  -- Kingdom
  -- 2
  Cellar -> [Action]
  Chapel -> [Action]
  Moat   -> [Action, Reaction]
  -- 3
  Harbinger -> [Action]
  Merchant  -> [Action]
  Vassal    -> [Action]
  Village   -> [Action]
  Workshop  -> [Action]
  -- 4
  Bureacrat   -> [Action]
  Gardens     -> [Victory]
  Militia     -> [Action, Attack]
  Moneylender -> [Action]
  Poacher     -> [Action]
  Remodel     -> [Action]
  Smithy      -> [Action]
  ThroneRoom  -> [Action]
  -- 5
  Bandit      -> [Action, Attack]
  CouncilRoom -> [Action]
  Festival    -> [Action]
  Laboratory  -> [Action]
  Library     -> [Action]
  Market      -> [Action]
  Mine        -> [Action]
  Sentry      -> [Action]
  Witch       -> [Action, Attack]
  Artisan     -> [Action]

cardHasType ty card = ty `elem` cardTypeList card

data BasicSupplyPile =
    CopperPile
  | SilverPile
  | GoldPile
  | EstatePile
  | DuchyPile
  | ProvincePile
  | CursePile
  deriving (Eq, Ord, Show)
                    
data KingdomSupplyPile =
  -- 2
    CellarPile
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

data SupplyPile = Basic BasicSupplyPile | Kingdom KingdomSupplyPile
  deriving (Eq, Ord, Show)

originPile :: Card -> SupplyPile
originPile card = case card of
  -- Treasure
  Copper -> Basic CopperPile
  Silver -> Basic SilverPile
  Gold   -> Basic GoldPile
  -- Curse
  Curse -> Basic CursePile
  -- Victory
  Estate   -> Basic EstatePile
  Duchy    -> Basic DuchyPile
  Province -> Basic ProvincePile
  -- Kingdom
  -- 2
  Cellar -> Kingdom CellarPile
  Chapel -> Kingdom ChapelPile
  Moat   -> Kingdom MoatPile
  -- 3
  Harbinger -> Kingdom HarbingerPile
  Merchant  -> Kingdom MerchantPile
  Vassal    -> Kingdom VassalPile
  Village   -> Kingdom VillagePile
  Workshop  -> Kingdom WorkshopPile
  -- 4
  Bureacrat   -> Kingdom BureacratPile
  Gardens     -> Kingdom GardensPile
  Militia     -> Kingdom MilitiaPile
  Moneylender -> Kingdom MoneylenderPile
  Poacher     -> Kingdom PoacherPile
  Remodel     -> Kingdom RemodelPile
  Smithy      -> Kingdom SmithyPile
  ThroneRoom  -> Kingdom ThroneRoomPile
  -- 5
  Bandit      -> Kingdom BanditPile
  CouncilRoom -> Kingdom CouncilRoomPile
  Festival    -> Kingdom FestivalPile
  Laboratory  -> Kingdom LaboratoryPile
  Library     -> Kingdom LibraryPile
  Market      -> Kingdom MarketPile
  Mine        -> Kingdom MinePile
  Sentry      -> Kingdom SentryPile
  Witch       -> Kingdom WitchPile
  Artisan     -> Kingdom ArtisanPile
  
newtype Player = Player Int
  deriving (Eq, Ord, Show)

data PlayerZone =
    Deck
  | Hand
  | InPlay
  | DiscardPile
  | SetAside  -- TODO: different kinds of set aside zones?
  deriving (Eq, Ord, Show)

data Zone =
    OfPlayer Player PlayerZone
  | Supply SupplyPile
  | Trash
  deriving (Eq, Ord, Show)

data Mechanic =
    Buy Card
  | Gain Card PlayerZone
  | Discard Card
  | Play Card
  -- etc

data PlayerPrompt a where
  -- TODO
  GainOne :: NonEmpty Card -> PlayerPrompt Card


data Turn = Turn Player [Mechanic]

data GameState = GameState
  { zones :: Zone -> [Card]
  , turn :: Turn
  , gameLog :: [Turn]
  }

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

{- Export these and not the constructors (so you can't pattern match) -}
changeState = ChangeState
randomChoice = RandomChoice
playerChoice = PlayerChoice

initState :: GameState
initState = GameState { zones = (\z -> []), turn = Turn (Player 0) [], gameLog = [] }

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
dummyPlayer (GainOne cs) = NE.head cs


simTurn :: Player -> Game ()
simTurn p = return ()

main = putStrLn $ show Action



