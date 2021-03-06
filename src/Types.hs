{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE FlexibleContexts           #-}
module Types where

import           Control.Monad.State
import           Data.Array
import           System.Random

data KeyState = Up | Down deriving Eq

data Key
  = KeyLeft
  | KeyRight
  | KeyUp
  | KeyDown
  | KeySpace
  | KeyEnter
  | Char Char
  deriving Eq

data Event = EventKey Key KeyState

type GameInput = [(Key, (KeyState, Int))]

data Cell = Wall | Free deriving (Eq, Show)
data Grid a = Grid
  { gridArray :: Array (Int, Int) Cell
  , gridColor :: Color a
  }

data Cache a = Cache
  { cacheLocation :: (Int, Int)
  , cacheFound    :: Bool
  , cachePic      :: Picture a
  }

data Level a = Level
  { levelCaches  :: [Cache a]
  , levelName    :: String
  , levelEnemies :: [Enemy a]
  }

data Compass a = Compass
  { compassPic   :: Picture a
  , compassAngle :: Int
  }

data Signal a = Signal
  { signalLives    :: Int
  , signalPic      :: Picture a
  , signalLocation :: (Int, Int)
  }

data Enemy a = Enemy
  { enemyLocation  :: (Int, Int)
  , enemyDirection :: (Int, Int)
  , enemyPic       :: Picture a
  , enemyTime      :: Int
  }

data Game a = Game
  { gameInput     :: GameInput
  , gameLevel     :: Int
  , gameLevels    :: Array Int (Level a)
  , gameGetPic    :: GetPic a
  , gameGrids     :: [Grid a]
  , gameRandomGen :: StdGen
  , signal        :: Signal a
  , compass       :: Compass a
  }

data PictureName
  = Level1Pic
  | Level2Pic
  | Level3Pic
  | Level4Pic
  | Level5Pic
  | Level6Pic
  | Level7Pic
  | Level8Pic
  | Level9Pic
  | Level10Pic
  | SignalPic
  | CompassPic
  | CactusPic
  | SpiderPic
  | PolicemanPic
  deriving (Eq, Enum, Show)

type GetPic a = PictureName -> Picture a

type RandomT a = State StdGen a

data TextSize = BigText | SmallText

class (Floating (FloatType a)) => Backend a where
  type FloatType a = f | f -> a
  type Picture a = p | p -> a
  type Color a = c | c -> a
  type EventT a = e | e -> a

  play ::
    a ->
    Game a ->
    (Game a -> Picture a) ->
    (Event -> Game a -> Game a) ->
    (Game a -> Game a) ->
    IO ()

  loadImage :: FilePath -> IO (Picture a)

  loadFile :: a -> FilePath -> IO String

  makeColor :: Int -> Int -> Int -> Int -> Color a

  blank :: Picture a
  colored :: Color a -> Picture a -> Picture a
  circle :: FloatType a -> Picture a
  line :: FloatType a -> FloatType a ->
    FloatType a -> FloatType a -> Picture a
  pictures :: [Picture a] -> Picture a
  rectangle :: FloatType a -> FloatType a -> Picture a
  text :: TextSize -> String -> Picture a
  translate :: FloatType a -> FloatType a -> Picture a -> Picture a

  toEvent :: EventT a -> Maybe Event
  makeHandleInput :: (Event -> Game a -> Game a) -> (EventT a -> Game a -> Game a)
  makeHandleInput handleInput event game =
    maybe game (`handleInput` game) (toEvent event)
