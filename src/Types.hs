module Types where

import           Control.Monad.State
import           Data.Array
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           System.Random

type GameInput = [(Key, (KeyState, Int))]

data Cell = Wall | Free deriving (Eq, Show)
data Grid = Grid
  { gridArray :: Array (Int, Int) Cell
  , gridColor :: Color
  } deriving Show

data Cache = Cache
  { cacheLocation :: (Int, Int)
  , cacheFound    :: Bool
  , cachePic      :: Picture
  } deriving Show

data Level = Level
  { levelCaches  :: [Cache]
  , levelName    :: String
  , levelEnemies :: [Enemy]
  } deriving Show

data Compass = Compass
  { compassPic   :: Picture
  , compassAngle :: Int
  }

data Signal = Signal
  { signalLives    :: Int
  , signalPic      :: Picture
  , signalLocation :: (Int, Int)
  }

data Enemy = Enemy
  { enemyLocation  :: (Int, Int)
  , enemyDirection :: (Int, Int)
  , enemyPic       :: Picture
  , enemyTime      :: Int
  } deriving Show

data Game = Game
  { gameInput     :: GameInput
  , gameLevel     :: Int
  , gameLevels    :: Array Int Level
  , gameGetPic    :: GetPic
  , gameGrids     :: [Grid]
  , gameRandomGen :: StdGen
  , signal        :: Signal
  , compass       :: Compass
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

type GetPic = PictureName -> Picture

type RandomT a = State StdGen a
