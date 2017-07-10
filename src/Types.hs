module Types where

import           Data.Array
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game

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
  { levelGrid    :: Grid
  , levelCaches  :: [Cache]
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
  { gameInput  :: GameInput
  , gameLevel  :: Int
  , gameLevels :: Array Int Level
  , signal     :: Signal
  , compass    :: Compass
  }
