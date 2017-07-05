module Types where

import           Data.Array
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game

type GameInput = [(Key, KeyState)]

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
  { levelGrid   :: Grid
  , levelCaches :: [Cache]
  , levelName   :: String
  } deriving Show

data Compass = Compass
  { compassPic   :: Picture
  , compassAngle :: Int
  }

data Signal = Signal
  { signalLives    :: Int
  , signalPic      :: Picture
  , signalLocation :: (Int, Int)
  , signalDelay    :: Int
  }

data Game = Game
  { gameInput  :: GameInput
  , gameLevel  :: Int
  , gameLevels :: Array Int Level
  , signal     :: Signal
  , compass    :: Compass
  }
