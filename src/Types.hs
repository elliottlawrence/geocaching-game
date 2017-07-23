{-# LANGUAGE TypeFamilies #-}
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

data Color = Color Int Int Int Int

data Cell = Wall | Free deriving (Eq, Show)
data Grid = Grid
  { gridArray :: Array (Int, Int) Cell
  , gridColor :: Color
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
  , gameGrids     :: [Grid]
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

class Backend a where
  type Picture a = p | p -> a

  loadImage :: FilePath -> IO (Picture a)

  play ::
    a ->
    Int ->
    Game a ->
    (Game a -> Picture a) ->
    (Event -> Game a -> Game a) ->
    (Game a -> Game a) ->
    IO ()

  blank :: Picture a
  colored :: Color -> Picture a -> Picture a
  circleSolid :: Double -> Picture a
  line :: [(Double, Double)] -> Picture a
  pictures :: [Picture a] -> Picture a
  polygon :: [(Double, Double)] -> Picture a
  rectangle :: Double -> Double -> Picture a
  rectangle w h = polygon [(0, 0), (w, 0), (w, h), (0, h)]
  scale :: Double -> Double -> Picture a -> Picture a
  text :: String -> Picture a
  translate :: Double -> Double -> Picture a -> Picture a
