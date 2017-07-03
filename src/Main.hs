{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Array

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy

import System.Random

main :: IO ()
main = do
  grids <- loadGrids
  signal <- loadSignal
  compass <- loadCompass
  let display = InWindow "Geocaching Game" (windowX, windowY) (200, 200)
      initialGame = Game
        { gameGrids = grids
        , gameInput = initialGameInput
        , gameLevel = 1
        , signal = signal
        , compass = compass
        }

  play
    display
    black   -- background color
    60      -- fps
    initialGame
    render
    handleInput
    updateGame

type GameInput = [(Key, KeyState)]

data Cache = Cache
  { cacheLocation :: (Int, Int)
  , cacheFound :: Bool
  }

data Game = Game
  { gameGrids :: Array Int Grid
  , gameInput :: GameInput
  , gameLevel :: Int
  , signal :: Signal
  , compass :: Compass
  }

data Compass = Compass
  { compassPic :: Picture
  , compassAngle :: Int
  }

data Signal = Signal
  { signalLives :: Int
  , signalPic :: Picture
  , signalPos :: (Int, Int)
  , signalDelay :: Int
  }

initialSignalPos = (12,12)

loadSignal :: IO Signal
loadSignal = do
  signalPic <- loadPNG "images/signal.png"
  return Signal
    { signalLives = 5
    , signalPic = signalPic
    , signalPos = initialSignalPos
    , signalDelay = 0
    }

loadCompass :: IO Compass
loadCompass = do
  compassPic <- loadPNG "images/compass.png"
  return Compass
    { compassPic = compassPic
    , compassAngle = 90
    }

getRandomLocations :: Grid -> IO [(Int, Int)]
getRandomLocations grid = do
  g <- getStdGen
  let coords = map (\[x,y] -> (x,y)) $ chunkify 2 $ randomRs (0, gridTiles - 1) g
      locs = take 3 $ filter
        (\(x,y) -> isGridCellFree grid (x,y) && (x,y) /= initialSignalPos)
        coords
  return locs


gridTiles, gridSize, gutter, tileSize, windowX, windowY :: Int
gridTiles = 25
tileSize = 20
gridSize = gridTiles * tileSize
gutter = 250
windowX = gutter + gridSize
windowY = gridSize

initialGameInput :: GameInput
initialGameInput =
  [ (leftKey, Up)
  , (rightKey, Up)
  , (downKey, Up)
  , (upKey, Up)
  , (spaceKey, Up)
  , (enterKey, Up)
  ]

leftKey, rightKey, upKey, downKey, spaceKey, enterKey :: Key
leftKey = SpecialKey KeyLeft
rightKey = SpecialKey KeyRight
upKey = SpecialKey KeyUp
downKey = SpecialKey KeyDown
spaceKey = SpecialKey KeySpace
enterKey = SpecialKey KeyEnter

class Renderable a where
  render :: a -> Picture

instance Renderable Game where
  render Game{..} = applyViewPortToPicture viewPort $ Pictures
    [ render compass
    , gutterArea
    , Translate (fromIntegral gutter) 0 gridArea
    ]
    where gridArea = Pictures
            [ render $ gameGrids ! gameLevel
            , render signal
            ]
          viewPort = viewPortInit {
            viewPortTranslate = (-fromIntegral windowX/2, -fromIntegral windowY/2)
          }

gutterArea :: Picture
gutterArea = Pictures [levelText, cacheType, livesText, trackablesText, cachesLeftText]
  where
    gold = makeColorI 239 174 0 255
    orange = makeColorI 237 100 0 255

    createBigText = Scale 0.2 0.2 . Color gold . Text
    createSmallText = Scale 0.15 0.15 . Color orange . Text

    levelText = Translate 10 460 $ createBigText "Level 1:"
    cacheType = Translate 10 430 $ createBigText "Traditional Cache"

    livesText = Translate 10 350 $ createSmallText "Lives: 5"
    trackablesText = Translate 10 320 $ createSmallText "Trackables: 0"
    cachesLeftText = Translate 10 290 $ createSmallText "Caches left: 3"

instance Renderable Signal where
  render Signal{..} = renderOnGrid signalPos signalPic

instance Renderable Compass where
  render Compass{..} = Pictures [compassPic, needle]
    where needle = Translate 125 125 $
            Color black $
            Line [(0, 0), (needleLength * cos rads, needleLength * sin rads)]
          needleLength = 75
          rads = fromIntegral compassAngle * pi / 180

handleInput :: Event -> Game -> Game
handleInput (EventKey key keyState _ _) game@Game{..} = game {gameInput = gameInput'}
  where gameInput' = [ (k,ks') | (k,ks) <- gameInput, let ks' = if k == key then keyState else ks ]
handleInput _ game = game

isKeyDown :: Key -> GameInput -> Bool
isKeyDown key gameInput = case lookup key gameInput of
  Just keyState -> keyState == Down
  Nothing -> False

updateGame :: Float -> Game -> Game
updateGame _ game@Game{..} = game { signal = signal' }
  where signal' = updateSignal signal gameInput (gameGrids ! gameLevel)

updateSignal :: Signal -> GameInput -> Grid -> Signal
updateSignal signal@Signal{..} gameInput grid
  | signalDelay == 0 = signal
      { signalPos = signalPos''
      , signalDelay = 10
      }
  | otherwise = signal { signalDelay = signalDelay - 1 }
  where (x, y) = signalPos
        (offsetX, offsetY)
          | isKeyDown rightKey gameInput = (1,0)
          | isKeyDown leftKey gameInput = (-1,0)
          | isKeyDown upKey gameInput = (0,1)
          | isKeyDown downKey gameInput = (0,-1)
          | otherwise = (0,0)
        signalPos' = (x + offsetX, y + offsetY)
        signalPos'' | isGridCellFree grid signalPos' = signalPos'
                    | otherwise = signalPos

loadPNG :: FilePath -> IO Picture
loadPNG path = do
  png <- loadJuicyPNG path
  case png of
    Just bitmap@(Bitmap w h _ _) -> return $
      Translate (fromIntegral w/2) (fromIntegral h/2) bitmap
    Nothing -> ioError $ userError $ "File not found: " ++ path

wallColor :: Color
wallColor = makeColorI 96 96 96 255

levelColors :: [Color]
levelColors = cycle
  [ makeColorI 161 212 131 255
  , makeColorI 117 167 218 255
  , makeColorI 243 189 146 255
  , makeColorI 247 246 144 255
  , makeColorI 197 113 241 255
  ]

data Cell = Wall | Free deriving Eq
data Grid = Grid
  { gridArray :: Array (Int, Int) Cell
  , gridColor :: Color
  }

instance Read Cell where
  readsPrec _ ('0':s) = [(Free,s)]
  readsPrec _ ('1':s) = [(Wall,s)]

loadGrids :: IO (Array Int Grid)
loadGrids = do
  gridData <- readFile "grids.txt"
  let cellChunks = chunkify (gridTiles ^ 2) (map read $ words gridData)
      gridArrays = map (listArray ((0, 0), (gridTiles - 1, gridTiles - 1))) cellChunks
      grids = zipWith Grid gridArrays levelColors
  return $ listArray (1, length grids) grids

chunkify :: Int -> [a] -> [[a]]
chunkify i [] = []
chunkify i xs = first : chunkify i rest
  where (first, rest) = splitAt i xs

instance Renderable Grid where
  render Grid{..} = Pictures cells
    where cells = map (\((x,y), cell) ->
            renderOnGrid (x,y) $
            (case cell of
              Wall -> Color wallColor
              Free -> Color gridColor) $
            rectangle tileSize tileSize)
            (assocs gridArray)

rectangle :: Int -> Int -> Picture
rectangle w h = Polygon [(0, 0), (w', 0), (w', h'), (0, h')]
  where [w', h'] = map fromIntegral [w, h]

renderOnGrid :: (Int, Int) -> Picture ->  Picture
renderOnGrid (x, y) = Translate x' y'
  where [x', y'] = map (fromIntegral . (* tileSize)) [x, y]

isGridCellFree :: Grid -> (Int, Int)  -> Bool
isGridCellFree Grid{..} coord
  | inRange (bounds gridArray) coord = gridArray ! coord == Free
  | otherwise = False
