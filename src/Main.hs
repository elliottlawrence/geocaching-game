{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Array
import Data.Char
import Data.List
import Data.Maybe
import Data.Ord (comparing)

import Debug.Trace

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy

import System.Random

isDebug :: Bool
isDebug = True

main :: IO ()
main = do
  initialGame <- loadInitialGame
  let display = InWindow "Geocaching Game" (windowX, windowY) (200, 200)

  play
    display
    black   -- background color
    60      -- fps
    initialGame
    render
    handleInput
    updateGame

type GameInput = [(Key, KeyState)]

data Game = Game
  { gameInput :: GameInput
  , gameLevel :: Int
  , gameLevels :: Array Int Level
  , signal :: Signal
  , compass :: Compass
  }

data Level = Level
  { levelGrid :: Grid
  , levelCaches :: [Cache]
  , levelName :: String
  } deriving Show

data Cache = Cache
  { cacheLocation :: (Int, Int)
  , cacheFound :: Bool
  , cachePic :: Picture
  } deriving Show

data Compass = Compass
  { compassPic :: Picture
  , compassAngle :: Int
  }

data Signal = Signal
  { signalLives :: Int
  , signalPic :: Picture
  , signalLocation :: (Int, Int)
  , signalDelay :: Int
  }

getCurrentGrid :: Game -> Grid
getCurrentGrid game = levelGrid
    where Level{..} = getCurrentLevel game

getCurrentCaches :: Game -> [Cache]
getCurrentCaches game = levelCaches
  where Level{..} = getCurrentLevel game

getCurrentLevel :: Game -> Level
getCurrentLevel Game{..} = gameLevels ! gameLevel

loadInitialGame :: IO Game
loadInitialGame = do
  signal <- loadSignal
  levels <- loadLevels
  let initialCaches = levelCaches (levels ! 1)
  compass <- loadCompass signal initialCaches
  return Game
    { gameInput = initialGameInput
    , gameLevel = 1
    , gameLevels = levels
    , signal = signal
    , compass = compass
    }

loadCachePics :: IO (Array Int Picture)
loadCachePics = do
  pics <- mapM (\i -> loadPNG ("images/level" ++ show i ++ ".png")) [1..numLevels]
  return $ listArray (1, numLevels) pics

loadLevels :: IO (Array Int Level)
loadLevels = do
  grids <- loadGrids
  cacheLocations <- mapM getRandomLocations grids
  cachePics <- loadCachePics
  let caches = map (\(i, locations) ->
                map (\location -> Cache
                  { cacheLocation = location
                  , cacheFound = False
                  , cachePic = cachePics ! i
                  }) locations) $
                zip [1..] cacheLocations
      levels = zipWith3 (\grid caches name -> Level
                { levelGrid = grid
                , levelCaches = caches
                , levelName = name
                }) grids caches levelNames
  return $ listArray (1, numLevels) levels
  where levelNames =
          [ "Traditional Cache"
          , "Multi-Cache"
          , "Mystery"
          , "Virtual Cache"
          , "Event Cache"
          , "Letterbox Hybrid"
          , "EarthCache"
          , "Webcam Cache"
          , "Wherigo Cache"
          , "Mega-Event Cache"
          ]

initialSignalLocation :: (Int, Int)
initialSignalLocation = (12,12)

loadSignal :: IO Signal
loadSignal = do
  signalPic <- loadPNG "images/signal.png"
  return Signal
    { signalLives = numLives
    , signalPic = signalPic
    , signalLocation = initialSignalLocation
    , signalDelay = 0
    }

loadCompass :: Signal -> [Cache] -> IO Compass
loadCompass signal caches = do
  compassPic <- loadPNG "images/compass.png"
  return Compass
    { compassPic = compassPic
    , compassAngle = getAngleFromSignalToNearestCache signal caches
    }

getRandomLocations :: Grid -> IO [(Int, Int)]
getRandomLocations grid = do
  g <- newStdGen
  let coords = map (\[x,y] -> (x,y)) $ chunkify 2 $ randomRs (0, gridTiles - 1) g
      locs = take numCachesPerLevel $ nub $ filter
        (\(x,y) -> isGridCellFree grid (x,y) && (x,y) /= initialSignalLocation)
        coords
  return locs

numCachesPerLevel, numLevels, numLives :: Int
numCachesPerLevel = 3
numLevels = 10
numLives = 5

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
  ] ++ map (\c -> (Char c, Up)) ['0'..'9']

leftKey, rightKey, upKey, downKey, spaceKey, enterKey :: Key
leftKey = SpecialKey KeyLeft
rightKey = SpecialKey KeyRight
upKey = SpecialKey KeyUp
downKey = SpecialKey KeyDown
spaceKey = SpecialKey KeySpace
enterKey = SpecialKey KeyEnter

class Renderable a where
  render :: a -> Picture

instance Renderable a => Renderable [a] where
  render = pictures . map render

instance Renderable Game where
  render game@Game{..} = applyViewPortToPicture viewPort $ Pictures
    [ render compass
    , gutterArea
    , Translate (fromIntegral gutter) 0 gridArea
    ]
    where gridArea = Pictures
            [ render $ getCurrentGrid game
            , render $ getCurrentCaches game
            , render signal
            ]
          gutterArea = getGutterArea game
          viewPort = viewPortInit {
            viewPortTranslate = (-fromIntegral windowX/2, -fromIntegral windowY/2)
          }

getGutterArea :: Game -> Picture
getGutterArea game@Game{..} =
  Pictures [levelText, cacheType, livesText, cachesLeftText]
  where
    gold = makeColorI 239 174 0 255
    orange = makeColorI 237 100 0 255

    createBigText = Scale 0.2 0.2 . Color gold . Text
    createSmallText = Scale 0.15 0.15 . Color orange . Text

    levelText = Translate 10 460 $ createBigText $ "Level " ++ show gameLevel ++ ":"
    cacheType = Translate 10 430 $ createBigText levelName

    livesText = Translate 10 350 $ createSmallText $ "Lives: " ++ show (signalLives signal)
    cachesLeftText = Translate 10 320 $ createSmallText $ "Caches left: " ++ show cachesLeft

    level@Level{..} = getCurrentLevel game
    cachesLeft = getCachesLeft level

getCachesLeft :: Level -> Int
getCachesLeft Level{..} = numCachesPerLevel - length (filter cacheFound levelCaches)

instance Renderable Signal where
  render Signal{..} = renderOnGrid signalLocation signalPic

instance Renderable Compass where
  render Compass{..} = Pictures [compassPic, needle]
    where needle = Translate 125 125 $
            Color black $
            Line [(0, 0), (needleLength * cos rads, needleLength * sin rads)]
          needleLength = 75
          rads = fromIntegral compassAngle * pi / 180

instance Renderable Cache where
  render Cache{..} = renderOnGrid cacheLocation pic
    where pic | cacheFound = cachePic
              | isDebug = Color red $ rectangle tileSize tileSize
              | otherwise = Blank

handleInput :: Event -> Game -> Game
handleInput (EventKey key keyState _ _) game@Game{..} = game {gameInput = gameInput'}
  where gameInput' = [ (k,ks') | (k,ks) <- gameInput, let ks' = if k == key then keyState else ks ]
handleInput _ game = game

isKeyDown :: Key -> GameInput -> Bool
isKeyDown key gameInput = case lookup key gameInput of
  Just keyState -> keyState == Down
  Nothing -> False

getNumKeyDown :: GameInput -> Maybe Int
getNumKeyDown gameInput = if null levels then Nothing else Just (head levels)
  where levels = [ levelNum | (Char c, Down) <- gameInput,
                   '0' <= c && c <= '9',
                   let levelNum = if c == '0' then 10 else digitToInt c ]

updateGame :: Float -> Game -> Game
updateGame _ game@Game{..}
  | isJust jumpToLevel = setLevel (fromJust jumpToLevel) game
  | otherwise = game
    { signal = signal'
    , compass = compass'
    , gameLevels = gameLevels'
    }
  where signal' = updateSignal signal gameInput (getCurrentGrid game)
        compass' = updateCompass compass signal' (getCurrentCaches game)
        gameLevels' = gameLevels // [(gameLevel, updatedLevel)]
        updatedLevel = updateLevel currentLevel signal' gameInput

        currentLevel = getCurrentLevel game
        isLevelComplete = getCachesLeft currentLevel == 0
        shouldProgressLevels = isLevelComplete && gameLevel < numLevels &&
          isKeyDown enterKey gameInput

        numKeyDown = getNumKeyDown gameInput
        jumpToLevel
          | shouldProgressLevels = Just (gameLevel + 1)
          | isDebug && isJust numKeyDown = numKeyDown
          | otherwise = Nothing

setLevel :: Int -> Game -> Game
setLevel level game@Game{..} = game
  { signal = signal'
  , gameLevel = level
  }
  where signal' = signal { signalLocation = initialSignalLocation }

updateLevel :: Level -> Signal -> GameInput -> Level
updateLevel level@Level{..} signal gameInput = level { levelCaches = levelCaches' }
  where levelCaches' = map (\cache -> updateCache cache signal gameInput) levelCaches

updateCache :: Cache -> Signal -> GameInput -> Cache
updateCache cache@Cache{..} Signal{..} gameInput
  | didSignalFindCache = cache { cacheFound = True }
  | otherwise = cache
  where didSignalFindCache = isKeyDown spaceKey gameInput && signalLocation == cacheLocation

updateSignal :: Signal -> GameInput -> Grid -> Signal
updateSignal signal@Signal{..} gameInput grid
  | signalDelay == 0 = signal
      { signalLocation = signalLocation''
      , signalDelay = 10
      }
  | otherwise = signal { signalDelay = signalDelay - 1 }
  where (x, y) = signalLocation
        (offsetX, offsetY)
          | isKeyDown rightKey gameInput = (1,0)
          | isKeyDown leftKey gameInput = (-1,0)
          | isKeyDown upKey gameInput = (0,1)
          | isKeyDown downKey gameInput = (0,-1)
          | otherwise = (0,0)
        signalLocation' = (x + offsetX, y + offsetY)
        signalLocation'' | isGridCellFree grid signalLocation' = signalLocation'
                         | otherwise = signalLocation

updateCompass :: Compass -> Signal -> [Cache] -> Compass
updateCompass compass@Compass{..} signal caches = compass { compassAngle = compassAngle' }
  where compassAngle' = (compassAngle + offset) `mod` 360
        desiredAngle = getAngleFromSignalToNearestCache signal caches
        offset | compassAngle == desiredAngle = 0
               | counterClockwiseDist < clockwiseDist = 1
               | otherwise = -1
        counterClockwiseDist = (desiredAngle - compassAngle) `mod` 360
        clockwiseDist = (compassAngle - desiredAngle) `mod` 360

getAngleFromSignalToNearestCache :: Signal -> [Cache] -> Int
getAngleFromSignalToNearestCache Signal{..} caches = getAngleFromSignalToCache (getNearestCache caches)
  where
    getNearestCache = minimumBy (comparing distFromSignalToCache)
      where dist (x1,y1) (x2,y2) = (x2 - x1)^2 + (y2 - y1)^2
            distFromSignalToCache cache = dist signalLocation (cacheLocation cache)

    getAngleFromSignalToCache Cache{..} = round $ angle' * 180 / pi
      where (signalX, signalY) = signalLocation
            (cacheX, cacheY) = cacheLocation
            (dx,dy) = (fromIntegral $ cacheX - signalX, fromIntegral $ cacheY - signalY)
            angle = atan2 dy dx
            angle' = if angle < 0 then angle + 2*pi else angle

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

data Cell = Wall | Free deriving (Eq, Show)
data Grid = Grid
  { gridArray :: Array (Int, Int) Cell
  , gridColor :: Color
  } deriving Show

instance Read Cell where
  readsPrec _ ('0':s) = [(Free,s)]
  readsPrec _ ('1':s) = [(Wall,s)]

loadGrids :: IO [Grid]
loadGrids = do
  gridData <- readFile "grids.txt"
  let cellChunks = chunkify (gridTiles ^ 2) (map read $ words gridData)
      gridArrays = map (listArray ((0, 0), (gridTiles - 1, gridTiles - 1))) cellChunks
      grids = take numLevels $ cycle $ zipWith Grid gridArrays levelColors
  return grids

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
