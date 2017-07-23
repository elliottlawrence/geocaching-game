{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
module Game where

import           Control.Monad.State
import           Data.Array
import           Data.Maybe

import           Compass
import           Constants
import           GameInput
import           Level
import           Renderable
import           Signal
import           Types
import           Utils

loadInitialGame :: GetPic a -> [Grid] -> RandomT (Game a)
loadInitialGame getPic grids = do
  levels <- loadLevels getPic grids
  let signal = loadSignal getPic
      initialCaches = levelCaches (levels ! 1)
      compass = loadCompass getPic signal initialCaches
  g <- get

  return Game
    { gameInput = initialGameInput
    , gameLevel = 1
    , gameLevels = levels
    , gameGetPic = getPic
    , gameGrids = grids
    , gameRandomGen = g
    , signal = signal
    , compass = compass
    }

getCurrentLevel :: Game a -> Level a
getCurrentLevel Game{..} = gameLevels ! gameLevel

getCurrentGrid :: Game a -> Grid
getCurrentGrid Game{..} = gameGrids !! (gameLevel - 1)

getCurrentCaches :: Game a -> [Cache a]
getCurrentCaches game = levelCaches
  where Level{..} = getCurrentLevel game

isGameWon :: Game a -> Bool
isGameWon Game{..} = isLevelComplete (gameLevels ! numLevels)

isGameOver :: Game a -> Bool
isGameOver Game{..} = signalLives signal <= 0

instance Backend a => Renderable (Game a) a where
  render game@Game{..} = pictures
    [ render compass
    , getGutterArea game
    , translate (fromIntegral gutter) 0 gridArea
    ]
    where
      gridArea = pictures
        [ render $ getCurrentGrid game
        , render $ getCurrentCaches game
        , render enemies
        , render signal
        , getOverlay game
        ]
      enemies = levelEnemies $ getCurrentLevel game

getOverlay :: Backend a => Game a -> Picture a
getOverlay game@Game{..}
  | isGameOver game || levelComplete =
    pictures
      [ colored transparentBlue $ rectangle (fromIntegral gridSize) (fromIntegral windowY)
      , textPicture
      ]
  | otherwise = blank
  where
    levelComplete = isLevelComplete (getCurrentLevel game)

    transparentBlue = Color 0 30 60 130
    white = Color 255 255 255 255

    maybeText
      | isGameOver game = Just "Game Over"
      | isGameWon game = Just "You Won!"
      | levelComplete = Just "Level Complete!"
      | otherwise = Nothing
    textPicture = maybe
      blank
      (translate (fromIntegral gridSize / 2 - 80) (fromIntegral windowY / 2 - 10) .
        scale 0.2 0.2 .
        colored white .
        text)
      maybeText

getGutterArea :: Backend a => Game a -> Picture a
getGutterArea game@Game{..} =
  pictures [levelText, cacheType, livesText, cachesLeftText]
  where
    gold = Color 239 174 0 255
    orange = Color 237 100 0 255

    createBigText = scale 0.2 0.2 . colored gold . text
    createSmallText = scale 0.15 0.15 . colored orange . text

    levelText = translate 10 460 $ createBigText $ "Level " ++ show gameLevel ++ ":"
    cacheType = translate 10 430 $ createBigText levelName

    livesText = translate 10 350 $ createSmallText $ "Lives: " ++ show (signalLives signal)
    cachesLeftText = translate 10 320 $ createSmallText $ "Caches left: " ++ show cachesLeft

    level@Level{..} = getCurrentLevel game
    cachesLeft = getCachesLeft level

updateGame :: Game a -> Game a
updateGame game@Game{..} = game' { gameRandomGen = g' }
  where (game', g') = runState (updateGame' game) gameRandomGen

updateGame' :: Game a -> RandomT (Game a)
updateGame' game@Game{..}
  | (isGameOver game || isGameWon game) && isEnterDown gameInput =
    loadInitialGame gameGetPic gameGrids
  | isJust jumpToLevel = return $ setLevel (fromJust jumpToLevel) game
  | isGameOver game || levelComplete = return game
  | otherwise = do
    updatedLevel <- updateLevel currentLevel signal' grid gameInput didSignalDie
    let gameLevels' = gameLevels // [(gameLevel, updatedLevel)]

    return game
      { signal = signal'
      , compass = compass'
      , gameInput = gameInput'
      , gameLevels = gameLevels'
      }
  where
    currentLevel = getCurrentLevel game
    enemies = levelEnemies currentLevel
    grid = getCurrentGrid game

    signal' = updateSignal signal gameInput grid enemies
    compass' = updateCompass compass signal' (getCurrentCaches game)

    didSignalDie = signalLives signal' < signalLives signal
    gameInput' = updateGameInput gameInput didSignalDie

    levelComplete = isLevelComplete currentLevel
    shouldProgressLevels =
      levelComplete && gameLevel < numLevels && isEnterDown gameInput

    numKeyDown = getNumKeyDown gameInput
    jumpToLevel
      | shouldProgressLevels = Just (gameLevel + 1)
      | isDebug && isJust numKeyDown = numKeyDown
      | otherwise = Nothing

setLevel :: Int -> Game a -> Game a
setLevel level game@Game{..} = game
  { signal = signal'
  , gameLevel = level
  }
  where signal' = signal { signalLocation = initialSignalLocation }
