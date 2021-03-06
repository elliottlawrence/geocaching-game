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

loadInitialGame :: GetPic a -> [Grid a] -> RandomT (Game a)
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

getCurrentGrid :: Game a -> Grid a
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
    , translate gutter 0 gridArea
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
    translate (gridSize / 2) (windowY / 2) $
    pictures [background, textPicture]
  | otherwise = blank
  where
    levelComplete = isLevelComplete (getCurrentLevel game)

    transparentBlue = makeColor 0 30 60 130
    white = makeColor 255 255 255 255

    background = colored transparentBlue $ rectangle gridSize windowY

    maybeText
      | isGameOver game = Just "Game Over"
      | isGameWon game = Just "You Won!"
      | levelComplete = Just "Level Complete!"
      | otherwise = Nothing
    textPicture = maybe
      blank
      (translate (-80) 10 . colored white . text BigText)
      maybeText

getGutterArea :: Backend a => Game a -> Picture a
getGutterArea game@Game{..} =
  pictures [levelText, cacheType, livesText, cachesLeftText]
  where
    gold = makeColor 239 174 0 255
    orange = makeColor 237 100 0 255

    createBigText = colored gold . text BigText
    createSmallText = colored orange . text SmallText

    levelText = translate 10 40 $ createBigText $ "Level " ++ show gameLevel ++ ":"
    cacheType = translate 10 70 $ createBigText levelName

    livesText = translate 10 150 $ createSmallText $ "Lives: " ++ show (signalLives signal)
    cachesLeftText = translate 10 180 $ createSmallText $ "Caches left: " ++ show cachesLeft

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
