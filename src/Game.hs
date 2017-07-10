{-# LANGUAGE RecordWildCards #-}
module Game where

import           Data.Array
import           Data.Maybe
import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort

import           Compass
import           Constants
import           GameInput
import           Level
import           Renderable
import           Signal
import           Types
import           Utils

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

getCurrentLevel :: Game -> Level
getCurrentLevel Game{..} = gameLevels ! gameLevel

getCurrentGrid :: Game -> Grid
getCurrentGrid game = levelGrid
    where Level{..} = getCurrentLevel game

getCurrentCaches :: Game -> [Cache]
getCurrentCaches game = levelCaches
  where Level{..} = getCurrentLevel game

isGameWon :: Game -> Bool
isGameWon game@Game{..} = isLevelComplete (gameLevels ! numLevels)

isGameOver :: Game -> Bool
isGameOver Game{..} = signalLives signal <= 0

instance Renderable Game where
  render game@Game{..} = applyViewPortToPicture viewPort $ Pictures
    [ render compass
    , getGutterArea game
    , Translate (fromIntegral gutter) 0 gridArea
    ]
    where
      viewPort = viewPortInit {
        viewPortTranslate = (-fromIntegral windowX/2, -fromIntegral windowY/2)
      }
      gridArea = Pictures
        [ render $ getCurrentGrid game
        , render $ getCurrentCaches game
        , render enemies
        , render signal
        , getOverlay game
        ]
      enemies = levelEnemies $ getCurrentLevel game

getOverlay :: Game -> Picture
getOverlay game@Game{..}
  | isGameOver game || levelComplete =
    Pictures [Color transparentBlue $ rectangle gridSize windowY, textPicture]
  | otherwise = Blank
  where
    levelComplete = isLevelComplete (getCurrentLevel game)

    transparentBlue = makeColorI 0 30 60 130
    white = makeColorI 255 255 255 255

    maybeText
      | isGameOver game = Just "Game Over"
      | isGameWon game = Just "You Won!"
      | levelComplete = Just "Level Complete!"
      | otherwise = Nothing
    textPicture = maybe
      Blank
      (Translate (fromIntegral gridSize / 2 - 80) (fromIntegral windowY / 2 - 10) .
        Scale 0.2 0.2 .
        Color white .
        Text)
      maybeText

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

updateGame :: Float -> Game -> IO Game
updateGame _ game@Game{..}
  | (isGameOver game || isGameWon game) && isEnterDown = loadInitialGame
  | isJust jumpToLevel = return $ setLevel (fromJust jumpToLevel) game
  | isGameOver game || levelComplete = return game
  | otherwise = do
    updatedLevel <- updateLevel currentLevel signal' gameInput didSignalDie
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
    signal' = updateSignal signal gameInput (getCurrentGrid game) enemies
    compass' = updateCompass compass signal' (getCurrentCaches game)

    didSignalDie = signalLives signal' < signalLives signal
    gameInput' = updateGameInput gameInput didSignalDie

    isEnterDown = isKeyDown enterKey gameInput
    levelComplete = isLevelComplete currentLevel
    shouldProgressLevels = levelComplete && gameLevel < numLevels && isEnterDown

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
