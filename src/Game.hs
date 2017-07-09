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

getCachesLeft :: Level -> Int
getCachesLeft Level{..} = numCachesPerLevel - length (filter cacheFound levelCaches)

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

updateGame :: Float -> Game -> Game
updateGame _ game@Game{..}
  | isJust jumpToLevel = setLevel (fromJust jumpToLevel) game
  | otherwise = game
    { signal = signal'
    , compass = compass'
    , gameInput = gameInput'
    , gameLevels = gameLevels'
    }
  where signal' = updateSignal signal gameInput (getCurrentGrid game)
        compass' = updateCompass compass signal' (getCurrentCaches game)
        gameLevels' = gameLevels // [(gameLevel, updatedLevel)]
        updatedLevel = updateLevel currentLevel signal' gameInput

        gameInput' = updateGameInput gameInput

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
