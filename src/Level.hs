{-# LANGUAGE RecordWildCards #-}
module Level where

import           Data.Array

import           Cache
import           Constants
import           Enemy
import           Types

type LevelConfig = (String, Int, Int)

levelConfigs :: [LevelConfig]
levelConfigs =
  [ ("Traditional Cache", 2, 1)
  , ("Multi-Cache", 2, 1)
  , ("Mystery", 3, 2)
  , ("Virtual Cache", 4, 2)
  , ("Event Cache", 5, 2)
  , ("Letterbox Hybrid", 6, 3)
  , ("EarthCache", 7, 3)
  , ("Webcam Cache", 8, 4)
  , ("Wherigo Cache", 9, 4)
  , ("Mega-Event Cache", 10, 5)
  ]

loadLevels :: GetPic -> [Grid] -> RandomT (Array Int Level)
loadLevels getPic grids = do
  let (levelNames, numCachesPerLevel, numEnemiesPerLevel) = unzip3 levelConfigs
  allCaches <- loadAllCaches getPic grids numCachesPerLevel
  allEnemies <- loadAllEnemies getPic grids numEnemiesPerLevel

  let levels = zipWith3 (\caches enemies name -> Level
                { levelCaches = caches
                , levelName = name
                , levelEnemies = enemies
                }) allCaches allEnemies levelNames
  return $ listArray (1, numLevels) levels

updateLevel :: Level -> Signal -> Grid -> GameInput -> Bool -> RandomT Level
updateLevel level@Level{..} signal@Signal{..} grid gameInput didSignalDie = do
  let levelCaches' = map (updateCache signal gameInput) levelCaches
  levelEnemies' <- mapM (updateEnemy didSignalDie signalLives grid) levelEnemies

  return level
    { levelCaches = levelCaches'
    , levelEnemies = levelEnemies'
    }


getCachesLeft :: Level -> Int
getCachesLeft Level{..} = length levelCaches - length (filter cacheFound levelCaches)

isLevelComplete :: Level -> Bool
isLevelComplete level = getCachesLeft level == 0
