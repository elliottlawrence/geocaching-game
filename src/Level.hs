{-# LANGUAGE RecordWildCards #-}
module Level where

import           Data.Array
import           Data.List

import           Cache
import           Constants
import           Enemy
import           Grid
import           Types

type LevelConfig = (String, Int, Int)

levelConfigs :: [LevelConfig]
levelConfigs =
  [ ("Traditional Cache", 3, 1)
  , ("Multi-Cache", 3, 1)
  , ("Mystery", 4, 2)
  , ("Virtual Cache", 4, 2)
  , ("Event Cache", 5, 2)
  , ("Letterbox Hybrid", 6, 3)
  , ("EarthCache", 7, 3)
  , ("Webcam Cache", 8, 4)
  , ("Wherigo Cache", 9, 4)
  , ("Mega-Event Cache", 10, 5)
  ]

loadLevels :: IO (Array Int Level)
loadLevels = do
  grids <- loadGrids

  let (levelNames, numCachesPerLevel, numEnemiesPerLevel) = unzip3 levelConfigs
  allCaches <- loadAllCaches grids numCachesPerLevel
  allEnemies <- loadAllEnemies grids numEnemiesPerLevel

  let levels = zipWith4 (\grid caches enemies name -> Level
                { levelGrid = grid
                , levelCaches = caches
                , levelName = name
                , levelEnemies = enemies
                }) grids allCaches allEnemies levelNames
  return $ listArray (1, numLevels) levels

updateLevel :: Level -> Signal -> GameInput -> IO Level
updateLevel level@Level{..} signal gameInput = do
  levelEnemies' <- mapM (updateEnemy levelGrid) levelEnemies
  let levelCaches' = map (\cache -> updateCache cache signal gameInput) levelCaches
  return level { levelCaches = levelCaches', levelEnemies = levelEnemies' }

getCachesLeft :: Level -> Int
getCachesLeft Level{..} = length levelCaches - length (filter cacheFound levelCaches)

isLevelComplete :: Level -> Bool
isLevelComplete level = getCachesLeft level == 0
