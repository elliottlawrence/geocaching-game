{-# LANGUAGE RecordWildCards #-}
module Enemy where

import           Control.Monad
import           System.Random

import           Backend
import           Constants
import           Grid
import           Renderable
import           Types
import           Utils

getRandomEnemyPics :: [Picture] -> Int -> RandomT [Picture]
getRandomEnemyPics enemyPics numEnemies = makeRandomT $
  map (enemyPics !!) . take numEnemies . randomRs (0, length enemyPics - 1)

innerCoords :: [(Int, Int)]
innerCoords = [ (x,y) | x <- inner, y <- inner ]
  where padding = 4
        inner = [padding..gridTiles - 1 - padding]

loadEnemies :: GetPic -> Grid -> Int -> RandomT [Enemy]
loadEnemies getPic grid numEnemies = do
  let enemyPics = map getPic [CactusPic, SpiderPic, PolicemanPic]
  randomEnemyPics <- getRandomEnemyPics enemyPics numEnemies
  locations <- getRandomLocations grid numEnemies innerCoords

  zipWithM (\location enemyPic -> do
    direction <- getRandomDirection grid location (0,0)
    return Enemy
      { enemyLocation = location
      , enemyDirection = direction
      , enemyPic = enemyPic
      , enemyTime = 0
      }) locations randomEnemyPics

loadAllEnemies :: GetPic -> [Grid] -> [Int] -> RandomT [[Enemy]]
loadAllEnemies getPic = zipWithM (loadEnemies getPic)

instance Renderable Enemy where
  render Enemy{..} = renderOnGrid enemyLocation enemyPic

updateEnemy :: Bool -> Int -> Grid -> Enemy -> RandomT Enemy
updateEnemy didSignalDie signalLives grid enemy@Enemy{..}
  | didSignalDie && signalLives > 0 = do
    [enemyLocation'] <- getRandomLocations grid 1 innerCoords
    return enemy
      { enemyLocation = enemyLocation'
      , enemyTime = enemyTime + 1
      }
  | enemyTime `mod` enemySpeed == 0 = do
    enemyDirection' <- getRandomDirection grid enemyLocation enemyDirection
    let enemyLocation' = enemyLocation `addPoints` enemyDirection'
    return enemy
      { enemyDirection = enemyDirection'
      , enemyLocation = enemyLocation'
      , enemyTime = enemyTime + 1
      }
  | otherwise = return enemy { enemyTime = enemyTime + 1 }
  where enemySpeed = 20
