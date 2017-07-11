{-# LANGUAGE RecordWildCards #-}
module Enemy where

import           Control.Monad
import           Graphics.Gloss
import           System.Random

import           Constants
import           Grid
import           Image
import           Renderable
import           Types

loadEnemyPics :: IO [Picture]
loadEnemyPics = do
  cactus <- loadPNG "images/cactus.png"
  spider <- loadPNG "images/spider.png"
  policeman <- loadPNG "images/policeman.png"
  return [cactus, spider, policeman]

getRandomEnemyPics :: [Picture] -> Int -> IO [Picture]
getRandomEnemyPics enemyPics numEnemies = do
  g <- newStdGen
  let randomIndices = take numEnemies $ randomRs (0, length enemyPics - 1) g
      randomPics = map (enemyPics !!) randomIndices
  return randomPics

innerCoords :: [(Int, Int)]
innerCoords = [ (x,y) | x <- inner, y <- inner ]
  where padding = 4
        inner = [padding..gridTiles - 1 - padding]

loadEnemies :: [Picture] -> Grid -> Int -> IO [Enemy]
loadEnemies enemyPics grid numEnemies = do
  locations <- getRandomLocations grid numEnemies innerCoords
  randomEnemyPics <- getRandomEnemyPics enemyPics numEnemies
  zipWithM (\location enemyPic -> do
    direction <- getRandomDirection grid location (0,0)
    return Enemy
      { enemyLocation = location
      , enemyDirection = direction
      , enemyPic = enemyPic
      , enemyTime = 0
      }) locations randomEnemyPics

loadAllEnemies :: [Grid] -> [Int] -> IO [[Enemy]]
loadAllEnemies grids numEnemiesPerLevel = do
  enemyPics <- loadEnemyPics
  zipWithM (loadEnemies enemyPics) grids numEnemiesPerLevel

instance Renderable Enemy where
  render Enemy{..} = renderOnGrid enemyLocation enemyPic

updateEnemy :: Bool -> Int -> Grid -> Enemy -> IO Enemy
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
