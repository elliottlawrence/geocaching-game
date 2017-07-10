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
import           Utils

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

loadEnemies :: [Picture] -> Grid -> Int  -> IO [Enemy]
loadEnemies enemyPics grid numEnemies = do
  let padding = 4
      inner = [padding..gridTiles - 1 - padding]
      innerCoords = [ (x,y) | x <- inner, y <- inner ]

  locations <- getRandomLocations grid numEnemies innerCoords
  randomEnemyPics <- getRandomEnemyPics enemyPics numEnemies
  let enemies = zipWith (\location enemyPic -> Enemy
                { enemyLocation = location
                , enemyPic = enemyPic
                }) locations randomEnemyPics
  return enemies

loadAllEnemies :: [Grid] -> [Int] -> IO [[Enemy]]
loadAllEnemies grids numEnemiesPerLevel = do
  enemyPics <- loadEnemyPics
  zipWithM (loadEnemies enemyPics) grids numEnemiesPerLevel

instance Renderable Enemy where
  render Enemy{..} = renderOnGrid enemyLocation enemyPic
