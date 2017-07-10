{-# LANGUAGE RecordWildCards #-}
module Cache where

import           Data.Array
import           Graphics.Gloss

import           Constants
import           GameInput
import           Grid
import           Image
import           Renderable
import           Types
import           Utils

loadCaches :: Grid -> Picture -> Int -> IO [Cache]
loadCaches grid cachePic numCaches = do
  locations <- getRandomLocations grid numCaches [initialSignalLocation]
  let caches = map (\location -> Cache
                { cacheLocation = location
                , cacheFound = False
                , cachePic = cachePic
                }) locations
  return caches

loadAllCaches :: [Grid] -> [Int] -> IO [[Cache]]
loadAllCaches grids numCachesPerLevel = do
  cachePics <- mapM (\i -> loadPNG ("images/level" ++ show i ++ ".png")) [1..numLevels]
  zipWith3M loadCaches grids cachePics numCachesPerLevel

instance Renderable Cache where
  render Cache{..} = renderOnGrid cacheLocation pic
    where pic | cacheFound = cachePic
              | isDebug = Color red $ rectangle tileSize tileSize
              | otherwise = Blank

updateCache :: Cache -> Signal -> GameInput -> Cache
updateCache cache@Cache{..} Signal{..} gameInput
  | didSignalFindCache = cache { cacheFound = True }
  | otherwise = cache
  where didSignalFindCache = isKeyDown spaceKey gameInput && signalLocation == cacheLocation
