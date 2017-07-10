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

loadCaches :: Grid -> Int -> Int -> IO [Cache]
loadCaches grid levelNum numCaches = do
  locations <- getRandomLocations grid numCaches [initialSignalLocation]
  cachePic <- loadPNG $ "images/level" ++ show levelNum ++ ".png"
  let caches = map (\location -> Cache
                { cacheLocation = location
                , cacheFound = False
                , cachePic = cachePic
                }) locations
  return caches

loadAllCaches :: [Grid] -> [Int] -> IO [[Cache]]
loadAllCaches grids = zipWith3M loadCaches grids [1..]

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
