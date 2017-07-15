{-# LANGUAGE RecordWildCards #-}
module Cache where

import           Graphics.Gloss

import           Constants
import           GameInput
import           Grid
import           Image
import           Renderable
import           Types
import           Utils

loadCaches :: GetPic -> Grid -> Int -> Int -> RandomT [Cache]
loadCaches getPic grid levelNum numCaches = do
  locations <- getRandomLocations grid numCaches [initialSignalLocation]
  let cachePic = getPic (getPicNameForLevel levelNum)
      caches = map (\location -> Cache
                { cacheLocation = location
                , cacheFound = False
                , cachePic = cachePic
                }) locations
  return caches

loadAllCaches :: GetPic -> [Grid] -> [Int] -> RandomT [[Cache]]
loadAllCaches getPic grids = zipWith3M (loadCaches getPic) grids [1..]

instance Renderable Cache where
  render Cache{..} = renderOnGrid cacheLocation pic
    where pic | cacheFound = cachePic
              -- | isDebug = Color red $ rectangle tileSize tileSize
              | otherwise = Blank

updateCache :: Signal -> GameInput -> Cache -> Cache
updateCache Signal{..} gameInput cache@Cache{..}
  | didSignalFindCache = cache { cacheFound = True }
  | otherwise = cache
  where didSignalFindCache = isKeyDown spaceKey gameInput && signalLocation == cacheLocation
