{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
module Cache where

import           Constants
import           GameInput
import           Grid
import           Image
import           Renderable
import           Types
import           Utils

loadCaches :: GetPic a -> Grid a -> Int -> Int -> RandomT [Cache a]
loadCaches getPic grid levelNum numCaches = do
  locations <- getRandomLocations grid numCaches [initialSignalLocation]
  let cachePic = getPic (getPicNameForLevel levelNum)
      caches = map (\location -> Cache
                { cacheLocation = location
                , cacheFound = False
                , cachePic = cachePic
                }) locations
  return caches

loadAllCaches :: GetPic a -> [Grid a] -> [Int] -> RandomT [[Cache a]]
loadAllCaches getPic grids = zipWith3M (loadCaches getPic) grids [1..]

instance Backend a => Renderable (Cache a) a where
  render Cache{..} = renderOnGrid cacheLocation pic
    where pic | cacheFound = cachePic
              | isDebug = colored red $ rectangle tileSize tileSize
              | otherwise = blank

updateCache :: Signal a -> GameInput -> Cache a -> Cache a
updateCache Signal{..} gameInput cache@Cache{..}
  | didSignalFindCache = cache { cacheFound = True }
  | otherwise = cache
  where didSignalFindCache = isKeyDown KeySpace gameInput && signalLocation == cacheLocation
