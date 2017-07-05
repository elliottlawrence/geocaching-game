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

loadCachePics :: IO (Array Int Picture)
loadCachePics = do
  pics <- mapM (\i -> loadPNG ("images/level" ++ show i ++ ".png")) [1..numLevels]
  return $ listArray (1, numLevels) pics

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
