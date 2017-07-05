{-# LANGUAGE RecordWildCards #-}
module Level where

import           Data.Array

import           Cache
import           Constants
import           Grid
import           Types

levelNames :: [String]
levelNames =
  [ "Traditional Cache"
  , "Multi-Cache"
  , "Mystery"
  , "Virtual Cache"
  , "Event Cache"
  , "Letterbox Hybrid"
  , "EarthCache"
  , "Webcam Cache"
  , "Wherigo Cache"
  , "Mega-Event Cache"
  ]

loadLevels :: IO (Array Int Level)
loadLevels = do
  grids <- loadGrids
  cacheLocations <- mapM getRandomLocations grids
  cachePics <- loadCachePics
  let caches = map (\(i, locations) ->
                map (\location -> Cache
                  { cacheLocation = location
                  , cacheFound = False
                  , cachePic = cachePics ! i
                  }) locations) $
                zip [1..] cacheLocations
      levels = zipWith3 (\grid caches name -> Level
                { levelGrid = grid
                , levelCaches = caches
                , levelName = name
                }) grids caches levelNames
  return $ listArray (1, numLevels) levels

updateLevel :: Level -> Signal -> GameInput -> Level
updateLevel level@Level{..} signal gameInput = level { levelCaches = levelCaches' }
  where levelCaches' = map (\cache -> updateCache cache signal gameInput) levelCaches
