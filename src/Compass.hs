{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
module Compass where

import           Data.List
import           Data.Ord   (comparing)

import           Constants
import           Renderable
import           Types

loadCompass :: GetPic a -> Signal a -> [Cache a] -> Compass a
loadCompass getPic signal caches = Compass
  { compassPic = getPic CompassPic
  , compassAngle = getAngleFromSignalToNearestCache signal caches
  }

instance Backend a => Renderable (Compass a) a where
  render Compass{..} = translate 125 (windowY - 125) $
    pictures [compassPic, needle]
    where needle = colored black $
            line 0 0 (needleLength * cos rads) (-needleLength * sin rads)
          needleLength = 75
          rads = fromIntegral compassAngle * pi / 180

updateCompass :: Compass a -> Signal a -> [Cache a] -> Compass a
updateCompass compass@Compass{..} signal caches = compass { compassAngle = compassAngle' }
  where compassAngle' = (compassAngle + offset) `mod` 360
        desiredAngle = getAngleFromSignalToNearestCache signal caches
        offset | compassAngle == desiredAngle = 0
               | counterClockwiseDist < clockwiseDist = 1
               | otherwise = -1
        counterClockwiseDist = (desiredAngle - compassAngle) `mod` 360
        clockwiseDist = (compassAngle - desiredAngle) `mod` 360

getAngleFromSignalToNearestCache :: Signal a -> [Cache a] -> Int
getAngleFromSignalToNearestCache Signal{..} caches = getAngleFromSignalToCache (getNearestCache caches)
  where
    getNearestCache = minimumBy (comparing distFromSignalToCache)
      where dist (x1,y1) (x2,y2) = (x2 - x1)^(2 :: Int) + (y2 - y1)^(2 :: Int)
            distFromSignalToCache cache = dist signalLocation (cacheLocation cache)

    getAngleFromSignalToCache Cache{..} = round $ angle' * 180 / (pi :: Double)
      where (signalX, signalY) = signalLocation
            (cacheX, cacheY) = cacheLocation
            (dx,dy) = (fromIntegral $ cacheX - signalX, fromIntegral $ signalY - cacheY)
            angle = atan2 dy dx
            angle' = if angle < 0 then angle + 2*pi else angle
