{-# LANGUAGE RecordWildCards #-}
module Compass where

import           Data.List
import           Data.Ord       (comparing)
import           Graphics.Gloss

import           Image
import           Renderable
import           Types

loadCompass :: Signal -> [Cache] -> IO Compass
loadCompass signal caches = do
  compassPic <- loadPNG "images/compass.png"
  return Compass
    { compassPic = compassPic
    , compassAngle = getAngleFromSignalToNearestCache signal caches
    }

instance Renderable Compass where
  render Compass{..} = Pictures [compassPic, needle]
    where needle = Translate 125 125 $
            Color black $
            Line [(0, 0), (needleLength * cos rads, needleLength * sin rads)]
          needleLength = 75
          rads = fromIntegral compassAngle * pi / 180

updateCompass :: Compass -> Signal -> [Cache] -> Compass
updateCompass compass@Compass{..} signal caches = compass { compassAngle = compassAngle' }
  where compassAngle' = (compassAngle + offset) `mod` 360
        desiredAngle = getAngleFromSignalToNearestCache signal caches
        offset | compassAngle == desiredAngle = 0
               | counterClockwiseDist < clockwiseDist = 1
               | otherwise = -1
        counterClockwiseDist = (desiredAngle - compassAngle) `mod` 360
        clockwiseDist = (compassAngle - desiredAngle) `mod` 360

getAngleFromSignalToNearestCache :: Signal -> [Cache] -> Int
getAngleFromSignalToNearestCache Signal{..} caches = getAngleFromSignalToCache (getNearestCache caches)
  where
    getNearestCache = minimumBy (comparing distFromSignalToCache)
      where dist (x1,y1) (x2,y2) = (x2 - x1)^2 + (y2 - y1)^2
            distFromSignalToCache cache = dist signalLocation (cacheLocation cache)

    getAngleFromSignalToCache Cache{..} = round $ angle' * 180 / pi
      where (signalX, signalY) = signalLocation
            (cacheX, cacheY) = cacheLocation
            (dx,dy) = (fromIntegral $ cacheX - signalX, fromIntegral $ cacheY - signalY)
            angle = atan2 dy dx
            angle' = if angle < 0 then angle + 2*pi else angle