{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
module Signal where

import           Constants
import           GameInput
import           Grid
import           Renderable
import           Types

loadSignal :: GetPic a -> Signal a
loadSignal getPic = Signal
  { signalLives = numLives
  , signalPic = getPic SignalPic
  , signalLocation = initialSignalLocation
  }

instance Backend a => Renderable (Signal a) a where
  render Signal{..} = renderOnGrid signalLocation signalPic

updateSignal :: Signal a -> GameInput -> Grid -> [Enemy a] -> Signal a
updateSignal signal@Signal{..} gameInput grid enemies
  | shouldSignalDie signal enemies = loseLife signal
  | otherwise = signal { signalLocation = signalLocation'' }
  where (x, y) = signalLocation
        (offsetX, offsetY)
          | isKeyDown KeyRight gameInput = (1,0)
          | isKeyDown KeyLeft gameInput = (-1,0)
          | isKeyDown KeyUp gameInput = (0,-1)
          | isKeyDown KeyDown gameInput = (0,1)
          | otherwise = (0,0)
        signalLocation' = (x + offsetX, y + offsetY)
        signalLocation'' = if isGridCellFree grid signalLocation'
          then signalLocation'
          else signalLocation

shouldSignalDie :: Signal a -> [Enemy a] -> Bool
shouldSignalDie Signal{..} = any (\Enemy{..} -> signalLocation == enemyLocation)

loseLife :: Signal a -> Signal a
loseLife signal@Signal{..} =
  signal { signalLives = signalLives', signalLocation = signalLocation' }
  where signalLives' = signalLives - 1
        signalLocation' = if signalLives' > 0
          then initialSignalLocation
          else signalLocation
