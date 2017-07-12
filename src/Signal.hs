{-# LANGUAGE RecordWildCards #-}
module Signal where

import           Constants
import           GameInput
import           Grid
import           Renderable
import           Types

loadSignal :: GetPic -> Signal
loadSignal getPic = Signal
  { signalLives = numLives
  , signalPic = getPic SignalPic
  , signalLocation = initialSignalLocation
  }

instance Renderable Signal where
  render Signal{..} = renderOnGrid signalLocation signalPic

updateSignal :: Signal -> GameInput -> Grid -> [Enemy] -> Signal
updateSignal signal@Signal{..} gameInput grid enemies
  | shouldSignalDie signal enemies = loseLife signal
  | otherwise = signal { signalLocation = signalLocation'' }
  where (x, y) = signalLocation
        (offsetX, offsetY)
          | isKeyDown rightKey gameInput = (1,0)
          | isKeyDown leftKey gameInput = (-1,0)
          | isKeyDown upKey gameInput = (0,-1)
          | isKeyDown downKey gameInput = (0,1)
          | otherwise = (0,0)
        signalLocation' = (x + offsetX, y + offsetY)
        signalLocation'' = if isGridCellFree grid signalLocation'
          then signalLocation'
          else signalLocation

shouldSignalDie :: Signal -> [Enemy] -> Bool
shouldSignalDie Signal{..} = any (\Enemy{..} -> signalLocation == enemyLocation)

loseLife :: Signal -> Signal
loseLife signal@Signal{..} =
  signal { signalLives = signalLives', signalLocation = signalLocation' }
  where signalLives' = signalLives - 1
        signalLocation' = if signalLives' > 0
          then initialSignalLocation
          else signalLocation
