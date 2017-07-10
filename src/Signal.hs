{-# LANGUAGE RecordWildCards #-}
module Signal where

import           Constants
import           GameInput
import           Grid
import           Image
import           Renderable
import           Types

loadSignal :: IO Signal
loadSignal = do
  signalPic <- loadPNG "images/signal.png"
  return Signal
    { signalLives = numLives
    , signalPic = signalPic
    , signalLocation = initialSignalLocation
    }

instance Renderable Signal where
  render Signal{..} = renderOnGrid signalLocation signalPic

updateSignal :: Signal -> GameInput -> Grid -> Signal
updateSignal signal@Signal{..} gameInput grid = signal { signalLocation = signalLocation'' }
  where (x, y) = signalLocation
        (offsetX, offsetY)
          | isKeyDown rightKey gameInput = (1,0)
          | isKeyDown leftKey gameInput = (-1,0)
          | isKeyDown upKey gameInput = (0,-1)
          | isKeyDown downKey gameInput = (0,1)
          | otherwise = (0,0)
        signalLocation' = (x + offsetX, y + offsetY)
        signalLocation'' | isGridCellFree grid signalLocation' = signalLocation'
                         | otherwise = signalLocation
