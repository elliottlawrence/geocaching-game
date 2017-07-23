module Constants where

import           Types

isDebug :: Bool
isDebug = True

numLevels, numLives :: Int
numLevels = 10
numLives = 5

gridTiles, gridSize, gutter, tileSize, windowX, windowY :: Int
gridTiles = 25
tileSize = 20
gridSize = gridTiles * tileSize
gutter = 250
windowX = gutter + gridSize
windowY = gridSize

initialSignalLocation :: (Int, Int)
initialSignalLocation = (12,12)

signalSpeed :: Int
signalSpeed = 9

black, red :: Color
black = Color 0 0 0 255
red = Color 255 0 0 255
