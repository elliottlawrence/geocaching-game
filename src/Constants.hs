module Constants where

import           Types

isDebug :: Bool
isDebug = True

numLevels, numLives :: Int
numLevels = 10
numLives = 5

gridTiles, gridSize, gutter, tileSize, windowX, windowY :: Num a => a
gridTiles = 25
gridSize = gridTiles * tileSize
gutter = 250
tileSize = 20
windowX = gutter + gridSize
windowY = gridSize

initialSignalLocation :: (Int, Int)
initialSignalLocation = (12,12)

signalSpeed :: Int
signalSpeed = 9

black, red :: Backend a => Color a
black = makeColor 0 0 0 255
red = makeColor 255 0 0 255
