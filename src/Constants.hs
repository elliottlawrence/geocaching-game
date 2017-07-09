module Constants where

isDebug :: Bool
isDebug = False

numCachesPerLevel, numLevels, numLives :: Int
numCachesPerLevel = 3
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
