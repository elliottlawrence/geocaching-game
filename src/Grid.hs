{-# LANGUAGE RecordWildCards #-}
module Grid where

import           Data.Array
import           Data.List
import           Graphics.Gloss
import           System.Random

import           Constants
import           Renderable
import           Types
import           Utils

instance Read Cell where
  readsPrec _ ('0':s) = [(Free,s)]
  readsPrec _ ('1':s) = [(Wall,s)]

wallColor :: Color
wallColor = makeColorI 96 96 96 255

gridColors :: [Color]
gridColors =
  [ makeColorI 161 212 131 255
  , makeColorI 117 167 218 255
  , makeColorI 243 189 146 255
  , makeColorI 247 246 144 255
  , makeColorI 197 113 241 255
  ]

loadGrids :: IO [Grid]
loadGrids = do
  gridData <- readFile "grids.txt"
  let cellChunks = chunkify (gridTiles ^ 2) (map read $ words gridData)
      grids = take numLevels $ cycle $ zipWith loadGrid cellChunks gridColors
  return grids

loadGrid :: [Cell] -> Color -> Grid
loadGrid cells = Grid gridArray
  where
    gridBounds = ((0, 0), (gridTiles - 1, gridTiles - 1))
    gridArray = array gridBounds
      [ ((c,r), cell) | ((r,c), cell) <- zip (range gridBounds) cells ]

chunkify :: Int -> [a] -> [[a]]
chunkify i [] = []
chunkify i xs = first : chunkify i rest
  where (first, rest) = splitAt i xs

getRandomCoords :: IO [(Int, Int)]
getRandomCoords = do
  g <- newStdGen
  let coords = map (\[x,y] -> (x,y)) $ chunkify 2 $ randomRs (0, gridTiles - 1) g
  return coords

getRandomLocations :: Grid -> Int -> [(Int, Int)] -> IO [(Int, Int)]
getRandomLocations grid numCaches coordsToExclude = do
  coords <- getRandomCoords
  let locs = take numCaches $ nub $ filter
        (\(x,y) -> isGridCellFree grid (x,y) && (x,y) `notElem` coordsToExclude)
        coords
  return locs

instance Renderable Grid where
  render Grid{..} = Pictures cells
    where cells = map (\((x,y), cell) ->
            renderOnGrid (x,y) $
            (case cell of
              Wall -> Color wallColor
              Free -> Color gridColor) $
            rectangle tileSize tileSize)
            (assocs gridArray)

renderOnGrid :: (Int, Int) -> Picture ->  Picture
renderOnGrid (x, y) = Translate x' y'
  where [x', y'] = map (fromIntegral . (* tileSize)) [x, gridTiles - 1 - y]

isGridCellFree :: Grid -> (Int, Int)  -> Bool
isGridCellFree Grid{..} coord
  | inRange (bounds gridArray) coord = gridArray ! coord == Free
  | otherwise = False
