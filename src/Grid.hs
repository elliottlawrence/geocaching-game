{-# LANGUAGE RecordWildCards #-}
module Grid where

import           Data.Array
import           Data.List
import           Graphics.Gloss
import           System.Random

import           Constants
import           Renderable
import           Types

instance Read Cell where
  readsPrec _ ('0':s) = [(Free,s)]
  readsPrec _ ('1':s) = [(Wall,s)]

wallColor :: Color
wallColor = makeColorI 96 96 96 255

levelColors :: [Color]
levelColors = cycle
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
      gridArrays = map (listArray ((0, 0), (gridTiles - 1, gridTiles - 1))) cellChunks
      grids = take numLevels $ cycle $ zipWith Grid gridArrays levelColors
  return grids

chunkify :: Int -> [a] -> [[a]]
chunkify i [] = []
chunkify i xs = first : chunkify i rest
  where (first, rest) = splitAt i xs

getRandomLocations :: Grid -> IO [(Int, Int)]
getRandomLocations grid = do
  g <- newStdGen
  let coords = map (\[x,y] -> (x,y)) $ chunkify 2 $ randomRs (0, gridTiles - 1) g
      locs = take numCachesPerLevel $ nub $ filter
        (\(x,y) -> isGridCellFree grid (x,y) && (x,y) /= initialSignalLocation)
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

rectangle :: Int -> Int -> Picture
rectangle w h = Polygon [(0, 0), (w', 0), (w', h'), (0, h')]
  where [w', h'] = map fromIntegral [w, h]

renderOnGrid :: (Int, Int) -> Picture ->  Picture
renderOnGrid (x, y) = Translate x' y'
  where [x', y'] = map (fromIntegral . (* tileSize)) [x, y]

isGridCellFree :: Grid -> (Int, Int)  -> Bool
isGridCellFree Grid{..} coord
  | inRange (bounds gridArray) coord = gridArray ! coord == Free
  | otherwise = False
