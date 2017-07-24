{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
module Grid where

import           Control.Monad.State
import           Data.Array
import           Data.List
import           System.Random

import           Constants
import           Renderable
import           Types
import           Utils

instance Read Cell where
  readsPrec _ ('0':s) = [(Free,s)]
  readsPrec _ ('1':s) = [(Wall,s)]
  readsPrec _ _ = error "Invalid grid"

wallColor :: Backend a => Color a
wallColor = makeColor 96 96 96 255

gridColors :: Backend a => [Color a]
gridColors =
  [ makeColor 210 250 139 255
  , makeColor 250 169 140 255
  , makeColor 114 175 250 255
  , makeColor 250 247 115 255
  , makeColor 147 141 250 255
  , makeColor 250 208 131 255
  , makeColor 139 250 248 255
  , makeColor 250 148 197 255
  , makeColor 224 152 250 255
  , makeColor 139 255 196 255
  ]

loadGrids :: Backend a => a -> IO [Grid a]
loadGrids b = do
  gridData <- loadFile b "grids.txt"
  let cellChunks = chunkify (gridTiles ^ (2 :: Int)) (map read $ words gridData)
      grids = take numLevels $ cycle $ zipWith loadGrid cellChunks gridColors
  return grids

loadGrid :: [Cell] -> Color a -> Grid a
loadGrid cells = Grid gridArray
  where
    gridBounds = ((0, 0), (gridTiles - 1, gridTiles - 1))
    gridArray = array gridBounds
      [ ((c,r), cell) | ((r,c), cell) <- zip (range gridBounds) cells ]

chunkify :: Int -> [a] -> [[a]]
chunkify _ [] = []
chunkify i xs = first : chunkify i rest
  where (first, rest) = splitAt i xs

tileSize' :: Backend a => FloatType a
tileSize' = fromIntegral tileSize

instance Backend a => Renderable (Grid a) a where
  render grid@Grid{..} = pictures cells
    where cells = map (\((x,y), cell) ->
            renderOnGrid (x,y)
            (case cell of
              Wall -> renderWallCell (x,y) grid
              Free -> renderFreeCell grid))
            (assocs gridArray)

renderFreeCell :: Backend a => Grid a -> Picture a
renderFreeCell Grid{..} = colored gridColor $ rectangle tileSize' tileSize'

data Corner = TL | TR | BL | BR

renderWallCell :: Backend a => (Int, Int) -> Grid a -> Picture a
renderWallCell (x,y) grid@Grid{..} = pictures
  [ renderFreeCell grid
  , colored wallColor $ pictures $
    translate halfTile halfTile (circleSolid halfTile) :
    corners
  ]
  where
    halfTile = tileSize' / 2
    quarterWall = rectangle halfTile halfTile

    renderCorner TL = quarterWall
    renderCorner TR = translate halfTile 0 quarterWall
    renderCorner BL = translate 0 halfTile quarterWall
    renderCorner BR = translate halfTile halfTile quarterWall

    getAdjacentCells TL = [(x-1, y), (x, y+1)]
    getAdjacentCells TR = [(x+1, y), (x, y+1)]
    getAdjacentCells BL = [(x-1, y), (x, y-1)]
    getAdjacentCells BR = [(x, y-1), (x+1, y)]

    hasCorner corner = any (not . isGridCellFree grid) (getAdjacentCells corner)
    corners = [ renderCorner corner | corner <- [TL, TR, BL, BR], hasCorner corner ]


renderOnGrid :: Backend a => (Int, Int) -> Picture a -> Picture a
renderOnGrid (x, y) = translate x' y'
  where [x', y'] = map (fromIntegral . (* tileSize)) [x, gridTiles - 1 - y]

isGridCellFree :: Grid a -> (Int, Int)  -> Bool
isGridCellFree Grid{..} coord
  | inRange (bounds gridArray) coord = gridArray ! coord == Free
  | otherwise = False

isValidDirection :: Grid a -> (Int, Int) -> (Int, Int) -> Bool
isValidDirection grid location direction =
  isGridCellFree grid (location `addPoints` direction)

addPoints :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPoints (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

getRandomDirection :: Grid a -> (Int, Int) -> (Int, Int) -> RandomT (Int, Int)
getRandomDirection grid location (dx,dy)
  | null validDirections = return (-dx,-dy)
  | otherwise = do
    randomIndex <- state $ randomR (0, length validDirections - 1)
    return $ validDirections !! randomIndex
  where
    validDirections = filter (isValidDirection grid location) $
      [(1,0), (-1,0), (0,1), (0,-1)] \\ [(-dx,-dy)]

getRandomCoords :: RandomT [(Int, Int)]
getRandomCoords = makeRandomT $
  map (\[x,y] -> (x,y)) . chunkify 2 . randomRs (0, gridTiles - 1)

getRandomLocations :: Grid a -> Int -> [(Int, Int)] -> RandomT [(Int, Int)]
getRandomLocations grid numLocations coordsToExclude = do
  coords <- getRandomCoords
  let locs = take numLocations $ nub $ filter
        (\(x,y) -> isGridCellFree grid (x,y) && (x,y) `notElem` coordsToExclude)
        coords
  return locs
