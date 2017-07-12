module Main where

import           Graphics.Gloss

import           Constants
import           Game
import           GameInput
import           Grid
import           Image
import           Renderable
import           Utils

main :: IO ()
main = do
  getPic <- loadGetPic
  grids <- loadGrids
  initialGame <- randomToIO $ loadInitialGame getPic grids

  let window = InWindow "Geocaching Game" (windowX, windowY) (200, 200)

  play
    window
    black   -- background color
    60      -- fps
    initialGame
    render
    handleInput
    updateGame
