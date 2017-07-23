module Main where

import           Backend
import           Game
import           GameInput
import           Grid
import           Image
import           Renderable
import           Types
import           Utils

main :: IO ()
main = do
  getPic <- loadGetPic
  grids <- loadGrids
  initialGame <- randomToIO $ loadInitialGame getPic grids

  play
    defaultBackend
    60      -- fps
    initialGame
    render
    handleInput
    updateGame
