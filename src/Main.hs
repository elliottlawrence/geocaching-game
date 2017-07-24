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
  grids <- loadGrids defaultBackend
  initialGame <- randomToIO $ loadInitialGame getPic grids

  play
    defaultBackend
    initialGame
    render
    handleInput
    updateGame
