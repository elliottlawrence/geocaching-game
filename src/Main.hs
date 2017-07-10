module Main where

import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game

import           Constants
import           Game
import           GameInput
import           Grid
import           Image
import           Renderable
import           Signal
import           Types

main :: IO ()
main = do
  initialGame <- loadInitialGame
  let display = InWindow "Geocaching Game" (windowX, windowY) (200, 200)

  playIO
    display
    black   -- background color
    60      -- fps
    initialGame
    (return . render)
    handleInput
    updateGame
