module Main where

import           Graphics.Gloss

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

  play
    display
    black   -- background color
    60      -- fps
    initialGame
    render
    handleInput
    updateGame
