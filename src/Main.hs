module Main where

import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game

import           Constants
import           Game
import           GameInput
import           Renderable

main :: IO ()
main = do
  initialGame <- loadInitialGame
  let window = InWindow "Geocaching Game" (windowX, windowY) (200, 200)

  playIO
    window
    black   -- background color
    60      -- fps
    initialGame
    (return . render)
    handleInput
    updateGame
