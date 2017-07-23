{-# LANGUAGE CPP #-}
module Main where

#ifdef __GHCJS__

import GHCJS.DOM (currentDocumentUnchecked)
import Graphics.Shine
import Graphics.Shine.Image
import Graphics.Shine.Input
import Graphics.Shine.Picture

import Constants

main :: IO ()
main = do
  doc <- currentDocumentUnchecked
  ctx <- fixedSizeCanvas doc windowX windowY
  img <- makeImage "images/signal.png"

  let initialGame = Up
      draw Up = Image Original img
      draw Down = Translate 200 200 $ RectF 200 200
      handleInput (MouseBtn BtnLeft buttonState _) = const buttonState
      handleInput _ = id
  play
    ctx
    doc
    30
    initialGame
    draw
    handleInput
    (\_ -> id)

#else

import           Backend
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

  let window = getWindow "Geocaching Game" (windowX, windowY) (200, 200)

  play
    window
    black   -- background color
    60      -- fps
    initialGame
    render
    handleInput
    updateGame

#endif
