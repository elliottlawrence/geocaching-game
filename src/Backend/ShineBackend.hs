{-# LANGUAGE TypeFamilies #-}
module Backend.ShineBackend where

import           Data.JSString                  (pack)
import           GHCJS.DOM                      (currentDocumentUnchecked)
import           GHCJS.DOM.NonElementParentNode (getElementById)
import           Graphics.Shine                 as Shine
import           Graphics.Shine.Image           as Shine
import qualified Graphics.Shine.Input           as Shine
import           Graphics.Shine.Picture         as Shine
import           JavaScript.Web.XMLHttpRequest
import           Web.KeyCode

import           Types

data ShineBackend = ShineBackend

instance Backend ShineBackend where
  type FloatType ShineBackend = Double
  type Picture ShineBackend = Shine.Picture
  type Color ShineBackend = Shine.Color
  type EventT ShineBackend = Shine.Input

  play _ initialGame renderGame handleInput updateGame = do
    doc <- currentDocumentUnchecked
    Just canvas <- getElementById doc "canvas"
    context <- toContext canvas

    Shine.play
      context
      doc
      5
      initialGame
      renderGame
      (makeHandleInput handleInput)
      updateGame'
    where
      updateGame' _ = updateGame

  loadImage filePath = do
    imageData <- makeImage filePath
    return $ Image Original imageData

  loadFile _ filePath = do
    let request = Request GET (pack filePath) Nothing [] False NoData
    response <- xhrString request
    case contents response of
      Just file -> return file
      _ -> ioError $ userError $ "File not found: " ++ filePath

  makeColor r g b a = Color r g b (fromIntegral a / 255)

  blank = Empty
  colored = Colored
  circle = CircleF
  line = Line
  pictures = foldl (<>) Empty
  rectangle = RectF
  text textSize = Text font LeftAlign Nothing
    where font = case textSize of
                  BigText -> "24px sans-serif"
                  SmallText -> "20px sans-serif"
  translate = Translate

  toEvent (Shine.Keyboard key keyState _) =
    maybe Nothing (\key' -> Just $ EventKey key' keyState') maybeKey
    where
      keyState' = case keyState of
        Shine.Up -> Up
        Shine.Down -> Down

      maybeKey = case key of
        ArrowLeft -> Just KeyLeft
        ArrowRight -> Just KeyRight
        ArrowUp -> Just KeyUp
        ArrowDown -> Just KeyDown
        Space -> Just KeySpace
        Enter -> Just KeyEnter
        Digit0 -> Just (Char '0')
        Digit1 -> Just (Char '1')
        Digit2 -> Just (Char '2')
        Digit3 -> Just (Char '3')
        Digit4 -> Just (Char '4')
        Digit5 -> Just (Char '5')
        Digit6 -> Just (Char '6')
        Digit7 -> Just (Char '7')
        Digit8 -> Just (Char '8')
        Digit9 -> Just (Char '9')
        _ -> Nothing
  toEvent _ = Nothing
